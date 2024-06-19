(ns tesserae.eval
  (:require
    [tesserae.db :as db]
    [clojure.string :as str]
    [tesserae.eval.schedule :as eval.sched]
    [edamame.core :as mame]
    [org.httpkit.client]
    [sci.core :as sci]
    [tesserae.ui.render :as uir]
    [datalevin.core :as d]
    [missionary.core :as m]
    [stuffs.datalevin.util :as sdu]
    [medley.core :as md]
    [tesserae.autoformat :as af]
    [cljfmt.core :as cfmt]
    [stuffs.util :as su]
    [net.cgrand.xforms :as x]
    [sci.impl.utils :as sci-utils]
    [tesserae.ui.sheet :as-alias ui.sheet]
    [mount.core :as mount :refer [defstate]]
    [tesserae.eval.vars :as eval.vars]
    [tesserae.push-notif :as push-notif]
    [tick.core :as t])
  (:import (missionary Cancelled)))

(defonce ^:dynamic *sheet-id* nil)
(defonce ^:dynamic *cell* nil)
(defonce
  ^:dynamic
  ^{:doc "dynamic var to capture referenced entities in query-functions - used to make cells reactive"}
  *ref-ids*
  nil)

(defn move [dir x y width height]
  (letfn [(cyc [v max] (if (zero? v) max v))
          (modc [cv op max] (cyc (mod (op cv) max) max))]
    (case dir
      :right [(modc x inc width) y]
      :left [(modc x dec width) y]
      :up [x (modc y dec height)]
      :down [x (modc y inc height)])))

(defn neighbor-pos [dir cell]
  (let [{:cell/keys                            [x y]
         {:sheet/keys [cols-count rows-count]} :sheet/_cells} cell]
    (move dir x y cols-count rows-count)))

(defn add-*ref-ids*! [{:as ent :keys [db/id]}]
  (when (and id *ref-ids*)
    (swap! *ref-ids* conj (:db/id ent)))
  ent)

(defn neighbor
  ([dir]
   (neighbor dir *cell*))
  ([dir cell]
   (let [{:cell/keys [x y]} cell
         sheet-id (-> cell :sheet/_cells :db/id)
         {:sheet/keys [cols-count rows-count] :as sheet} (db/entity sheet-id)
         [nx ny :as pos] (move dir x y cols-count rows-count)]
     (or
      (add-*ref-ids*!
       (db/where-entity
        [[sheet-id :sheet/cells '?e]
         ['?e :cell/pos [nx ny]]]))
      {:sheet/_cells sheet
       :cell/x       nx
       :cell/y       ny}))))

(defn sheet-fns [conn]
  (letfn [(cell-by-name
            ([cell-name]
             (cell-by-name @conn cell-name))
            ([db cell-name]
             (assert *sheet-id* "*sheet-id* cannot be nil")
             (add-*ref-ids*!
               (sdu/where-entity
                 db
                 [[*sheet-id* :sheet/cells '?e]
                  ['?e :cell/name cell-name]]))))
          (cell-by-coord
            ([x y]
             (cell-by-coord @conn x y))
            ([db x y]
             (assert *sheet-id* "*sheet-id* cannot be nil")
             (assert *cell* "*cell* cannot be nil")
             (let [{cx :cell/x cy :cell/y} *cell*
                   x (if (integer? x) x cx)
                   y (if (integer? y) y cy)]
               (add-*ref-ids*!
                 (sdu/where-entity
                   db
                   [[*sheet-id* :sheet/cells '?e]
                    ['?e :cell/pos [x y]]])))))
          (set-code [ent-or-cell-name code]
            (when-let [ent (cond
                             (or (sdu/entity? ent-or-cell-name)
                                 (map? ent-or-cell-name)) ent-or-cell-name
                             (string? ent-or-cell-name) (cell-by-name ent-or-cell-name)
                             :else nil)]
              (d/transact! conn [(-> ent
                                     (assoc :cell/form-str
                                            (cond-> code
                                              (not (string? code)) pr-str))
                                     (update :sheet/_cells :db/id))])
              true))]
    (let [$name     (comp uir/value :cell/ret cell-by-name)
          $code-str (comp :cell/form-str cell-by-name)]
      {'$                  $name
       '$code-str          $code-str
       '$coord             (su/some-comp uir/value :cell/ret cell-by-coord)
       ;; fixme improve naming
       '$neighbor-ent      neighbor
       '$neighbor          (su/some-comp uir/value :cell/ret neighbor)
       '$neighbor-code-str (su/some-comp :cell/form-str neighbor)
       '$set-neighbor-code (fn [dir code]
                             (some-> (neighbor dir) (set-code code)))
       '$set-code          set-code})))

(defn dyn-sci-ns [namespace]
  (sci/create-ns namespace nil))

(def mem-dyn-sci-ns (memoize dyn-sci-ns))

(defn sci-copy-clj-var [var]
  (let [ns     (namespace (symbol var))
        sci-ns (mem-dyn-sci-ns ns)]
    (sci/copy-var* var sci-ns)))

(defn namespaces [alias-sym->ns-sym-or-resolved-ns-map]
  (update-vals
    alias-sym->ns-sym-or-resolved-ns-map
    (fn [ns-sym-or-resolved-ns-map]
      (if (symbol? ns-sym-or-resolved-ns-map)
        (do
          (require ns-sym-or-resolved-ns-map)
          (-> (ns-publics ns-sym-or-resolved-ns-map)
              (update-vals #_deref sci-copy-clj-var)))
        ns-sym-or-resolved-ns-map))))

(defn bindings [sym->qualified-sym-or-var]
  (update-vals
    sym->qualified-sym-or-var
    (fn [qualified-sym-or-var]
      (cond-> qualified-sym-or-var
        (qualified-symbol? qualified-sym-or-var) requiring-resolve))))

(declare eval-form eval-form-or-str sci-ctx mame-parse)

(defn eval-form-or-str
  ([x]
   (cond->> x
     (string? x) mame-parse
     true eval-form)))

(def sci-*cell* (sci/new-dynamic-var '*cell*))
(def sci-*cb-str* (sci/new-dynamic-var '*cb-str*))

(defstate sci-ctx
  :start
  (let [fns  (sheet-fns db/conn)
        nss  (merge
               {'http (do
                        (require 'org.httpkit.client)
                        (reduce-kv
                          (fn [out k v]
                            (let [f (deref v)]
                              (assoc
                                out
                                k
                                (if (contains? #{'request 'get 'post 'patch 'put} k)
                                  (comp deref f)
                                  f))))
                          {}
                          (ns-publics 'org.httpkit.client)))}
               (namespaces {'db     'tesserae.db
                            'su     'stuffs.util
                            'x      'net.cgrand.xforms
                            'str    'clojure.string
                            'render 'tesserae.ui.render
                            't      'tick.core
                            ;; doesn't work with sci yet
                            #_#_'m 'missionary.core})
               (some-> (mount/args) ::namespaces namespaces))
        bdgs (merge
               fns
               {'help (constantly
                        {:namespaces
                         (update-vals nss keys)})}
               (bindings {'slurp    slurp
                          'eval     eval-form-or-str
                          'sleep    (fn [ms] (Thread/sleep ms))
                          'println  println
                          'tap>     tap>
                          '*cell*   sci-*cell*
                          '*cb-str* sci-*cb-str*})
               (some-> (mount/args) ::bindings bindings))]
    (sci/init
      {:bindings   bdgs
       :namespaces nss})))

(comment
  (-> sci-ctx :env deref :namespaces keys)
  (-> sci-ctx :env deref :namespaces 'su)
  (-> sci-ctx :env deref :namespaces (get 'user) keys)
  )

;; unused
#_(defonce current-ns
    (atom (sci-utils/namespace-object
            (:env sci-ctx)
            'user true nil)))

(comment
  '(do
     (let [rewritten (openai/chat-1 {:message
                                     (str "Make this clojure code more interesting: "
                                          ($neighbor-code :up))})])

     ($set-neighbor-code
       :right
       rewritten)
     :done))

(defn make-ns [ns-str]
  (sci-utils/namespace-object
    (:env sci-ctx)
    ns-str true nil))

#_(defn set-ns [ns-str]
    (reset! current-ns (make-ns ns-str)))

(defn eval-form
  ([form] (eval-form sci-ctx form))
  ([ctx form]
   (sci/with-bindings
     {sci/out      *out*
      sci/in       *in*
      ;; binding from dynamic clojure land into dynamic sci land
      sci-*cell*   *cell*
      sci-*cb-str* eval.vars/*cb-str*
      ; sci/ns  @current-ns
      }
     (sci/eval-form ctx form))))

(comment
  (eval-form '(harvest/send-budget-spent-slack-notifs))
  (eval-form '(slurp "deps.edn"))
  (eval-form '(help)))


(defn exec-form [form]
  {:form     form
   :ret      (eval-form form)
   :ent-name "foo"
   :cell-x   :A
   :cell-y   1
   ;:cell-coord [:A 1]
   })


(defn- ex->data
  [ex phase]
  (assoc (Throwable->map ex) :phase phase))

(defn ex-cancelled? [e]
  (let [interrupted? (contains?
                       #{java.io.InterruptedIOException
                         java.lang.InterruptedException
                         java.util.concurrent.RejectedExecutionException
                         java.nio.channels.ClosedByInterruptException}
                       (type e))]
    interrupted?))

(defn eval-form-map [{:as m :keys [form]}]
  (try
    (-> m
        (assoc :ret (eval-form #_spy/p form)
               ;; assoc nil to clear possible
               ;; exception from previous eval
               :exception nil))
    (catch Throwable ex
      (if (ex-cancelled? ex)
        (throw (Cancelled. (.getMessage ex)))
        (assoc m
          :exception true
          :ret (ex->data ex :execution))))))

(defn- mame-parse [s]
  (mame/parse-string (str "(do " s ")")
                     {:syntax-quote true
                      :quote        true
                      :deref        true
                      :regex        true
                      :fn           true}))

(defn eval-form-str-map [{:as m :keys [form-str]}]
  (if-not form-str
    m
    (try
      (let [form (mame-parse form-str)]
        (-> (assoc m :form form)
            eval-form-map))
      (catch Throwable ex
        (if (some-> ex-data :type (= :edamame/error))
          (assoc m
            :exception true
            :ret (ex->data ex :read-source))
          (throw ex))))))

#_(eval-form-str-map {:cell/form-str "(+ 1 2)"})


(defn capture-refs [eval-thunk->map]
  (binding [*ref-ids* (atom #{})]
    (let [ret (eval-thunk->map)]
      (md/assoc-some ret :cell/refs (some-> *ref-ids* deref not-empty vec)))))

(defn fmt-clj-str [s]
  (cond-> s
    (af/likely-edn? s) cfmt/reformat-string))

(defn fmt-cell-formstr [{:as cell-map :keys [cell/form-str]}]
  (try (-> cell-map
           (dissoc :cell/exception?)
           (update :cell/form-str fmt-clj-str))
       (catch Throwable t
         (let [ret (assoc (Throwable->map t) :phase :read-source)]
           (md/assoc-some
             cell-map
             :cell/exception? true
             :cell/ret ret
             :cell/ret-str (if ret (pr-str ret) ""))))))

(defn eval-cell [{:as cell :cell/keys [form-str]}]
  ; (println :RUNNING-EVAL cell)
  (let [cell' (dissoc cell :cell/ret :cell/refs :cell/ret-str :cell/ret-pending? :cell/exception?)]
    (if (str/blank? form-str)
      (dissoc cell' :cell/form-str)
      (let [{:as m :keys [ret exception cell/refs]}
            (binding [*cell*     cell
                      *sheet-id* (-> cell' :sheet/_cells :db/id)]
              (capture-refs #(eval-form-str-map {:form-str (af/edn-ize form-str)})))]
        #_(println :RUNNING-EVAL m)
        ; (tesserae.ui.sheet/setxx [:cell/ret ret :cell/refs refs])
        (md/assoc-some
          cell'
          :cell/ret ret
          :cell/refs refs
          :cell/ret-str (if ret (pr-str ret) "")
          :cell/evaled-at (t/inst (t/now))
          :cell/exception? (boolean exception))))))

(defn empty-unnamed-refless-cell? [{id :db/id :as cell :cell/keys [name form-str _refs]}]
  (boolean (and id (str/blank? form-str) (str/blank? name) (empty? _refs))))

(defn retract-empty-unnamed-refless-cell!
  ([tx-fn cell]
   (when (empty-unnamed-refless-cell? cell)
     (tx-fn [[:db/retractEntity (:db/id cell)]]))))

(defn fmt-eval-cell [{:as cell :cell/keys [form-str]}]
  (if (str/blank? form-str)
    (dissoc cell :cell/form-str :cell/ret :cell/ret-str :cell/ret-pending?)
    (let [{:as fmt-cell :cell/keys [exception?]} (fmt-cell-formstr cell)]
      (if exception?
        fmt-cell
        (eval-cell fmt-cell)))))

#_(fmt-eval-cell @tesserae.ui.sheet/xx)
#_(let [{:syms [$]} (sheet-fns db/conn)]
    (binding [*sheet-id* nil
              *ref-ids*  (atom #{})]
      ($ "year")))

(defn transact-cell-fn! [transact! tx-meta]
  (fn run-transact!
    [{:as   cell-ent
      :keys [db/id
             cell/form-str
             cell/ret
             cell/exception
             sheet/_cells]}]
    #_(prn :cellent cell-ent)
    (let [id-txs (if id
                   [[:db/retract id :cell/refs]
                    [:db/retract id :cell/ret]
                    [:db/retract id :cell/ret-str]
                    [:db/retract id :cell/ret-pending?]
                    [:db/retract id :cell/exception?]]
                   [])]
      (try
        (transact! (conj id-txs cell-ent) tx-meta)
        ;; catch serialization error
        (catch Throwable t
          (transact! (conj id-txs (dissoc cell-ent :cell/ret)) tx-meta))))))

(def default-timeout 2e4)

(defn eval-cell-task [{:keys [cell eval-fn timeout]
                       :or   {timeout default-timeout}}]
  {:pre [cell eval-fn]}
  (m/sp
    (let [evaled-cell (m/?
                        (m/timeout
                          (m/via m/blk (eval-fn cell))
                          timeout
                          ::timeout))]
      (if (= evaled-cell ::timeout)
        (-> cell
            (dissoc :cell/ret-pending?)
            (assoc :cell/exception? true
                   :cell/ret evaled-cell
                   :cell/ret-str (pr-str evaled-cell)))
        evaled-cell))))

(defn parallel-cells-eval-task
  "Evals cells in parallel"
  [{:keys [cells eval-fn timeout transact-cell!]
    :or   {timeout default-timeout}}]
  (let [flow (m/ap
               ;; fork process for every cell
               ;; to exec in **parallel**
               (let [cell        (m/?> ##Inf (m/seed cells))
                     evaled-cell (m/? (m/timeout (m/via m/blk (eval-fn cell))
                                                 timeout
                                                 ::timeout))]
                 (if (= evaled-cell ::timeout)
                   (-> cell
                       (dissoc :cell/ret-pending?)
                       (assoc :cell/exception? true
                              :cell/ret evaled-cell
                              :cell/ret-str (pr-str evaled-cell)))
                   evaled-cell)))
        task (m/reduce (fn [_ evaled-cell]
                         #_(println :transacting evaled-cell)
                         (transact-cell! evaled-cell))
                       nil
                       flow)]
    task))

(comment
  (def sheet (db/entity 24))
  (mapcat :sheet/cells (db/datoms->entities :ave :sheet/cells))
  (m/? (parallel-cells-eval-task {:cells          (mapcat :sheet/cells (distinct (db/datoms->entities :ave :sheet/cells)))
                                  :eval-fn        eval-cell
                                  :transact-cell! #(db/transact! [%])})))


(defn db-observe-flow [conn listen-k]
  (m/observe
    (fn [!]
      (d/listen! conn listen-k !)
      #(d/unlisten! conn listen-k))))

(defonce debug-scheduled
  (atom {}))

(defn merge-flows
  "Returns a flow that merges provided flows"
  [a b]
  (m/ap (m/?> (m/amb= a b))))

(defstate eval-schedule-listener
  :start
  (let [_               (swap! debug-scheduled empty)
        transact-cell!  (fn [c] (db/transact! [c] {:transacted-by ::schedule-listener}))
        observed-scheds (->> (db-observe-flow db/conn ::schedule-listener)
                             (m/eduction
                               (mapcat (fn [{:as m :keys [db-after tx-data]}]
                                         (let [m (dissoc m :tx-data)]
                                           (map #(assoc m :datom %) tx-data))))
                               (filter (fn [{{:keys [a added]} :datom}]
                                         (and added (= a :schedule/next))))
                               (map (fn [{{:keys [e]} :datom
                                          :keys       [db-after]}]
                                      (d/entity db-after e)))))
        >scheds         (->> (merge-flows (m/seed (db/datoms->entities :ave :schedule/from))
                                          observed-scheds)
                             (m/eduction
                               ;; ignore orphan schedules
                               (filter :cell/_schedule)
                               (map eval.sched/add-next-time)))
        eval-by-id      (m/ap
                          ;; group schedules by cell-ids
                          (let [[cell-id >scheds] (m/?> ##Inf (m/group-by
                                                                (comp :db/id :cell/_schedule)
                                                                >scheds))
                                ;; cancel previous schedule when a new one appears
                                {cell :cell/_schedule snext :schedule/next :as sched} (m/?< >scheds)
                                wait (t/millis (t/between (t/now) snext))]
                            (swap! debug-scheduled assoc cell-id {:wait wait
                                                                  :now  (t/now)})
                            (try
                              #_(println :sleep wait cell-id (:db/id sched) (:db/updated-at sched))
                              (m/? (m/sleep wait))
                              ;(println :done-sleep)
                              (transact-cell! (assoc cell :cell/ret-pending? true))

                              [(eval.sched/add-next-time sched)
                               (m/?
                                 (eval-cell-task {:cell    cell
                                                  :eval-fn eval-cell}))]
                              (catch missionary.Cancelled c
                                (println ::schedule-listener-cancelled-eval-cell cell-id)
                                (transact-cell! (assoc cell :cell/ret-pending? false))
                                (m/amb)))))
        task            (m/reduce
                          (fn [out sched+cell]
                            ;(def s (first sched+cell))
                            ;(println :Fi (first sched+cell))
                            #_(let [[sched cell] sched+cell]
                                (println ::sched-evaled (:db/id (second sched+cell))
                                         (:db/id sched) (:db/updated-at sched)))
                            (d/transact! db/conn sched+cell {:transacted-by ::schedule-listener}))
                          []
                          eval-by-id)
        cancel          (task #(println :success %)
                              #(println :fail %))]
    cancel)
  :stop (eval-schedule-listener))

(def tx-report->new-ret-cell-entity-xf
  (comp
    (filter (fn [{:keys [tx-data db-after tx-meta] :as report}]
              (not-empty tx-data)))
    (mapcat (fn [{:as m :keys [db-after tx-data]}]
              (let [m (dissoc m :tx-data)]
                (map #(assoc m :datom %) tx-data))))
    (filter (fn [{{:keys [a added]} :datom}]
              (and added (or
                           (= a :cell/ret)
                           (= a :cell/ret-str)))))
    (map (fn [{{:keys [e]} :datom
               :keys       [db-after]}]
           (d/entity db-after e)))
    (distinct)))

(defstate cell-notifs-listener
  :start (let [transact-one! (fn [tx]
                               (d/transact!
                                 db/conn
                                 [tx]
                                 {:transacted-by ::notifs-listener}))
               >cells        (->> (db-observe-flow db/conn ::notifs-listener)
                                  (m/eduction
                                    (filter (fn [{:keys [tx-data db-after tx-meta] :as report}]
                                              (and (not-empty tx-data)
                                                   ;; listen only to scheduled executions
                                                   (contains?
                                                     #{::schedule-listener}
                                                     (:transacted-by tx-meta)))))
                                    tx-report->new-ret-cell-entity-xf))
               <notify       (m/ap
                               (let [cell (m/?> ##Inf >cells)]
                                 ;(tap> cell)
                                 (when-let [notify-users (:cell/notify-on-ret cell)]
                                   (let [sub  (->> (m/seed notify-users)
                                                   (m/eduction (mapcat :user/web-push-subs))
                                                   (m/?> ##Inf))
                                         #_(tap> sub)
                                         resp (m/?
                                                (m/via m/blk
                                                       #_(println :SENDING-NOTIF)
                                                       (push-notif/send!
                                                         sub
                                                         (push-notif/cell->notif-m cell))))]
                                     (if (tesserae.push-notif/sub-gone? resp)
                                       (transact-one! [:db/retractEntity (:db/id sub)])
                                       [:notified sub])))))
               task          (m/reduce (fn [_ x]
                                         #_(when x (println :x x)))
                                       nil
                                       <notify)
               cancel        (task #(println :success %)
                                   #(println :fail %))]
           cancel)
  :stop (cell-notifs-listener))



(comment
  (mount/stop #'eval-schedule-listener)

  (db/transact!
    (map (fn [sched-ent]
           (assoc (tesserae.eval.schedule/parse->schedule (:schedule/text sched-ent) {:zone "America/New_York"})
             :db/id (:db/id sched-ent)))
         (db/datoms->entities :ave :schedule/text))))

(comment
  (db/transact! (map (fn [x] [:db/retractEntity (:db/id x)]) (remove :cell/_schedule (db/datoms->entities :ave :schedule/text)))))

(defstate db-eval-refs-listener
  :start (let [transact-cell! (fn [c]
                                (d/transact!
                                  db/conn
                                  [c]
                                  {:transacted-by ::refs-listener}))
               >cells         (->> (db-observe-flow db/conn ::refs-listener)
                                   (m/eduction
                                     tx-report->new-ret-cell-entity-xf
                                     (mapcat :cell/_refs)))
               eval-by-id     (m/ap
                                (let [cell (m/?> ##Inf >cells)]
                                  (println ::refs-listener :db/id (:db/id cell))
                                  (transact-cell! (assoc cell :cell/ret-pending? true))
                                  (try
                                    #_(println :db/id (:db/id cell))
                                    (m/?
                                      (eval-cell-task {:cell    cell
                                                       :eval-fn eval-cell}))
                                    (catch missionary.Cancelled c
                                      (println ::refs-listener-cancelled-eval-cell (:db/id cell))
                                      (m/amb)))))
               task           (m/reduce
                                (fn [out v]
                                  #_(tap> v)
                                  (println ::refs-listener-evaled (:db/id v))
                                  (transact-cell! v))
                                []
                                eval-by-id)
               cancel         (task #(println :success %)
                                    #(println :fail %))]

           cancel)
  :stop (db-eval-refs-listener))


(defstate db-eval-form-str-listener
  :start (let [transact-cell! (transact-cell-fn! db/transact! {:transacted-by ::form-str-listener})
               >cells         (->> (db-observe-flow db/conn ::form-str-listener)
                                   (m/eduction
                                     (filter (fn [{:keys [tx-data db-after tx-meta] :as report}]
                                               ;; todo could also add ignore for  meta here
                                               (and (not-empty tx-data)
                                                    (not (contains?
                                                           #{::form-str-listener ::ui.sheet/cell}
                                                           (:transacted-by tx-meta))))))
                                     (mapcat (fn [{:as m :keys [db-after tx-data]}]
                                               (let [m (dissoc m :tx-data)]
                                                 (map #(assoc m :datom %) tx-data))))
                                     (filter (fn [{{:keys [a added]} :datom}]
                                               (and added (= a :cell/form-str))))
                                     (map (fn [{{:keys [e]} :datom
                                                :keys       [db-after]}]
                                            (d/entity db-after e)))))
               eval-by-id     (m/ap
                                (let [[id >same-id-cells] (m/?> ##Inf (m/group-by :db/id >cells))
                                      cell (m/?< >same-id-cells)]
                                  (d/transact!
                                    db/conn
                                    [(assoc cell :cell/ret-pending? true)]
                                    {:transacted-by ::form-str-listener})

                                  (try
                                    (m/?
                                      (eval-cell-task {:cell    cell
                                                       :eval-fn fmt-eval-cell}))
                                    (catch missionary.Cancelled c
                                      (println ::form-str-listener-cancelled-eval-cell id)
                                      (m/amb)))))
               task           (m/reduce
                                (fn [out v]
                                  (println ::form-str-listener-evaled (:db/id v))
                                  #_(def ret v)
                                  (transact-cell! v))
                                []
                                eval-by-id)
               cancel         (task #(println :success %)
                                    #(println :fail %))]

           cancel)
  :stop (db-eval-form-str-listener))

(comment
  (db/transact! [(dissoc (db/entity 18) :cell/ret :cell/form-str :cell/ret-str)])
  (transact-cell-fn!
    (eval-cell (assoc (db/entity 18)
                 :cell/form-str "2012")))
  (:cell/refs (db/entity 6))
  (transact-cell-fn!
    (eval-cell (assoc (db/entity 6)
                 :cell/form-str "(db/cell-ret \"year\")")))
  (:cell/refs (db/entity 2))
  ;(d/touch (db/entity 2))
  (mount/stop #'tesserae.serve/server)
  (mount/start #'tesserae.serve/server)
  (db/transact! [])
  (mount/start #'db/conn)
  (mount/stop #'db/conn)
  (mount/start)
  (mount/stop)
  (mount/start #'db-eval-form-str-listener)
  (mount/start #'db-eval-refs-listener)
  (mount/start #'eval-schedule-listener)
  (mount/stop #'eval-schedule-listener)
  (mount/running-states)
  ;(mount/start #'db/conn)
  ;(mount/running-states)
  (mount/start-without #'eval-schedule-listener)
  (mount/stop #'db-eval-form-str-listener #'db-eval-refs-listener)
  (mount/stop #'eval-schedule-listener #'db-eval-refs-listener #'db-eval-form-str-listener)
  (mount/stop #'db-eval-form-str-listener #'db-eval-refs-listener
              #'eval-schedule-listener)
  #_(db/transact! [[:db/retract 6 :cell/refs]
                   [:db/add 6 :cell/refs 18]])

  ;(:sheet/_cells (db/entity 6))
  (-> (db/entity 6) :cell/refs first :cell/ret-str))

(comment
  (d/listen! db/conn ::debug-listener
             (fn [{:keys [tx-data tx-meta]}]
               (println :debug-listener)
               (clojure.pprint/pprint tx-meta)
               (clojure.pprint/pprint (mapv (juxt :e :a :v :added) tx-data))))
  (d/unlisten! db/conn ::debug-listener))


(defn remove-empties []
  (db/transact!
    (sequence
      (comp (filter (comp str/blank? :v))
            (map :e)
            (map (partial vector :db/retractEntity)))
      (db/datoms :ave :cell/form-str))))

#_(eval-cell (update (db/entity 7) :cell/form-str str/replace "=nr" "nr="))

;(transact-cell! (eval-cell (db/entity 17)))

;; could make transact and fmt a tx-listen

(defn rename-in-form-strs-txs [match replacement]
  (sequence
    (comp
      (filter (fn [{:keys [v]}]
                (str/includes? v match)))
      (map (fn [{:keys [e v]}]
             [:db/add e :cell/form-str (str/replace v match replacement)])))
    (db/datoms :ave :cell/form-str)))

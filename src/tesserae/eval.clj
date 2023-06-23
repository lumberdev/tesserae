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
    [tick.core :as t])
  (:import (missionary Cancelled)))

(defonce ^:dynamic *sheet-id* nil)
(defonce ^:dynamic *cell* nil)
(defonce
  ^:dynamic
  ^{:doc "dynamic var to capture referenced entities in query-functions - used to make cells reactive"}
  *ref-ids*
  nil)


(defn sheet-fns [conn]
  (letfn [(cell-by-name
            ([cell-name]
             (cell-by-name @conn cell-name))
            ([db cell-name]
             (assert *sheet-id* "*sheet-id* cannot be nil")
             (when-let [ent (sdu/where-entity
                              db
                              [[*sheet-id* :sheet/cells '?e]
                               ['?e :cell/name cell-name]])]
               (swap! *ref-ids* conj (:db/id ent))
               ent)))]
    (let [$name (comp uir/value :cell/ret cell-by-name)]
      {'$ $name})))

(defn namespaces [alias-sym->ns-sym-or-resolved-ns-map]
  (update-vals
    alias-sym->ns-sym-or-resolved-ns-map
    (fn [ns-sym-or-resolved-ns-map]
      (if (symbol? ns-sym-or-resolved-ns-map)
        (do
          (require ns-sym-or-resolved-ns-map)
          (-> (ns-publics ns-sym-or-resolved-ns-map)
              (update-vals deref)))
        ns-sym-or-resolved-ns-map))))

(defn bindings [sym->qualified-sym-or-var]
  (update-vals
    sym->qualified-sym-or-var
    (fn [qualified-sym-or-var]
      (cond-> qualified-sym-or-var
        (qualified-symbol? qualified-sym-or-var) requiring-resolve))))

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
                            't      'tick.core})
               (some-> (mount/args) ::namespaces namespaces))
        bdgs (merge
               fns
               {'help (constantly
                        {:namespaces
                         (update-vals nss keys)})}
               (bindings {slurp    `slurp
                          'sleep   (fn [ms] (Thread/sleep ms))
                          'println println
                          'tap>    tap>})
               (some-> (mount/args) ::bindings bindings))]
    (sci/init
      {:bindings   bdgs
       :namespaces nss})))

;; unused
#_(defonce current-ns
    (atom (sci-utils/namespace-object
            (:env sci-ctx)
            'user true nil)))

(defn make-ns [ns-str]
  (sci-utils/namespace-object
    (:env sci-ctx)
    ns-str true nil))

#_(defn set-ns [ns-str]
    (reset! current-ns (make-ns ns-str)))

(defn eval-form
  ([form] (eval-form sci-ctx form))
  ([ctx form]
   (sci/with-bindings {sci/out *out*
                       sci/in  *in*
                       ; sci/ns  @current-ns
                       }
                      (sci/eval-form ctx form))))


(defn exec-form [form]
  {:form     form
   :ret      (eval-form form)
   :ent-name "foo"
   :cell-x   :A
   :cell-y   1
   ;:cell-coord [:A 1]
   }
  )


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
                     {:quote true
                      :deref true
                      :fn    true}))

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
      (let [{:keys [ret exception cell/refs]}
            (binding [*cell*     cell
                      *sheet-id* (-> cell' :sheet/_cells :db/id)]
              (capture-refs #(eval-form-str-map {:form-str (af/edn-ize form-str)})))]
        ; (println :RUNNING-EVAL)
        ; (tesserae.ui.sheet/setxx [:cell/ret ret :cell/refs refs])
        (md/assoc-some
          cell'
          :cell/ret ret
          :cell/refs refs
          :cell/ret-str (if ret (pr-str ret) "")
          :cell/evaled-at (t/inst (t/now))
          :cell/exception? (boolean exception))))))

(defn fmt-eval-cell [{:as cell :cell/keys [form-str]}]
  (if (str/blank? form-str)
    (dissoc cell :cell/form-str)
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
          (transact! (conj id-txs (dissoc cell-ent :cell/ret)) tx-meta)
          )))))

(def default-timeout 2e4)
;(:sheet/_cells (db/entity 20))
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
                         (transact-cell! evaled-cell)
                         )
                       nil
                       flow)]
    task))

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
                                _    (println :new cell-id)
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
                                (m/amb))
                              )
                            ))
        task            (m/reduce
                          (fn [out sched+cell]
                            ;(def s (first sched+cell))
                            ;(println :Fi (first sched+cell))
                            #_(let [[sched cell] sched+cell]
                              (println ::sched-evaled (:db/id (second sched+cell))
                                       (:db/id sched) (:db/updated-at sched)))
                            (d/transact! db/conn sched+cell {:transacted-by ::schedule-listener})
                            )
                          []
                          eval-by-id)
        cancel          (task #(println :success %)
                              #(println :fail %))]
    cancel)
  :stop (eval-schedule-listener))

(comment
  (db/transact!
    (map (fn [sched-ent]
           (assoc (tesserae.eval.schedule/parse->schedule (:schedule/text sched-ent) {:zone "America/New_York"})
             :db/id (:db/id sched-ent))
           )
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
                                     (distinct)
                                     (mapcat :cell/_refs)))
               eval-by-id     (m/ap
                                (let [cell (m/?> ##Inf >cells)]
                                  (transact-cell! (assoc cell :cell/ret-pending? true))
                                  (try
                                    #_(println :db/id (:db/id cell))
                                    (m/?
                                      (eval-cell-task {:cell    cell
                                                       :eval-fn eval-cell}))
                                    (catch missionary.Cancelled c
                                      (println ::refs-listener-cancelled-eval-cell (:db/id cell))
                                      (m/amb))
                                    )))
               task           (m/reduce
                                (fn [out v]
                                  #_(println ::refs-listener-evaled (:db/id v))
                                  (transact-cell! v)
                                  )
                                []
                                eval-by-id)
               cancel         (task #(println :success %)
                                    #(println :fail %))]

           cancel
           )
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
                                      (m/amb))
                                    )))
               task           (m/reduce
                                (fn [out v]
                                  (println ::form-str-listener-evaled (:db/id v))
                                  #_(def ret v)
                                  (transact-cell! v)
                                  )
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
                 :cell/form-str "2012"))
    )
  (:cell/refs (db/entity 6))
  (transact-cell-fn!
    (eval-cell (assoc (db/entity 6)
                 :cell/form-str "(db/cell-ret \"year\")"))
    )
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
  (mount/stop #'eval-schedule-listener #'db-eval-refs-listener)
  (mount/stop #'db-eval-form-str-listener #'db-eval-refs-listener
              #'eval-schedule-listener)
  #_(db/transact! [[:db/retract 6 :cell/refs]
                   [:db/add 6 :cell/refs 18]])

  ;(:sheet/_cells (db/entity 6))
  (-> (db/entity 6) :cell/refs first :cell/ret-str))


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
    (db/datoms :ave :cell/form-str))
  )

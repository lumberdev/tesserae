(ns tesserae.ui.sheet
  (:require
    [clojure.string :as str]
    [hyperfiddle.electric :as e]
    [hyperfiddle.electric-dom2 :as dom]
    [hyperfiddle.electric-svg :as svg]
    [medley.core :as md]
    [stuffs.keybind :as keybind]
    [spyscope.core]
    [missionary.core :as m]
    [tesserae.ui.globals :as g]
    [tesserae.eval.schedule :as eval.sched]
    [stuffs.env :as env]
    [tesserae.ui.render :as uir]
    [tesserae.ui.electric-util :as eu :include-macros true]
    [tesserae.ui.vega :as ui.vega :include-macros true]
    [tesserae.ui.popup :as popup :include-macros true]
    [kitchen-async.promise :as p]
    [net.cgrand.xforms :as x]
    [stuffs.js-interop :as j]
    [stuffs.util :as su]
    [tick.core :as t]
    [tesserae.eval.vars :as eval.vars]
    #?@(:clj  [[tesserae.db :as db]
               [stuffs.datalevin.util :as sdu]
               [tesserae.eval :as eval]]
        :cljs [[stuffs.dom :as sdom]]))
  #?(:cljs (:require-macros [tesserae.ui.sheet]))
  (:import [hyperfiddle.electric Pending]))

(e/def active-element
  ;#?(:cljs)
  (e/client
    (->> (m/observe
           (fn [!]
             (letfn [(!active [e]
                       (! (.-activeElement js/document)))]
               (.addEventListener js/window "focus" !active true)
               #(.removeEventListener js/window "focus" !active true))))
         (m/relieve {})
         (m/reductions {} nil)
         new)))

(defn- coll->css-named-grid-cols [coll]
  (transduce (comp (map (su/wrap "[" "]"))
                   (map #(str % (if (= "[xHEADER]" %)
                                  " 1.5rem "
                                  " auto "
                                  #_" minmax(4rem auto) "))))
             str
             coll))

(defn- coll->css-named-grid-rows [coll]
  (transduce (comp (map (su/wrap "[" "]"))
                   (map #(str % (if (= "[yHEADER]" %)
                                  " 1.5rem "
                                  " auto "))))
             str
             coll))

(defn css-col [c] (str "x" c))
(defn css-row [r] (str "y" r))

(defn css-grid-columns [cols-count]
  (coll->css-named-grid-cols
    (map css-col
         (cons "HEADER" (range 1 (inc cols-count))))))

(defn css-grid-rows [rows-count]
  (coll->css-named-grid-rows
    (map css-row
         (cons "HEADER" (range 1 (inc rows-count))))))

(def !active-cell-pos (atom nil))

(e/defn eval-tx-cell! [{:keys [cell eval-fn timeout] :as opts}]
  (let [cb-str eu/clipboard-on-window-focus]
    (e/server
      (db/transact! [(assoc cell :cell/ret-pending? true)]
                    {:transacted-by ::cell})
      (let [opts   (cond-> opts (nil? eval-fn) (assoc :eval-fn eval/eval-cell))
            result (new
                     (e/task->cp
                       (eval/eval-cell-task
                         (update
                           opts
                           :eval-fn (fn [eval-f]
                                      (fn [cell]
                                        (binding
                                          [eval.vars/*cb-str* cb-str]
                                          (eval-f cell)))
                                      )))))]
        (db/transact! [result]
                      {:transacted-by ::cell}))
      true)))


(defn arrow-pos-fn! [cols-count rows-count]
  (fn arrow-pos! [dir]
    (when-let [[x y] @!active-cell-pos]
      (letfn [(cyc [v max] (if (zero? v) max v))
              (modc [cv op max] (cyc (mod (op cv) max) max))]
        (reset! !active-cell-pos
                (case dir
                  :right [(modc x inc cols-count) y]
                  :left [(modc x dec cols-count) y]
                  :up [x (modc y dec rows-count)]
                  :down [x (modc y inc rows-count)]))))))

(def !editor-cell-pos (atom nil))


(e/defn IncDecButtons [state cb]
  (dom/div
    (dom/props {:class ["flex" "border" "border-black" :rounded-sm :m-2]})
    (dom/button
      (dom/props {:class ["font-bold" "px-1"]})
      (dom/on "click" (e/fn [_]
                        (new cb (dec state))))
      (dom/on "dblclick"
              (e/fn [e] (.stopPropagation e)))
      (dom/text "-"))
    (dom/text " " state " ")
    (dom/button
      (dom/props {:class ["font-bold" "px-1"]})
      (dom/on "click" (e/fn [_] (new cb (inc state))))
      (dom/on "dblclick"
              (e/fn [e] (.stopPropagation e)))
      (dom/text "+"))))



(e/defn CellRet [{:as <cell-ent :keys [db/id] :cell/keys [exception? form-str ret ret-str ret-pending? pos x y]}]
  (e/client
    (dom/div
      (dom/props {:class ["grow" "flex" :items-center #_:w-max]
                  :style {:background (when (and (not ret-pending?) exception?) "salmon")}})
      (e/server
        (let [cell-ent (db/entity id)]
          (cond
            exception? (e/client
                         (dom/details
                           (dom/summary (dom/text "Error"))
                           (dom/pre
                             (dom/props {:class [:max-h-80 :overflow-auto :whitespace-pre-line]})
                             (dom/text ret-str))))
            ret
            (cond
              (or (number? ret) (string? ret)) (e/client (dom/div
                                                           (dom/props {:class [:max-w-screen-sm :max-h-96 :overflow-auto]})
                                                           (dom/text (str ret))))
              (= :vegalite (uir/content-type ret)) (new ui.vega/VegaLiteEmbed (uir/value ret))
              (= :ui/button (uir/content-type ret)) (e/client
                                                      (let [state (uir/value ret)]
                                                        (dom/div
                                                          (dom/props {:class [:flex :justify-center :grow :p-2]})
                                                          (dom/button
                                                            (dom/props
                                                              {:class ["pointer-events-auto"
                                                                       "rounded-md"
                                                                       "bg-indigo-600"
                                                                       "py-2"
                                                                       "px-3"
                                                                       "text-[0.8125rem]"
                                                                       "font-semibold"
                                                                       "leading-5"
                                                                       "text-white"
                                                                       "hover:bg-indigo-500"]})
                                                            (dom/on "click"
                                                                    (e/fn [_]
                                                                      (e/server
                                                                        (new eval-tx-cell! {:cell cell-ent}))))
                                                            (dom/text state)))))
              (= :ui/inc-dec (uir/content-type ret)) (e/client
                                                       (let [state (uir/value ret)]
                                                         (new IncDecButtons
                                                              state
                                                              (e/fn [nv]
                                                                (e/server
                                                                  (let [v (assoc ret ::uir/val nv)]
                                                                    (db/transact! [{:db/id         id
                                                                                    :cell/form-str (pr-str v)}]))
                                                                  nil)))))
              (uir/content-type ret) (let [html (uir/->html-str ret)]
                                       (e/client
                                         (j/assoc!
                                           dom/node
                                           :innerHTML html)
                                         nil))
              (coll? ret) (let [pretty (su/pretty-string ret)]
                            (e/client
                              (dom/pre
                                (dom/props {:class [:max-w-screen-sm :max-h-96 :overflow-auto]})
                                (dom/text pretty)))))

            ret-str
            (e/client (dom/pre
                        (dom/props {:class [:max-w-screen-sm :max-h-96 :overflow-auto]})
                        #_(e/server (str (type ret)))
                        (dom/text ret-str)))))))))

(e/defn ClockSym []
  (e/client
    (svg/svg
      (dom/props {:viewBox "0 0 526 526"
                  :class   [:h-3]})
      (svg/path
        (dom/props {:fill-rule "evenodd",
                    :clip-rule "evenodd",
                    :fill      "black"
                    :d         "M263 0.5C118.12 0.5 0.5 118.12 0.5 263C0.5 407.88 118.12 525.5 263 525.5C407.88 525.5 525.5 407.88 525.5 263C525.5 118.12 407.88 0.5 263 0.5ZM263 35.5C388.56 35.5 490.5 137.44 490.5 263C490.5 388.56 388.56 490.5 263 490.5C137.44 490.5 35.5 388.56 35.5 263C35.5 137.44 137.44 35.5 263 35.5ZM245.5 123V263C245.5 268.059 247.688 272.871 251.52 276.195L347.77 359.949C355.051 366.285 366.109 365.515 372.445 358.234C378.797 350.953 378.028 339.894 370.731 333.543L280.501 255.039V122.999C280.501 113.339 272.661 105.499 263.001 105.499C253.341 105.499 245.501 113.339 245.501 122.999L245.5 123Z",
                    }))
      )))

;; fixme HACK
(defn toggle-class-on-change
  ([change-val node add-css-classes]
   (toggle-class-on-change change-val node add-css-classes add-css-classes))
  ([change-val node add-css-classes remove-css-classes]
   #?(:cljs
      (let [cl (.-classList node)]
        (j/apply cl :add (into-array add-css-classes))
        (set! (.-offsetWidth node) (.-offsetWidth node))
        (js/setTimeout
          #(j/apply cl :remove (into-array remove-css-classes))
          2000)))))

(e/defn CellAnchorMenu [{:as <cell-ent :keys [db/id] :cell/keys [evaled-at eval-upon schedule ret]}
                        {:keys [set-edit-schedule]}]
  (let [cell-ent  (or (db/entity id) <cell-ent)
        schedule? (boolean schedule)
        {shed-text :schedule/text sched-next :schedule/next} schedule]
    (e/client
      (new popup/Menu
           {:anchor (e/fn [] (new popup/TriangleAnchor {}))
            :items  [{:override
                      (e/fn []
                        (dom/div
                          (dom/div (dom/props {:class [:p-1 :whitespace-pre]})
                                   (dom/text "id: " id))
                          (when evaled-at
                            (dom/div (dom/props {:class [:p-1 :whitespace-pre]})
                                     (dom/text "last run: " (su/local-date-time-string evaled-at))))
                          (when sched-next
                            (dom/div (dom/props {:class [:p-1 :whitespace-pre]})
                                     (dom/text "next run: " (su/local-date-time-string sched-next))))))}
                     {:label    "run"
                      :on-click (e/fn [_]
                                  (e/server
                                    (new eval-tx-cell! {:cell cell-ent})
                                    false))}
                     {:label    (str (if schedule? "change" "add") " schedule")
                      :on-click (e/fn [_]
                                  (e/server
                                    (new set-edit-schedule true))
                                  false)}
                     {:label    "copy"
                      :on-click (e/fn [_]
                                  (some-> ret uir/value str sdom/copy-to-clipboard)
                                  false)}
                     {:label                   "delete"
                      :label-after-first-click "are you sure?"
                      :on-click                (e/fn [_]
                                                 (e/server
                                                   (db/transact! [[:db/retractEntity id]])
                                                   nil)
                                                 false)}
                     {:label    "run on"
                      :subitems [(let [checked? (contains? eval-upon :ui/window-focus)]
                                   {:label    "window focus"
                                    :checked? checked?
                                    :on-click (e/fn [_]
                                                (e/server
                                                  (db/transact!
                                                    [[(if checked?
                                                        :db/retract
                                                        :db/add)
                                                      id :cell/eval-upon :ui/window-focus]])
                                                  nil)
                                                true)})]}
                     ]}))))

(e/defn CellEvalUpon [{:keys [db/id cell/eval-upon]}]
  (e/for [eval-trigger eval-upon]
    (case eval-trigger
      :ui/window-focus (when eu/window-focus
                         (e/server
                           (new eval-tx-cell! {:cell (db/entity id)})
                           false)))))

(e/defn EditableCell
  [{:as         <cell-ent
    :keys       [db/id db/updated-at]
    :cell/keys  [form-str pos x y ret ret-pending? schedule evaled-at eval-upon]
    :sheet/keys [_cells cols-count]
    cname       :cell/name}]
  (e/server
    (let [cell-ent  (or (db/entity id) <cell-ent)
          schedule? (boolean schedule)
          {shed-text :schedule/text sched-next :schedule/next} schedule]
      (e/client
        (when eval-upon
          (new CellEvalUpon {:db/id id :cell/eval-upon eval-upon}))
        (let [active-cell-pos    (e/watch !active-cell-pos)
              active?            (= pos active-cell-pos)
              editor-cell-pos    (e/watch !editor-cell-pos)
              editor?            (= pos editor-cell-pos)
              toggle-change-anim (su/f-skip 1 toggle-class-on-change)]
          (dom/div
            (let [cell-node dom/node]
              (when active? (.focus cell-node))
              (dom/on "focus"
                      (e/fn [_]
                        (reset! !active-cell-pos pos)))
              (dom/on "dblclick"
                      (e/fn [_] (reset! !editor-cell-pos pos)))
              (dom/on "keydown"
                      (e/fn [e]
                        (keybind/chord-case e
                          "enter" (reset! !editor-cell-pos pos)
                          "shift+enter" (do
                                          (.preventDefault e)
                                          (e/server
                                            (new eval-tx-cell! {:cell cell-ent})
                                            nil)))))
              (dom/props
                {:class    ["bg-white" "flex" "flex-col" :outline-none :rounded-sm
                            (when active? "z-10")
                            (when ret-pending? "animate-[shadow-pulse_1s_ease-in-out_infinite] z-10")]
                 :style    {:grid-column-start (css-col x)
                            :grid-row-start    (css-row y)
                            :border            (when active? "2px solid blue")
                            :margin            (when active? "-2px")}
                 :tabindex (int
                             (+ (Math/pow (+ cols-count y) 2)
                                x))})
              ;; hacky
              (toggle-change-anim updated-at dom/node
                                  ["animate-[shadow-pulse-yellow_1s_ease-in-out]" "z-10"]
                                  ["animate-[shadow-pulse-yellow_1s_ease-in-out]" "z-10"])
              (e/server
                (new CellRet <cell-ent)
                (e/client
                  (when editor?
                    (dom/textarea
                      (.focus dom/node)
                      (dom/props {:class ["grow" "outline-none" "border-t-[2px]"]
                                  :value form-str
                                  :style {:padding "10px"}})
                      (dom/on "keydown"
                              (e/fn [e]
                                (keybind/chord-case e
                                  ("left" "right" "up" "down") (.stopPropagation e)
                                  "shift+enter" (let [s (-> (j/get-in e [:target :value]) str/trim)]
                                                  (.preventDefault e)
                                                  ;; prevent running twice
                                                  ;; by calling listener in ascending node
                                                  (.stopPropagation e)
                                                  (e/server
                                                    (new eval-tx-cell! {:cell (assoc cell-ent :cell/form-str s)})
                                                    false))
                                  "esc" (do #_(.blur dom/node)
                                          (.focus cell-node)))))
                      (dom/on "blur"
                              (e/fn [e]
                                ;; this only blur when the next active node is not the current node
                                ;; i.e. it will not blur on tab switch, only when another node is focused
                                (if (= dom/node (j/get js/document :activeElement))
                                  (.preventDefault e)
                                  (let [s (-> (j/get-in e [:target :value]) str/trim)]
                                    (e/server
                                      (let [txr (new eval-tx-cell! {:cell    (assoc cell-ent :cell/form-str s)
                                                                    :eval-fn eval/fmt-eval-cell})]
                                        (when txr
                                          (e/client (reset! !editor-cell-pos nil))))
                                      nil)))))))
                  (dom/div
                    (dom/props {:class ["gap-1" "flex" "justify-between" :p-1 :h-6]
                                :style {:min-width "4rem"}})
                    (when (or cname active?)
                      (dom/on "keydown"
                              (e/fn [e]
                                (keybind/chord-case e
                                  ("left" "right" "up" "down") (.stopPropagation e)
                                  "esc" (do #_(.blur dom/node)
                                          (.focus cell-node))))))
                    (let [[!edit-schedule? edit-schedule?] (eu/state false)]
                      (if-not edit-schedule?
                        (dom/input
                          (dom/on "keydown"
                                  (e/fn [e]
                                    (keybind/chord-case e
                                      ("left" "right" "up" "down") (.stopPropagation e))))
                          (dom/on "blur"
                                  (e/fn [e]
                                    (let [s (-> (j/get-in e [:target :value])
                                                str/trim not-empty)]
                                      (e/server
                                        (db/transact! [(if s
                                                         (assoc cell-ent :cell/name s)
                                                         (when id
                                                           [:db/retract id :cell/name]))])
                                        nil))))
                          (dom/props {:class       ["text-xs"
                                                    "outline-none"
                                                    :w-full
                                                    (when-not (or cname active?) "hidden")]
                                      :placeholder "name.."
                                      :value       cname}))
                        (dom/div
                          (dom/props {:class [:flex :items-end :h-4 :gap-1 :w-max :relative]})
                          (new ClockSym)
                          (let [[!parsed parsed] (eu/state nil)]
                            (dom/input
                              (dom/on "keydown"
                                      (e/fn [e]
                                        (keybind/chord-case e
                                          ("left" "right" "up" "down") (.stopPropagation e)
                                          ("esc" "enter") (do (.stopPropagation e)
                                                              (.blur dom/node)))))
                              (dom/on "blur"
                                      (e/fn [e]
                                        (let [sched-str (some->> (j/get-in e [:target :value]) str/trim not-empty)]
                                          (if sched-str
                                            (e/server
                                              (when-let [sched (eval.sched/parsed->schedule parsed)]
                                                (when (db/transact! [{:db/id         id
                                                                      :cell/schedule sched}])
                                                  (e/client (reset! !edit-schedule? false)))
                                                nil))
                                            (e/server
                                              (when (db/transact! [[:db/retract id :cell/schedule]])
                                                (e/client (reset! !edit-schedule? false)))
                                              nil)))))
                              (dom/on "input"
                                      (e/fn [e]
                                        (reset! !parsed
                                                (some->> (j/get-in e [:target :value])
                                                         str/trim not-empty eval.sched/parse))))
                              (.focus dom/node)
                              (dom/props {:class       ["text-xs"
                                                        "outline-none"
                                                        :w-full]
                                          :placeholder "e.g. daily at 4pm..."
                                          :value       (or shed-text "")}))

                            (when-let [{:keys [time-at repeat time-in errored?]} parsed]
                              (when-not errored?
                                (dom/div
                                  (dom/props {:class [:absolute :z-30 :w-max :p-1 :flex :flex-col :bg-white
                                                      :border :border-black :top-full :text-xs :font-mono]})
                                  (cond
                                    repeat (let [[tag v] repeat
                                                 next-runs (sequence
                                                             (comp (filter eval.sched/future?)
                                                                   (take 3))
                                                             (case tag
                                                               (:day :days) (eval.sched/next-matching-days time-at v)
                                                               :period (eval.sched/next-times time-at v)))]
                                             (dom/text "Next 3 runs:")
                                             (dom/div
                                               (dom/props {:class [:pl-3]})
                                               (e/for [r next-runs]
                                                 (dom/div (dom/text (eval.sched/fmt-day-date-time r))))))
                                    time-in (dom/div (dom/text (eval.sched/fmt-day-date-time
                                                                 (t/>> time-at (second time-in)))))
                                    time-at (dom/div (dom/text (eval.sched/fmt-day-date-time time-at))))))
                              ))))
                      (when active?
                        (e/client
                          (dom/div
                            (dom/props {:class [:flex :gap-1 :items-end]})
                            (when (and schedule? (not edit-schedule?))
                              (dom/span
                                (dom/props {:class [:cursor-pointer]})
                                (dom/on "click" (e/fn [_] (reset! !edit-schedule? true)))
                                (new ClockSym)
                                ))
                            (when id
                              (e/server
                                (new CellAnchorMenu
                                     <cell-ent
                                     {:set-edit-schedule
                                      (e/fn [v]
                                        (e/client
                                          (reset! !edit-schedule? v)))})))))))))))))))))


(e/defn Sheet [{:as         x
                :keys       [db/id]
                :sheet/keys [cells cols-count rows-count]}]
  (let [pos->cell (into {} (map (juxt :cell/pos identity)) cells)]
    (e/client
      (let [arrow-pos! (arrow-pos-fn! cols-count rows-count)
            cols       (range 1 (inc cols-count))
            rows       (range 1 (inc rows-count))]
        (dom/div
          (dom/on "keydown"
                  (e/fn [e]
                    (keybind/chord-case e
                      "right" (do
                                (.preventDefault e)
                                (arrow-pos! :right))
                      "left" (do
                               (.preventDefault e)
                               (arrow-pos! :left))
                      "up" (do
                             (.preventDefault e)
                             (arrow-pos! :up))
                      "down" (do
                               (.preventDefault e)
                               (arrow-pos! :down)))))
          (dom/props
            {:class ["w-full" "h-full" "inline-grid" "bg-amber-300" :overflow-auto]
             :style {:overscroll-behavior-x :none
                     :gap                   :1px
                     :grid-template-columns (css-grid-columns cols-count)
                     :grid-template-rows    (css-grid-rows rows-count)}})

          (dom/div
            (dom/props {:style {:grid-column-start (css-col "HEADER")
                                :grid-row-start    (css-row "HEADER")}}))
          (e/for [col cols]
            (dom/div
              (dom/props
                {:class ["sticky" "flex" "justify-center"
                         "align-center" "font-mono" "text-xs"
                         "bg-amber-300" "border-b-[1px]" "border-black"
                         "z-20" :py-1]
                 :style {:top               0
                         :grid-column-start (css-col col)
                         :grid-row-start    (css-row "HEADER")}})
              (dom/text col)
              ))
          (e/for [row rows]
            (dom/div
              (dom/props
                {:class ["sticky" "flex" "justify-center"
                         "align-center" "font-mono" "text-xs"
                         "bg-amber-300" "border-r-[1px]" "border-black"
                         "z-20" "px-1" :items-center]
                 :style {:left              0
                         :grid-column-start (css-col "HEADER")
                         :grid-row-start    (css-row row)}})
              (dom/text row)))
          (e/server
            (e/for [x (range 1 (inc cols-count)) y (range 1 (inc rows-count))]
              (let [pos           [x y]
                    existing-cell (pos->cell pos)]
                (new EditableCell
                     (or
                       existing-cell
                       {:cell/pos pos :cell/x x :cell/y y :sheet/_cells {:db/id id}}))))))))))



(e/defn Entrypoint [sheet-ent]
  (e/server
    (new Sheet sheet-ent)))

(comment

  (db/entity [:dev-id "sheet7"])
  (:cell/pos (:sheet/cells (db/entity [:dev-id "sheet4"])))

  (:sheet/cells (db/entity [:dev-id "55"]))
  (db/transact!
    [{:sheet/name "demo"}]
    )
  (db/datoms :ave :sheet/name)
  (db/transact!
    [{:dev-id      "demo"
      :sheet/cells [{:cell/name     "runs-clj"
                     :dev-id        "runs-clj"
                     :cell/x        5
                     :cell/y        5
                     :cell/form-str "(sleep 1000)\n(repeatedly 10 #(rand-int 10))"}]}]))

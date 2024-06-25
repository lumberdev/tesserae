(ns tesserae.ui.views
  (:require
    [hyperfiddle.electric :as e]
    [hyperfiddle.electric-dom2 :as dom]
    [clojure.string :as str]
    [missionary.core :as m]
    [stuffs.dom :as sdom]
    [stuffs.js-interop :as j]
    [spyscope.core]
    [stuffs.keybind :as keybind]
    [tesserae.ui.sheet :as sh]
    [tesserae.ui.panel :as panel]
    [tesserae.ui.electric-util :as eu]
    [tesserae.ui.typeahead :refer [Typeahead]]
    [tesserae.ui.popup :as popup]
    [tesserae.ui.globals :as g]
    [tesserae.ui.vega :as ui.vega]
    [tesserae.ui.notif :as notif]
    #_[tesserae.ui.task :as task]
    [stuffs.route :as route]
    [stuffs.util :as su]
    [tesserae.serialize]
    #?@(:clj [[datalevin.core :as d]
              [stuffs.datalevin.util :as sdu]
              [tesserae.db :as db]])))

#?(:clj
   (defn search-cell-names->ent [db q]
     (when-let [q (some-> q str/trim str/lower-case)]
       (let [pat (re-pattern (str "\\b" q "\\w*"))]
         (sequence
           (comp
             (filter (fn [{:keys [v]}]
                       (re-find pat v)))
             (map db/entity))
           (d/datoms db :ave :cell/name))))))

#?(:clj
   (defn search-attrs->ents [attrs q]
     (when-let [q (some-> q str/trim not-empty str/lower-case)]
       (let [pat (re-pattern (str "\\b" q "\\w*"))]
         (not-empty
           (sequence
             (comp
               (mapcat #(db/datoms :ave %))
               (filter (fn [{:keys [v]}]
                         (re-find pat v)))
               (map db/entity))
             attrs))))))

#?(:clj
   (defn recent-attrs->ents [attrs]
     (not-empty
       (sequence
         (comp (map db/entity)
               (filter #(some % attrs)))
         (reverse (db/datoms :ave :db/updated-at))))))

#?(:clj
   (defn search-or-recent->ents [attrs q]
     (if (empty? q)
       (recent-attrs->ents attrs)
       (search-attrs->ents attrs q))))

#_(e/defn CellSearch []
    (e/server
      (let [[!q q] (eu/state "")]
        (e/client
          (dom/input
            (dom/props {:value q})
            (dom/on "input"
                    (e/fn [e]
                      (let [in (-> (j/get-in e [:target :value])
                                   str/trim)]
                        (e/server (reset! !q in)))))))
        (e/client
          (dom/div
            (dom/props {:class [:flex :flex-row :flex-wrap :gap-3]})
            (e/server
              (e/for-by :db/id [{:as c :cell/keys [name]} (search-cell-names->ent g/db q)]
                ;(e/client)
                (e/client
                  (dom/div
                    (dom/props {:class [:shadow-md :p-2 :rounded-md]})
                    (e/server
                      (new sh/EditableCell c))))
                #_(dom/div (dom/text name)))))))))

#_(e/defn SideBar []
    (e/client
      (let [[!open? open?] (eu/state true)]
        (when open?
          (dom/div
            (dom/props {:class [:h-full :w-80 :bg-amber-200]})
            (dom/text "foo")
            ;(new Search)
            )))))

(e/defn Recents []
  (e/client
    (dom/div
      (dom/props {:class [:flex :flex-col :items-center :pt-5]})
      (e/server
        (when-let [recent-sheets (not-empty (take 10 (recent-attrs->ents [:sheet/name])))]
          (e/client
            (dom/div
              (dom/props {:class [:text-center :text-xl :font-bold]})
              (dom/text "Recents"))
            (dom/div
              (e/server
                (e/for-by :db/id [{:as sh id :db/id :sheet/keys [name]} recent-sheets]
                  ;(e/client)
                  (e/client
                    (dom/div
                      (dom/props {:class [:cursor-pointer "hover:bg-slate-100"]})
                      (dom/on "mousedown"
                              (e/fn [_]
                                (route/push-state
                                  (if sdom/mobile-device? :panel :sheet)
                                  {:id id})))
                      (dom/text name)))))))))
      #_(dom/div
          (dom/props {:class [:text-center :text-xl :font-bold]})
          (dom/text "Tasks"))
      #_(new task/Tasks)

      (dom/div
        (dom/props {:class [:text-center :text-xl :font-bold :pt-5]})
        (dom/text "Settings"))
      (new notif/Toggle))))

(e/defn Nav []
  (e/client
    (dom/div
      (dom/props {:class [:flex
                          :justify-between
                          :items-center
                          :w-full
                          :py-0.5
                          :h-8
                          :px-1
                          :group]})
      (dom/div
        (dom/props {:class [:sm:w-80 :w-16]})
        (dom/a
          (dom/props {:href  "/app"
                      :class [:gap-2 :flex :items-center]})
          (dom/div
            (dom/props {:class [:rounded-full :h-5 :w-5]
                        :style {:background "#FFCC08"}}))
          (dom/span
            (dom/props {:class ["hidden" "md:inline"]})
            (dom/text "Tesserae"))))
      ;; ent
      (when-let [eid (-> g/route-match :parameters :path :id)]
        (e/server
          (let [ent (db/entity eid)
                nm  ((some-fn :sheet/name) ent)]
            (e/client
              (dom/div
                (dom/props {:class [:items-center
                                    "group-focus-within:hidden"
                                    "group-focus-within:sm:flex"
                                    "flex"]})
                (dom/span
                  (dom/on "blur"
                          (e/fn [e]
                            (let [v (-> (j/get-in e [:target :innerText])
                                        str/trim
                                        not-empty)]
                              (e/server
                                #_(println :running eid)
                                (db/transact! [(if v
                                                 ;[:db/add eid :sheet/name v]
                                                 ;[:db/retract eid :sheet/name]
                                                 (assoc ent :sheet/name v)
                                                 (dissoc ent :sheet/name))])
                                nil))))
                  (dom/props {:role            "input"
                              :contenteditable true
                              :class           [:outline-none :text-center :whitespace-nowrap]
                              :placeholder     "untitled"})
                  (dom/text (or nm "")))
                (new popup/Menu
                     {:anchor (e/fn [] (new popup/TriangleAnchor {:css-class "m-1"}))
                      :items  [(let [oppo (case g/route
                                            :panel :sheet
                                            :sheet :panel)]
                                 {:label    (str "show as " (name oppo))
                                  :on-click (e/fn [_]
                                              (route/push-state oppo {:id eid}))})
                               {:label                   "delete"
                                :label-after-first-click "are you sure?"
                                :on-click                (e/fn [_]
                                                           (e/server
                                                             (db/transact! [[:db/retractEntity eid]]) nil)
                                                           (e/client
                                                             (route/push-state :home)))}]}))))))
      (e/server
        (new Typeahead
             {:placeholder                 "search or create"
              :template-fn                 (e/fn [ent]
                                             ((some-fn :sheet/name :panel/name) ent))
              :on-create                   (e/fn [{:keys [input-value]}]
                                             (e/server
                                               (let [{:keys [db/id]} (db/transact-entity!
                                                                       {:sheet/name       input-value
                                                                        :sheet/cols-count 10
                                                                        :sheet/rows-count 10})]
                                                 (e/client (route/push-state :sheet {:id id})
                                                           {:input-value ""}))))
              :on-pick                     (e/fn [{:keys [picked]}]
                                             (e/client
                                               (route/push-state
                                                 (if sdom/mobile-device? :panel :sheet)
                                                 {:id (e/server (:db/id picked))})
                                               {:input-value ""}))
              :on-blur                     (e/fn [_]
                                             {:input-value ""})
              :on-esc                      (e/fn [_]
                                             {:input-value ""
                                              :blur?       true})
              :suggestions-fn              (e/fn [q]
                                             (take 10 (search-or-recent->ents [:sheet/name] q)))
              :container-class             "relative z-[10000] sm:w-80 w-16 group-focus-within:w-80"
              :input-class                 [:outline-none :w-full :text-right "focus:text-left" :truncate]
              :suggestions-container-style {:border "1px solid black"}
              :suggestions-container-class [:flex :flex-col :gap-1 :py-1 :absolute :bg-white :mt-1 :w-full]
              :suggestion-class            "px-1 hover:bg-slate-200"
              :selected-suggestion-class   :bg-slate-200})))))

(e/defn Route []
  (e/client
    (dom/div
      (dom/props {:class [:flex :flex-col]
                  :style {:height "calc(100vh - 30px)"}})
      (case g/route
        :home (new Recents)

        (:sheet :panel :cell)
        (let [id (-> g/route-match :parameters :path :id)]
          (e/server
            (let [<ent (sdu/entity g/db id)]
              (e/client
                (case g/route
                  :sheet (e/server (new sh/Entrypoint <ent))
                  :panel (e/server (new panel/Entrypoint <ent))
                  :cell (dom/div
                          (dom/props {:class [:flex :justify-center :p-2 :overflow-auto :h-full]})
                          (e/server (new sh/EditableCell <ent))))))))))))

;; *** debugging views

(defonce xx (atom nil))
(defn setxx [x] (reset! xx x))

(defonce dbg-html (atom nil))
(defn sethtml [x] (reset! dbg-html x) :done)
(defonce dbg-vl (atom nil))
(defn setvl
  ([] (setvl nil))
  ([x]
   (reset! dbg-vl (some-> x su/write-json-string)) :done))

(defn cleardbg []
  (setxx nil)
  (sethtml nil)
  (setvl nil))

(comment
  (cleardbg))

(e/defn AtomPre [label a]
  (let [v  (e/watch a)
        pv (some-> v su/pretty-string)]
    (when v
      (e/client
        (dom/div
          (dom/props {:style {:display :flex
                              :gap     :5px}})
          (dom/div (dom/text label))
          (dom/pre (dom/props {:style {:margin 0}}) (dom/text pv)))))))

(e/defn Debug []
  (when-let [vega-json-spec (e/watch dbg-vl)]
    (new ui.vega/VegaLiteEmbed vega-json-spec))
  (when-let [html (e/watch dbg-html)]
    (e/client
      (dom/div
        (j/assoc!
          dom/node
          :innerHTML html))))
  (e/client
    (new AtomPre "client xx" xx))
  (e/server
    (new AtomPre "server xx" xx)))

(e/defn App []
  (e/server
    (new Debug))
  (e/client
    (dom/div
      (dom/props {:class [:flex :flex-col :w-100vw :h-100vh :overflow-hidden]})
      (e/server
        (binding [g/db (new (eu/async-watch db/conn))]
          (new Nav)
          (new Route))))))

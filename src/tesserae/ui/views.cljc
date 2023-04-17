(ns tesserae.ui.views
  (:require
    [hyperfiddle.electric :as e]
    [hyperfiddle.electric-dom2 :as dom]
    [clojure.string :as str]
    [stuffs.js-interop :as j]
    [spyscope.core]
    [stuffs.keybind :as keybind]
    [tesserae.ui.sheet :as sh :include-macros true]
    [tesserae.ui.electric-util :as eu]
    [tesserae.ui.typeahead :refer [Typeahead] :include-macros true]
    [tesserae.ui.globals :as g]
    [stuffs.route :as route]
    #?@(:clj [[datalevin.core :as d]
              [stuffs.datalevin.util :as sdu]
              [tesserae.db :as db]]))
  #?(:cljs (:require-macros tesserae.ui.views)))

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
             attrs
             ))))))

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
            )
          )
        )
      ))

(e/defn Recents []
  (e/client
    (dom/div
      (dom/props {:class [:flex :flex-col :items-center :pt-5]})
      (dom/div
        (dom/props {:class [:text-center :text-xl :font-bold]})
        (dom/text "Recents"))
      (dom/div
        (e/server
          (when-let [ents (take 10 (recent-attrs->ents [:sheet/name]))]
            (e/for-by :db/id [{:as sh id :db/id :sheet/keys [name]} ents]
              ;(e/client)
              (e/client
                (dom/div
                  (dom/props {:class [:cursor-pointer "hover:bg-slate-100"]})
                  (dom/on "mousedown"
                          (e/fn [_]
                            (route/push-state :sheet {:id id})))
                  (dom/text name))))))))))

(e/defn Nav []
  (e/client
    (dom/div
      (dom/props {:class [:flex
                          :justify-between
                          :items-center
                          :w-full
                          :py-0.5
                          :h-8
                          :px-1]})
      (dom/div
        (dom/props {:class [:w-80]})
        (dom/a
          (dom/props {:href  "/app"
                      :class [:gap-2 :flex :items-center]})
          (dom/div
            (dom/props {:class [:rounded-full :h-5 :w-5]
                        :style {:background "#FFCC08"}}))
          (dom/text "Tesserae")))
      ;; ent
      (when-let [eid (-> g/route-match :parameters :path :id)]
        (e/server
          (let [ent (db/entity eid)
                nm  ((some-fn :sheet/name) ent)]
            (e/client
              (dom/input
                (dom/on "blur"
                        (e/fn [e]
                          (let [v (-> (j/get-in e [:target :value])
                                      str/trim
                                      not-empty)]
                            (e/server
                              #_(println :running eid)
                              (db/transact! [(if v
                                               ;[:db/add eid :sheet/name v]
                                               ;[:db/retract eid :sheet/name]
                                               (assoc ent :sheet/name v)
                                               (dissoc ent :sheet/name))])
                              nil)
                            )))
                (dom/props {:value       (or nm "")
                            :class       [:outline-none :text-center]
                            :placeholder "untitled"}))))))
      (e/server
        (new Typeahead
             {:placeholder                 "search or create"
              :template-fn                 (e/fn [ent]
                                             ((some-fn :sheet/name :panel/name) ent))
              :on-create                   (e/fn [{:keys [input-value]}]
                                             (e/server
                                               (let [{:keys [db/id]} (db/transact-entity! {:sheet/name input-value})]
                                                 (e/client (route/push-state :sheet {:id id})
                                                           {:input-value ""})
                                                 )))
              :on-pick                     (e/fn [{:keys [picked]}]
                                             (e/client
                                               (route/push-state :sheet {:id (e/server (:db/id picked))})
                                               {:input-value ""}))
              :on-blur                     (e/fn [_]
                                             {:input-value ""})
              :on-esc                      (e/fn [_]
                                             {:input-value ""
                                              :blur?       true})
              :suggestions-fn              (e/fn [q]
                                             (take 10 (search-or-recent->ents [:sheet/name] q)))
              :container-class             "relative z-30 w-80"
              :input-class                 [:outline-none :w-full :text-right "focus:text-left"]
              :suggestions-container-style {:border "1px solid black"}
              :suggestions-container-class [:flex :flex-col :gap-1 :py-1 :absolute :bg-white :mt-1 :w-full]
              :suggestion-class            "px-1 hover:bg-slate-200"
              :selected-suggestion-class   :bg-slate-200})))))

(e/defn Route []
  (e/client
    (dom/div
      (dom/props {:style {:height "calc(100vh - 30px)"}})
      (case g/route
        :home (new Recents)
        :sheet (let [sheet-id (-> g/route-match :parameters :path :id)]
                 (new sh/Entrypoint sheet-id))
        (dom/div
          (dom/props {:class [:text-2xl]})
          (dom/text "not found")
          )))))


(e/defn App []
  (e/client
    (dom/div
      (dom/props {:class [:flex :flex-col :w-100vw :h-100vh :overflow-hidden]})
      (e/server
        (binding [g/db (e/watch db/conn)]
          (new Nav)
          (new Route))))))
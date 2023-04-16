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
         (db/datoms :ave :db/updated-at)))))

#?(:clj
   (defn search-or-recent->ents [attrs q]
     (if (empty? q)
       (recent-attrs->ents attrs)
       (search-attrs->ents attrs q))))



;(db/datoms :ave  :sheet/name)

#_(db/transact! (mapv (fn [{:keys [e a v]}]
                        [:db/add e a v]
                        ) (db/datoms :ave :cell/ret)))

(e/defn Search []
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

(e/defn SideBar []
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

;(:sheet/name (db/entity 1))

;; sidebar
;; search -> shows results on the right
;; below divisions for sheets & panels by name


(e/defn App []
  (e/client
    (dom/div
      (dom/props {:class [:flex :flex-col :w-100vw :h-100vh :overflow-hidden]})
      (e/server
        (binding [g/db (e/watch db/conn)]
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
                (dom/props {:class [:gap-2 :flex :items-center :w-80]})
                (dom/div
                  (dom/props {:class [:rounded-full :h-5 :w-5]
                              :style {:background "#FFCC08"}}))
                (dom/text "Tesserae"))
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
                                    :class       [:outline-none]
                                    :placeholder "untitled"}))
                      ))))
              (dom/div
                (let [[!q q] (eu/state "")
                      [!focus? focus?] (eu/state false)]
                  (dom/props {:class [:relative :w-80]})
                  (dom/input
                    (dom/props {:class       [:outline-none :w-full :text-right "focus:text-left"]
                                :value       q
                                :placeholder (if focus?
                                               "search or create"
                                               "search...")})
                    (dom/on
                      "input"
                      (e/fn [e] (let [v (-> (j/get-in e [:target :value])
                                            str/trim
                                            not-empty)]
                                  (reset! !q v))))
                    (dom/on "focus" (e/fn [e] (reset! !focus? true)))
                    (dom/on "blur" (e/fn [e] (reset! !focus? false)))
                    (dom/on "keydown"
                            (e/fn [e] (keybind/chord-case e
                                        "esc" (do (reset! !q "")
                                                  (.blur dom/node))))))
                  (when focus?
                    (dom/div
                      (dom/props {:class [:absolute :z-30 :w-max :p-1 :flex :flex-col :bg-white
                                          :border :border-black :top-full :text-xs]})
                      (dom/div
                        (dom/props {:class [:flex :flex-col :gap-1]})

                        (e/server
                          (when-let [ents (take 10 (search-or-recent->ents [:sheet/name] q))]
                            (e/for-by :db/id [{:as sh id :db/id :sheet/keys [name]} ents]
                              ;(e/client)
                              (e/client
                                (dom/div
                                  (dom/props {:class [:cursor-pointer "hover:bg-slate-100"]})
                                  (dom/on "mousedown"
                                          (e/fn [_]
                                            (route/push-state :sheet {:id id})))
                                  (dom/text name))))))
                        (when (not-empty q)
                          (dom/div
                            (dom/on "mousedown"
                                    (e/fn [_]
                                      (e/server
                                        (let [{:keys [db/id]} (db/transact-entity! {:sheet/name q})]
                                          (e/client (route/push-state :sheet {:id id}))
                                          ))))
                            (dom/props {:class [:cursor-pointer "hover:bg-slate-100"]})
                            (dom/text (str "Create " q))))))))))
            (dom/div
              (dom/props {:style {:height "calc(100vh - 30px)"}})
              (case g/route
                :home (new SideBar)
                :sheet (let [sheet-id (-> g/route-match :parameters :path :id)]
                         (new sh/Entrypoint sheet-id))
                (dom/div
                  (dom/props {:class [:text-2xl]})
                  (dom/text "not found")
                  )))))))))

;(:cell/form-str (db/entity 7))

;(:cell/ret-str (db/entity 98))
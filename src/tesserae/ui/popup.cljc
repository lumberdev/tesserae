(ns tesserae.ui.popup
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [stuffs.dom :as sdom]
            [tesserae.ui.electric-util :as eu]))

(e/defn TriangleAnchor [{:keys [css-class css-style]
                         :or   {css-style {}}}]
  (dom/div
    (dom/props {:class [:cursor-pointer css-class]
                :style (merge
                         {:width        0
                          :height       0
                          :border-left  "5px solid transparent"
                          :border-right "5px solid transparent"
                          :border-top   "6px solid black"}
                         css-style)})))

(e/defn Menu [{:keys [anchor items]
               :or   {anchor "..."}}]
  (let [[!open? open?] (eu/state false)]
    (dom/div
      (dom/props {:class ["z-[10000]" "text-xs" "relative" :cursor-pointer]})
      (dom/div
        (dom/props
          {:class [:tracking-tighter :p-1]})
        (dom/on "click" (e/fn [_]
                          (swap! !open? not)))
        (if (string? anchor)
          (dom/text anchor)
          (new anchor)))
      (when open?
        ;; basically click-outside
        (dom/on! js/window
                 "click"
                 (fn [e]
                   (when-not (sdom/node=or-contains? dom/node (.-target e))
                     (reset! !open? false))))
        (dom/div
          (dom/props {:class ["absolute" :p-1 :bg-white :flex :flex-col
                              :border :border-black :whitespace-pre]})
          (e/for-by (some-fn :label :override)
            [{:as x :keys [override
                           label
                           label-after-first-click
                           on-click
                           subitems]} items]
            (let [[!clicked-once? clicked-once?] (eu/state (if label-after-first-click false true))
                  [!open-subitems-label open-subitems-label] (eu/state nil)]
              (when x
                (cond
                  override (new override)
                  :else
                  (dom/div
                    (dom/props {:class [:p-1 :relative "hover:bg-slate-200"]})
                    (dom/div
                      (dom/props {:class [:flex :items-baseline :justify-between]})
                      (dom/on "click"
                              (e/fn [e]
                                (if subitems
                                  (if (= label open-subitems-label)
                                    (reset! !open-subitems-label nil)
                                    (reset! !open-subitems-label label))
                                  (if (and label-after-first-click (not clicked-once?))
                                    (reset! !clicked-once? true)
                                    (let [ret (new on-click e)]
                                      (when (boolean? ret)
                                        (reset! !open? ret)))))))
                      (dom/on "dblclick"
                              (e/fn [e]
                                (.stopPropagation e)))
                      (dom/text (if (and label-after-first-click clicked-once?)
                                  label-after-first-click
                                  label))
                      (when subitems
                        (new anchor)))
                    (when (and subitems (= label open-subitems-label))
                      (dom/div
                        (dom/props {:class ["absolute" :p-1 :mt-1 :bg-white :flex :flex-col
                                            :border :border-black :whitespace-pre]})
                        (e/for [{:as x :keys [label
                                              checked?
                                              on-click]} subitems]
                          (dom/div
                            (dom/props {:class [:relative :gap-1 :flex "hover:bg-slate-200"]})
                            (when (contains? x :checked?)
                              (dom/input
                                (dom/props {:type    "checkbox"
                                            :checked (boolean checked?)})))
                            (dom/on "click"
                                    (e/fn [e]
                                      (let [ret (new on-click e)]
                                        (when (boolean? ret)
                                          (reset! !open? ret)))))
                            (dom/on "dblclick"
                                    (e/fn [e]
                                      (.stopPropagation e)))
                            (dom/text label)))
                        ))))))))))))

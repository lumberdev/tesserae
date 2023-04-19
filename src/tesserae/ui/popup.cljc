(ns tesserae.ui.popup
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [stuffs.dom :as sdom]
            [missionary.core :as m]
            [tesserae.ui.electric-util :as eu]))


(e/defn Menu [{:keys [anchor items]
               :or   {anchor "..."}}]
  (let [[!open? open?] (eu/state false)]
    (dom/div
      (dom/props {:class ["z-[10000]" "text-xs" "relative" :cursor-pointer]})
      (dom/span
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
          (dom/props {:class ["absolute" :p-1 :bg-white :flex :flex-col :gap-1
                              :border :border-black :whitespace-pre]})
          (e/for [{:as x :keys [override
                                label
                                label-after-first-click
                                on-click]} items]
            (let [[!clicked-once? clicked-once?] (eu/state (if label-after-first-click false true))]
              (when x
                (if override
                  (new override)
                  (dom/div
                    (dom/props {:class [:p-1 "hover:bg-slate-200"]})
                    (dom/on "click" (e/fn [e]
                                      (if (and label-after-first-click (not clicked-once?))
                                        (reset! !clicked-once? true)
                                        (let [ret (new on-click e)]
                                          (when (boolean? ret)
                                            (reset! !open? ret))))))
                    (dom/text (if (and label-after-first-click clicked-once?)
                                label-after-first-click
                                label))))))))))))

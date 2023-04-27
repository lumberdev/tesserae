(ns tesserae.ui.panel
  (:require [clojure.string :as str]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [net.cgrand.xforms :as x]
            [stuffs.util :as su]
    #_[tesserae.db :as db]
            [tesserae.ui.sheet :as sheet :include-macros true]))


(e/defn Entrypoint [{:as x :keys [sheet/cells db/id]}]
  (e/client
    (dom/div
      (dom/props
        {:class
         [:flex :flex-wrap :gap-6 "w-full" "h-full" :overflow-y-auto
          :p-3]})
      (e/server
        (e/for-by :db/id [c (sequence
                              (comp
                                (remove (su/rcomp :cell/form-str str/blank?))
                                ;; sort by row then column
                                (x/sort-by (fn [{:cell/keys [x y]}]
                                             (+ (* y 1000)
                                                x))))
                              cells)]
          (e/client
            (dom/div
              (dom/props {:class ["rounded-lg bg-white p-4 text-xs leading-5 shadow-xl shadow-black/5 ring-1 ring-slate-200"]})
              (e/server
                (new sheet/EditableCell c)))))))))
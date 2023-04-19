(ns tesserae.ui.panel
  (:require [clojure.string :as str]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [tesserae.ui.sheet :as sheet :include-macros true]))


(e/defn Entrypoint [{:as x :keys [sheet/cells db/id]}]
  (e/client
    (dom/div
      (dom/props
        {:class
         [:flex :flex-wrap :gap-6 "w-full" "h-full" :overflow-y-auto
          :p-3]})
      (e/server
        (e/for-by :db/id [{:as c :cell/keys [form-str]} cells]
          (when-not (str/blank? form-str)
            (e/client
              (dom/div
                (dom/props {:class ["rounded-lg bg-white p-4 text-xs leading-5 shadow-xl shadow-black/5 ring-1 ring-slate-200"]})
                (e/server
                  (new sheet/EditableCell c))))))))))
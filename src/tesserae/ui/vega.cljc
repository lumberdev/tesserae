(ns tesserae.ui.vega
  (:require
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]
   [missionary.core :as m]
   [stuffs.util :as su]
   [stuffs.js-interop :as j]
   #?@(:clj  []
       :cljs [["vega-embed" :as vega-embed :refer [embed]]])))

#?(:cljs
   (defn render-vega-embed [css-selector spec opts]
     (let [vega-promise
           (vega-embed/embed
            css-selector
            (j/assoc! spec
                      "$schema" "https://vega.github.io/schema/vega-lite/v5.json")
            (clj->js opts))]
       ;; teardown thunk (for m/observe)
       #(.then vega-promise (fn [view] (.finalize view))))))

(e/defn VegaLiteEmbed [json-spec]
  (e/client
   (let [css-id (str (gensym "vegalite"))]
     (dom/div (dom/props {:id css-id}))
     (new
      (m/observe
       (fn mount [!]
         (! nil)
         (render-vega-embed
          (str "#" css-id)
          (su/read-json json-spec)
          {:renderer "svg"}))))
     nil)))

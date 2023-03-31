(ns tesserae.ui.render
  (:require [stuffs.util :as su]))

(defn content-type [x]
  (and (map? x) (::as x)))

(defn tag-map? [x]
  (boolean (and (map? x) (contains? x ::val))))

(defn value [x]
  (or (and (map? x) (::val x))
      x))

(defn as [t v]
  {:pre [(#{:html :hiccup :ui/inc-dec :ui/button} t)]}
  {::as t
   ::val v})

#?(:clj
   (defn ->html-str [x]
     (when-let [as (content-type x)]
       (let [v (::val x)]
         (case as
           :html v
           :hiccup (su/hiccup->html v))))))


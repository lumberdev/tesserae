(ns tesserae.ui.views
  (:require
    [hyperfiddle.electric :as e]
    [spyscope.core]
    [tesserae.ui.sheet :as sh :include-macros true]
    ))

(e/defn App []
  (e/server
    (new sh/Entrypoint)))

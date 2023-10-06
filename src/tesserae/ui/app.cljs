(ns tesserae.ui.app
  (:require [clojure.string :as str]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [tesserae.ui.globals :as g]
            [tesserae.ui.views :as views :include-macros true]
            [missionary.core :as m]
            [stuffs.js-interop :as j]
            [reitit.core :as rr]
            [reitit.coercion :as rc]
            [reitit.frontend.easy :as rfe]
            [reitit.coercion.spec :as rss]))


(def router
  (rr/router
    [["/app"
      ["" :home]
      ["/sheet/:id" {:name       :sheet
                     :parameters {:path {:id int?}}}]
      ["/panel/:id" {:name       :panel
                     :parameters {:path {:id int?}}}]
      ["/cell/:id" {:name       :cell
                    :parameters {:path {:id int?}}}]]]
    {:compile rc/compile-request-coercers
     :data    {:coercion rss/coercion}}))

(defn set-page-title! [route-match]
  (j/assoc! js/document
            :title
            (->> route-match :data :name (str "Tesserae "))))

(e/def re-router
  (->> (m/observe
         (fn [!]
           (rfe/start!
             router
             !
             {:use-fragment false})))
       (m/relieve {})
       new))

(def electric-main
  (e/boot
    (binding [dom/node (dom/by-id "root")]
      (let [{:as match :keys [data query-params path-params]} re-router]
        (binding [g/route-match match
                  g/route       (some-> data :name)]
          (set-page-title! match)
          (new views/App))))))

(defonce reactor nil)

(defn ^:dev/after-load ^:export start []
  (assert (nil? reactor) "reactor already running")
  (set! reactor (electric-main
                  #(js/console.log "Reactor success:" %)
                  #(js/console.error "Reactor failure:" %))))

(defn ^:dev/before-load stop []
  (when reactor (reactor))                                  ; teardown
  (set! reactor nil))

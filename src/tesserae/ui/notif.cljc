(ns tesserae.ui.notif
  (:require [clojure.string :as str]
            [hyperfiddle.electric :as e]
            [stuffs.env :as env :include-macros true]
            [stuffs.util :as su]
            [tesserae.ui.electric-util :as eu]
            [hyperfiddle.electric-dom2 :as dom]
            [stuffs.js-interop :as j]
            [tesserae.ui.globals :as g]
            #?@(:cljs
                [[kitchen-async.promise :as p]
                 [goog.crypt.base64]
                 [lambdaisland.fetch :as fetch]])))

(e/def <notif-permission
  (e/client
    (eu/<await-promise
      (j/call js/Notification :requestPermission))))

#?(:cljs
   (defn register-service-worker [path]
     (if-let [sw (j/get js/navigator :serviceWorker)]
       (j/call sw :register path)
       (do (js/console.log "Could not register service worker")
           (j/call js/Promise :resolve false)))))

#?(:cljs
   (defn new-notif! [{:keys [icon title body]}]
     (let [js-opts (-> {:icon icon
                        :body body}
                       su/remove-nil-vals
                       clj->js)]
       (new js/Notification title js-opts))))

#?(:cljs
   (defn registrations []
     (p/let [sw       (j/get js/navigator :serviceWorker)
             existing (j/call sw :getRegistrations)]
       existing)))

#?(:cljs
   (defn existing-registration [sw-path]
     (p/let [regs (registrations)]
       (su/ffilter
         #(let [{:as sw :keys [state scriptURL]} (j/lookup (j/get % :active))]
            (and
              (= state "activated")
              (str/includes? scriptURL sw-path)))
         regs))))

#?(:cljs
   (defn existing-subscription [sw-path]
     (p/let [matching (existing-registration sw-path)]
       (when matching
         (j/call-in matching [:pushManager :getSubscription])))))

(def server-public-key
  (env/get :web-push-public-key))

#?(:cljs
   (defn install-worker [sw-path]
     (letfn [(handle-sw [st reg]
               (when (= "activated" st)
                 (p/let [pm           (j/get reg :pushManager)
                         #_(js/console.log "pm " pm)
                         #_(js/console.log "server key" (env/get :web-push-public-key))
                         #_(js/console.log "server base64 key" server-public-key)
                         subscription (j/call pm :subscribe
                                              #js {:userVisibleOnly
                                                   true
                                                   :applicationServerKey
                                                   (goog.crypt.base64/decodeStringToUint8Array server-public-key)})
                         #_            (js/console.log "sub" subscription)
                         {:keys [status body]} (fetch/post "/app/notif/sub"
                                                           {:content-type :json
                                                            :accept       :json
                                                            :body         subscription})]
                   (case status
                     200 (new-notif! {:title "Yield"
                                      :body  "Notifications Enabled"})
                     (js/console.error "Could not persist notification to server")
                     )
                   )
                 )

               )]
       (p/try
         (p/let [sw           (j/get js/navigator :serviceWorker)
                 existing     (existing-registration sw-path)
                 _            (when existing (j/call existing :unregister))
                 registration (j/call sw :register sw-path)
                 sw           (or (j/get registration :installing)
                                  (j/get registration :waiting)
                                  (j/get registration :active))]
           (handle-sw (j/get sw :state) registration)
           (j/call sw :addEventListener "statechange"
                   (fn [e]
                     (let [st (j/get-in e [:target :state])]
                       (handle-sw st registration))))
           ;; return truthy value for electric
           :sw-installed!)
         (p/catch js/Error e
           (js/console.error "Could not install service worker" sw-path e))))))

#?(:cljs
   (defn install-service-worker []
     (p/let [perm (j/call js/Notification :requestPermission)]
       (case perm
         "granted" (install-worker "/app/service-worker.js")
         (js/console.log "Could not get permission for service worker:" perm)))))

#?(:cljs
   (defn install-if-needed []
     (p/let [reg (existing-registration "/app/service-worker.js")]
       (when-not reg
         (install-service-worker)))))


(e/defn Toggle []
  (e/server
    ;(when-let [has-subs? (:user/web-push-subs g/user-ent)])
    (e/client
      (let [[!existing-reg existing-reg] (eu/state (eu/<await-promise (existing-registration "/app/service-worker.js")))]
        (dom/div
          (dom/props {:class [:relative :gap-1 :flex "hover:bg-slate-200"]})
          (dom/input
            (dom/props {:type    "checkbox"
                        :checked (boolean existing-reg)})
            (dom/on
              "click"
              (e/fn [_]
                (when-let [ret (eu/<await-promise
                                 (if existing-reg
                                   (j/call existing-reg :unregister)
                                   (install-service-worker)))]
                  (reset! !existing-reg
                          (eu/<await-promise
                            (existing-registration
                              "/app/service-worker.js")))))))
          (dom/text "Notifications on this device"))))))
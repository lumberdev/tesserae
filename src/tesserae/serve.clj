(ns tesserae.serve
  (:require
    [clojure.java.io :as io]
    [datalevin.core :as d]
    [mount.core :as mount :refer [defstate]]
    [org.httpkit.server :as http-kit]
    [tesserae.ui.views]
    [reitit.ring :as r.ring]
    [ring.util.response :as resp]
    [stuffs.util :as su]
    [medley.core :as md]
    [stuffs.env :as env]
    ;; require order matters
    [tesserae.db :as db]
    [tesserae.eval]
    [tesserae.ui.app]
    [hyperfiddle.electric :as e]
    [hyperfiddle.electric-httpkit-adapter :as electric]
    [ring.middleware.resource :refer [wrap-resource]]
    [tesserae.ring.middleware.oauth2 :refer [wrap-oauth2]]
    [ring.middleware.session :refer [wrap-session]]
    [ring.middleware.params :refer [wrap-params]]
    [ring.middleware.defaults :refer [wrap-defaults site-defaults api-defaults]]
    [ring.middleware.content-type :refer [wrap-content-type]]
    [ring.middleware.not-modified :refer [wrap-not-modified]]
    [ring.middleware.session.cookie :as session.cookie]
    [clojure.tools.cli :as tools.cli]
    [org.httpkit.client :as http]
    [tick.core :as t]))

(comment
  ;; google oauth2 impl - not used now for demo purposes
  (def google-user-info-url
    "https://www.googleapis.com/oauth2/v1/userinfo")

  (defn oauth-config []
    {:google
     {:authorize-uri    "https://accounts.google.com/o/oauth2/v2/auth"
      :access-token-uri "https://www.googleapis.com/oauth2/v4/token"
      :client-id        (env/env :google-client-id)
      :client-secret    (env/env :google-client-secret)
      :scopes           ["email"]
      :launch-uri       "/oauth/google" #_"http://localhost:3800/oauth2/google"
      :redirect-uri     "/oauth/google/callback"
      :landing-uri      "/oauth/google/success"}})

  (defn google-get-email [token]
    (some-> @(http/get google-user-info-url {:query-params {:access_token token}})
            :body
            su/read-json-keywordized
            :email))

  (defonce temp-token->email
    (atom {}))

  (defn wrap-user-session [handler]
    ; https://github.com/weavejester/ring-oauth2/issues/27
    (let [temp-secret-k "gwbgzmMg9BuBdLow"]
      (wrap-session
        handler
        {:store
         (session.cookie/cookie-store
           {:key (.getBytes ^String temp-secret-k)
            #_(:session-secret-key env/env)})
         ; https://www.reddit.com/r/webdev/comments/jfk6t8/setting_cookie_expiry_date_always_defaults_to/g9kqnh5/
         :cookie-name  "tesserae-cookie"
         :cookie-attrs {:max-age (t/seconds (t/new-duration 365 :days))}})))

  (defn google-login-success [req]
    (if-let [token (some-> req :oauth2/access-tokens :google :token)]
      (if-let [email (google-get-email token)]
        (do (swap! temp-token->email assoc token email)
            (resp/redirect "/"))
        (resp/redirect "/login"))
      (resp/redirect "/login")))

  (defn wrap-auth-redirect-fn
    [redirect-path]
    (fn [handler]
      (fn [req]
        (if-let [token (some-> req :oauth2/access-tokens :google :token)]
          (if-let [cached-email (get @temp-token->email token)]
            (if (contains? #{"dennis@lumber.dev"
                             "jan@lumber.dev"} cached-email)
              (handler req)
              {:status  401
               :headers {"content-type" "text/html"}
               :body    "Unauthorized."})
            (google-login-success req))
          (resp/redirect redirect-path))))))

(defn document
  [{:as opts :keys [js styles links meta body title]}]
  [:html
   [:head
    #_[:link {:rel "icon" :href "data:image/svg+xml,<svg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 100 100%22><text y=%22.9em%22 font-size=%2290%22>🤘</text></svg>"}]
    (for [m meta]
      [:meta m])
    (for [s styles]
      [:style {:type                    "text/css"
               :dangerouslySetInnerHTML {:__html s}}])
    (for [l links]
      [:link (if (map? l)
               l
               {:rel "stylesheet" :href l})])
    (when title [:title title])]
   [:body
    body
    (for [{:keys [src script]} js]
      [:script
       (if src
         {:src src}
         {:dangerouslySetInnerHTML {:__html script}})])]])

(def css-pulse-shadow
  "
@keyframes shadow-pulse {
    0% {
        box-shadow: 0 0 0 0 orange;
    }

    100% {
        box-shadow: 0 0 5px 10px transparent;
    }


}
")

(def css-pulse-shadow2
  "
@keyframes shadow-pulse-yellow {
    0% {
        box-shadow: 0 0 0 0 rgb(255, 204, 8);
    }

    100% {
        box-shadow: 0 0 5px 10px transparent;
    }
}
")

(defonce demo-mode? (atom false))

(def google-user-info-url
  "https://www.googleapis.com/oauth2/v2/userinfo")

(defn oauth-config []
  {:google
   {:authorize-uri          "https://accounts.google.com/o/oauth2/v2/auth"
    :access-token-uri       "https://oauth2.googleapis.com/token"
    :client-id              (env/env :google-client-id)
    :client-secret          (env/env :google-client-secret)
    ;; provides refresh token
    :extra-authorize-params {:access_type "offline"}
    :scopes                 ["openid" "email" "https://www.googleapis.com/auth/gmail.readonly"]
    :launch-uri             "/oauth/google" #_"http://localhost:3800/oauth2/google"
    :redirect-uri           "/oauth/google/callback"
    :landing-uri            "/oauth/google/success"}})

(defn google-get-email [token]
  (some-> @(http/get google-user-info-url {:oauth-token token})
          :body
          su/read-json-keywordized
          :email))

(defn req->user [req]
  (if @demo-mode?
    (db/entity [:user/email "demo@example.com"])
    (when-let [{:as m :keys [token refresh-token expires]} (some-> req :oauth2/access-tokens :google)]
      (or
        ; existing user
        (some-> (db/entity [:user/oauth2-token token]))
        ; create user
        (when-let [email (google-get-email token)]
          (db/transact-entity! (md/assoc-some {:user/email                email
                                               :user/oauth2-token         token
                                               :user/oauth2-token-expires expires}
                                              :user/oauth2-refresh-token refresh-token))))))
  )

(defn wrap-user-session [handler]
  ; https://github.com/weavejester/ring-oauth2/issues/27
  ;; FXIME not so secret
  (let [temp-secret-k "gwbgtmMg9BuBdLow"]
    (wrap-session
      handler
      {:store
       (session.cookie/cookie-store
         {:key (.getBytes ^String temp-secret-k)
          #_(:session-secret-key env/env)})
       ; https://www.reddit.com/r/webdev/comments/jfk6t8/setting_cookie_expiry_date_always_defaults_to/g9kqnh5/
       :cookie-name  "tessarae-cookie"
       :cookie-attrs {:max-age (t/seconds (t/new-duration 365 :days))}})))



(defn html [{:as opts :keys [js styles links meta body]}]
  [:<>
   [:meta {:charset "UTF-8"}]
   [document opts]])

(defn index [_]
  {:status  200
   :headers {"content-type" "text/html"}
   :body    (su/hiccup->html
              [html
               {:title  "Tesserae by Lumber"
                :meta   [{:name    "viewport"
                          :content "width=device-width, initial-scale=1"}]
                :links  [#_"/css/tachyons.css"
                         {:rel  "manifest"
                          :href "/manifest.json"}]
                :styles [css-pulse-shadow css-pulse-shadow2]
                :js     [{:src "/js/compiled/main.js"}
                         {:script "tesserae.ui.app.start()"}
                         #_{:src "https://cdn.tailwindcss.com"}
                         {:src "/js/tailwind.js"}
                         ]
                :body   [:div#root]}])}
  )

(defn resource-handler [handler]
  (-> handler
      (wrap-resource "public")
      (wrap-content-type)
      (wrap-not-modified)))

(def router
  (r.ring/router
    [["/"
      {#_#_:middleware [(wrap-auth-redirect-fn "/login")]
       :get (fn [req]
              (resp/redirect "/app"))}]
     ["/app*"
      {#_#_:middleware [(wrap-auth-redirect-fn "/login")]
       :get index}
      ["/notif"
       ["/sub"
        {:post (fn [{:as req :keys [body]}]
                 (if-let [user (req->user req)]
                   (when-let [{:as sub :keys [endpoint keys]} (su/read-json-keywordized body)]
                     (tap> sub)
                     (let [ent-sub
                           #:web-push-sub {:endpoint endpoint
                                           :auth     (:auth keys)
                                           :p256dh   (:p256dh keys)}]
                       (db/transact! [{:db/id              (:db/id user)
                                       :user/web-push-subs ent-sub}]))
                     {:status 200})
                   {:status 401}))}]]]
     #_["/login"
        {:get
         (fn [_]
           {:status  200
            :headers {"content-type" "text/html"}
            :body    (su/hiccup->html
                       [html
                        {:styles ["body { font-family: sans-serif}"]
                         :body   [:div [:a {:href "/oauth/google"}
                                        "Login with Google"]]}])})}]
     #_["/oauth"
        ["/google"
         ["/success"
          {:get google-login-success}]]]]))


(defn wrap-tesserae-electric-websocket [next-handler]
  (fn [req]
    (let [user (req->user req)]
      ((electric/wrap-electric-websocket
         next-handler
         (fn boot [_req]
           (e/boot-server {} tesserae.ui.app/Main user)))
       req))))


(defstate handler
  :start
  (r.ring/ring-handler
    router
    (r.ring/create-default-handler)
    {:middleware [#(wrap-defaults %
                                  (-> site-defaults
                                      ;; needed for notif/sub POST
                                      (assoc-in [:security :anti-forgery] false)
                                      (assoc-in [:session :cookie-attrs :same-site] :lax)))
                  wrap-user-session
                  (when-not @demo-mode? #(wrap-oauth2 % (oauth-config)))
                  wrap-tesserae-electric-websocket
                  #_(fn [h]
                      #(h (su/dtap %)))
                  resource-handler]})
  )

(defonce init-opts
  (atom {:port 3900}))

(def cli-opts
  [["-p" "--port PORT" "Port number"
    :default (:port @init-opts)
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]])

(defn default+command-line-args [opts]
  (let [{:keys [options errors]} (tools.cli/parse-opts opts cli-opts)]
    (when errors
      (println errors))
    (swap! init-opts merge options)))

(defstate server
  :start (http-kit/run-server
           handler
           {:port                 (:port @init-opts)
            :legacy-return-value? false})
  :stop (http-kit/server-stop! server))


#_(defstate server
    :start (ring.jetty9/run-jetty
             (fn [req]
               (try
                 (handler req)
                 (catch Throwable t
                   (println ::err t))))
             {:port       (:port @init-opts)
              :join?      false
              :websockets {"/"
                           #_(wrap-user-session)
                           (fn [ring-req]
                             (el-jetty/electric-ws-adapter
                               (fn [write-msg read-msg]
                                 (el-jetty/electric-ws-message-handler ring-req write-msg read-msg))))}})
    :stop (ring.jetty9/stop-server server))

(when env/prod?
  (defonce socket-repl
    (let [port 4460 #_(:socket-port *command-line-args*)]
      (println :starting-repl-with-port port)
      (clojure.core.server/start-server
        {:name          "socket-repl"
         ;; :address is important for docker!E
         :address       "0.0.0.0"
         :port          port
         :accept        'clojure.core.server/repl
         :server-daemon false
         :client-daemon false
         })))
  (comment
    (clojure.core.server/stop-server "socket-repl")))


(defn -main [& opts]
  (let [parsed-opts (default+command-line-args opts)]
    (println "Tesserae booting up...")
    (println "CLI Opts:" (su/pretty-string parsed-opts))
    (mount/start)
    (println "Tesserae is ready.")))

(comment
  (def demo-sheet (d/pull @db/conn '[*] [:dev-id "demo"]))
  (spit "dev/resources/demo-sheet.edn" (su/pretty-string demo-sheet)))

(defn demo [& opts]
  (reset! demo-mode? true)
  (apply -main opts)
  (db/transact! [{:user/email "demo@example.com"}])
  ;; TODO add ademo sheet
  #_(when-not (db/entity [:dev-id "demo"])
      (let [demo-sheet (read-string
                         (slurp "dev/resources/demo-sheet.edn"))]
        (db/transact! [demo-sheet])))
  :demo-ready!)


(comment
  (demo)
  (mount/start)
  (mount/stop)
  (mount/stop #'server)
  (mount/start #'server)
  (mount/start #'db/conn #'server)
  (mount/start-without #'tesserae.eval/eval-schedule-listener)
  (mount/running-states)
  )

(when env/dev?
  (do
    (def shadow-start! @(requiring-resolve 'shadow.cljs.devtools.server/start!))
    (def shadow-stop! @(requiring-resolve 'shadow.cljs.devtools.server/stop!))
    (def shadow-watch @(requiring-resolve 'shadow.cljs.devtools.api/watch))
    (do
      (shadow-start!)
      (shadow-watch :tesserae)))
  (comment
    (shadow-stop!)
    (def shadow-release @(requiring-resolve 'shadow.cljs.devtools.api/release))
    (shadow-release :tesserae))
  )

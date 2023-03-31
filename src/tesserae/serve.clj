(ns tesserae.serve
  (:require
    [datalevin.core :as d]
    [mount.core :as mount :refer [defstate]]
    [ring.adapter.jetty9 :as ring.jetty9]
    [tesserae.ui.sheet]
    [reitit.ring :as r.ring]
    [ring.util.response :as resp]
    [stuffs.util :as su]
    [stuffs.env :as env]
    ;; require order matters
    [tesserae.db :as db]
    [tesserae.eval]
    [hyperfiddle.electric-jetty-adapter :as el-jetty]
    [ring.middleware.resource :refer [wrap-resource]]
    [ring.middleware.oauth2 :refer [wrap-oauth2]]
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
      [:link {:rel "stylesheet" :href l}])
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
                ;:links  ["/css/tachyons.css"]
                :styles [css-pulse-shadow]
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
       :get        index}]
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

(def handler
  (r.ring/ring-handler
    router
    (r.ring/create-default-handler)
    {:middleware [#_default-middleware
                  #(wrap-defaults %
                                  (-> site-defaults
                                      (assoc-in [:session :cookie-attrs :same-site] :lax)))
                  ;wrap-user-session
                  #_(wrap-oauth2 % (oauth-config))

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
         ;; :address is important for docker!
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
  (apply -main opts)
  (when-not (db/entity [:dev-id "demo"])
    (let [demo-sheet (read-string
                       (slurp "dev/resources/demo-sheet.edn"))]
      (db/transact! [demo-sheet]))))

(comment
  (mount/start)
  (mount/stop)
  (mount/stop #'server)
  (mount/start #'server)
  (mount/start #'db/conn #'server)
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
  (comment (shadow-stop!))
  )


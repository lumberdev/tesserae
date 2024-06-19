(ns tesserae.ring.middleware.oauth2
  (:require [org.httpkit.client :as http]
            [clojure.string :as str]
            [crypto.random :as random]
            [stuffs.time :as st]
            [stuffs.util :as su]
            [ring.util.codec :as codec]
            [ring.util.request :as req]
            [ring.util.response :as resp]
            [ring.middleware.oauth2 :as-alias rmo])
  (:import (java.util Date)))

(defn- redirect-uri [profile request]
  (-> (req/request-url request)
      (java.net.URI/create)
      (.resolve (:redirect-uri profile))
      str))

(defn- scopes [profile]
  (str/join " " (map name (:scopes profile))))

(defn- authorize-uri [profile request state]
  #_#_#_(def r request)
          (def p profile)
          (def s state)
  (str (:authorize-uri profile)
       (if (.contains ^String (:authorize-uri profile) "?") "&" "?")
       (let [f (into {:response_type "code"
                      :client_id     (:client-id profile)
                      :redirect_uri  (redirect-uri profile request)
                      :scope         (scopes profile)
                      :state         state}
                     (:extra-authorize-params profile))]
         (codec/form-encode f))))

(defn- random-state []
  (-> (random/base64 9) (str/replace "+" "-") (str/replace "/" "_")))

(defn- make-launch-handler [profile]
  (fn [{:keys [session] :or {session {}} :as request}]
    (let [state (random-state)]
      (-> (resp/redirect (authorize-uri profile request state))
          (assoc :session (assoc session ::rmo/state state))))))

(defn- state-matches? [request]
  (= (get-in request [:session ::rmo/state])
     (get-in request [:query-params "state"])))

(defn- coerce-to-int [n]
  (if (string? n)
    (Integer/parseInt n)
    n))

(defn seconds-from-now-to-date [seconds]
  (-> (Date.)
      .getTime
      (+ (* seconds 1000))
      (Date.)))

(defn- format-access-token
  [{{:keys [access_token expires_in refresh_token id_token] :as body} :body}]
  (-> {:token      access_token
       :extra-data (dissoc body :access_token :expires_in :refresh_token :id_token)}
      (cond-> expires_in (assoc :expires (-> expires_in
                                             coerce-to-int
                                             seconds-from-now-to-date))
        refresh_token (assoc :refresh-token refresh_token)
        id_token (assoc :id-token id_token))))

(defn- get-authorization-code [request]
  (get-in request [:query-params "code"]))

(defn- request-params [{:as profile :keys [id client-id client-secret]} request]
  (let [params (let [{:keys [refresh-token]} (get-in request [:session ::rmo/access-tokens id])]
                 (if refresh-token
                   {:grant_type    "refresh_token"
                    :refresh_token refresh-token
                    :client_id     client-id
                    :client_secret client-secret}
                   {:grant_type   "authorization_code"
                    :code         (get-authorization-code request)
                    :redirect_uri (redirect-uri profile request)}))]
    (into
      params
      (:extra-request-params profile))))

(defn- add-header-credentials [opts id secret]
  (assoc opts :basic-auth [id secret]))

(defn- add-form-credentials [opts id secret]
  (assoc opts :form-params (-> (:form-params opts)
                               (merge {:client_id     id
                                       :client_secret secret}))))

(defn- get-access-token
  [{:keys [access-token-uri client-id client-secret basic-auth?]
    :or   {basic-auth? false} :as profile} request]
  (format-access-token
    (-> @(http/post access-token-uri
                    (cond-> {:form-params (request-params profile request)}
                      basic-auth? (add-header-credentials client-id client-secret)
                      (not basic-auth?) (add-form-credentials client-id client-secret)))
        (update :body su/read-json-keywordized))))

(defn- valid-access-token [{:keys [id] :as profile} request]
  (when-let [{:as access-token-map :keys [expires]} (get-in request [:session ::rmo/access-tokens id])]
    (when (and expires (st/future? expires))
      access-token-map)))

(defn get-or-refresh-access-token
  [profile request]
  #_#_(def pro profile)
          (def req request)
  ;(tap> profile)
  ;(tap> request)
  (or (valid-access-token profile request)
      (get-access-token profile request)))

(defn state-mismatch-handler [_]
  {:status 400, :headers {}, :body "State mismatch"})

(defn no-auth-code-handler [_]
  {:status 400, :headers {}, :body "No authorization code"})

(defn- make-redirect-handler [{:keys [id landing-uri] :as profile}]
  (let [state-mismatch-handler (:state-mismatch-handler
                                 profile state-mismatch-handler)
        no-auth-code-handler   (:no-auth-code-handler
                                 profile no-auth-code-handler)]
    (fn [{:keys [session] :or {session {}} :as request}]
      (cond
        (not (state-matches? request))
        (state-mismatch-handler request)

        (nil? (get-authorization-code request))
        (no-auth-code-handler request)

        :else
        (let [access-token (get-or-refresh-access-token profile request)]
          #_(def rr request)
          (-> (resp/redirect landing-uri)
              (assoc :session (-> session
                                  (assoc-in [::rmo/access-tokens id] access-token)
                                  (dissoc ::state)))))))))

(defn- assoc-access-tokens [request]
  (if-let [tokens (-> request :session ::rmo/access-tokens)]
    (assoc request :oauth2/access-tokens tokens)
    request))

(defn- parse-redirect-url [{:keys [redirect-uri]}]
  (.getPath (java.net.URI. redirect-uri)))

(defn- valid-profile? [{:keys [client-id client-secret] :as profile}]
  (and (some? client-id) (some? client-secret)))

(defn wrap-oauth2 [handler profiles]
  {:pre [(every? valid-profile? (vals profiles))]}
  (let [profiles  (for [[k v] profiles] (assoc v :id k))
        launches  (into {} (map (juxt :launch-uri identity)) profiles)
        redirects (into {} (map (juxt parse-redirect-url identity)) profiles)]
    (fn [{:keys [uri] :as request}]
      (if-let [profile (launches uri)]
        ((make-launch-handler profile) request)
        (if-let [profile (redirects uri)]
          ((:redirect-handler profile (make-redirect-handler profile)) request)
          (handler (assoc-access-tokens request)))))))


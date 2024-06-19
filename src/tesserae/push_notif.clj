(ns tesserae.push-notif
  (:require [clojure.set :as set]
            [stuffs.util :as su]
            [webpush.core :as wp]
            [webpush.utils :as wu]
            [mount.core :as mount :refer [defstate]]
            [tesserae.db :as db]
            [missionary.core :as m]
            [stuffs.env :as env]
            ))

(comment
  (wp/add-security-provider!)
  (wu/generate-keys)
  )

(defstate ^{:on-reload :noop} service
  :start (let [public-k  (env/get :web-push-public-key)
               private-k (env/get :web-push-private-key)]
           (su/ascertain (and public-k private-k)
                         (str "Push notif keys missing!\n"
                              "Add the following to your env and restart the process\n\n"
                              (su/pretty-string
                                (set/rename-keys (wu/generate-keys)
                                                 {:public  :web-push-public-key
                                                  :private :web-push-private-key}))
                              ))
           (wp/add-security-provider!)
           (wp/service
             public-k
             private-k
             "mailto:dennis@lumber.dev")))

;(map :v (db/datoms :ave :user/web-push-subs))

(defn send! [{:web-push-sub/keys [endpoint p256dh auth] :as sub-ent} notif-m]
  (let [;; Create a subscription
        sub   (wp/subscription endpoint p256dh auth)
        ;; Create a push-service instance by passing your `public-key`,
        ;; `private-key` (as strings), and the webpush `subject` (most of the time
        ;; your email)
        notif (wp/notification sub (su/write-json-string notif-m))]
    (wp/send! service notif))
  )

(defn sub-gone? [resp]
  (-> resp .getStatusLine .getStatusCode (= 410)))

(defn send-to-many! [notif-m web-push-ents]
  (when-let [gone-subs (not-empty
                         (sequence
                           (comp (map (juxt :db/id #(send! % notif-m)))
                                 (keep (fn [[eid resp]]
                                         (when (-> resp .getStatusLine .getStatusCode (= 410))
                                           [:db/retractEntity eid]))))
                           web-push-ents))]
    (db/transact! gone-subs)))

(defn send-to-all! [notif-m]
  (send-to-many! notif-m (db/datoms->entities :ave :web-push-sub/auth)))

(comment
  (def reqs (send-to-all! {:title "yssaa me"
                           :body  "hey"})))

(defn cell->notif-m [{:as cell id :db/id :cell/keys [name ret-str]}]
  {:title (str
            (-> (db/entity id) :sheet/_cells :sheet/name) "/" (or name id))
   :body  ret-str
   :icon  "/img/yield-icon512.png"
   :data  {:routeTo (str "/app/cell/" id)}})


(comment
  #_(datalevin.core/touch (rand-nth (db/datoms->entities :ave :cell/name)))
  (send-to-all! (cell->notif-m (db/entity 67))))



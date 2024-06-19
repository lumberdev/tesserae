(ns tesserae.db
  (:require [mount.core :as mount :refer [defstate]]
            [missionary.core :as m]
            [medley.core :as md]
            [datalevin.core :as d]
            [stuffs.datalevin.util :as sdu]
            [stuffs.env :as env]
            [stuffs.util :as su]
            [stuffs.mount :as smount]
            [datalevin.search-utils :as dsu]
            [tick.core :as t]))

(def ^:dynamic *user-ent* nil)

(defn db-dir []
  (or (::dir (mount/args)) "data/tesserae/datalevin/db"))

(def schema
  (merge
    (when env/dev?
      {:dev-id {:db/valueType :db.type/string
                :db/unique    :db.unique/identity}})
    {:user/email                {:db/valueType :db.type/string
                                 :db/unique    :db.unique/identity}
     :user/oauth2-token         {:db/valueType :db.type/string
                                 :db/unique    :db.unique/identity}
     :user/oauth2-refresh-token {:db/valueType :db.type/string
                                 :db/unique    :db.unique/identity}
     :user/oauth2-token-expires {:db/valueType :db.type/instant}

     :user/web-push-subs        {:db/valueType   :db.type/ref
                                 :db/cardinality :db.cardinality/many}
     :web-push-sub/auth         {:db/valueType :db.type/string}
     :web-push-sub/p256dh       {:db/valueType :db.type/string}
     :web-push-sub/endpoint     {:db/valueType :db.type/string}

     :sheet/name                {:db/valueType :db.type/string}
     :sheet/cells               {:db/valueType   :db.type/ref
                                 :db/cardinality :db.cardinality/many
                                 :db/isComponent true}
     :sheet/size                {:db/valueType  :db.type/tuple
                                 :db/tupleAttrs [:sheet/cols-count :sheet/rows-count]}

     :cell/name                 {:db/valueType :db.type/string}
     :cell/form-str             {:db/valueType :db.type/string}
     :cell/ret-str              {:db/valueType :db.type/string}
     :cell/pos                  {:db/valueType  :db.type/tuple
                                 :db/tupleAttrs [:cell/x :cell/y]}
     :cell/refs                 {:db/valueType   :db.type/ref
                                 :db/cardinality :db.cardinality/many}
     :cell/schedule             {:db/valueType   :db.type/ref
                                 :db/cardinality :db.cardinality/one
                                 :db/isComponent true}
     :cell/evaled-at            {:db/valueType :db.type/instant}
     :cell/eval-upon            {:db/valueType   :db.type/keyword
                                 :db/cardinality :db.cardinality/many}
     :cell/notify-on-ret        {:db/valueType   :db.type/ref
                                 :db/cardinality :db.cardinality/many}

     :schedule/text             {:db/valueType :db.type/string}
     :schedule/from             {}
     :schedule/repeat           {}
     :schedule/next             {}}))



(declare
  entity datoms datoms->entities q where-entity where-entities transact! transact-entity!
  get-schema get-rschema update-schema)

(defstate ^{:on-reload :noop} conn
  :start (let [conn
               (d/create-conn (db-dir)
                              schema
                              {:auto-entity-time? true
                               :validate-data?    true #_env/dev?})]
           (def entity
             (sdu/make-entity conn env/dev?))
           (def datoms (sdu/make-datoms conn))
           (def q (sdu/make-q conn))
           (def where-entity (sdu/make-where-entity conn))
           (def where-entities (sdu/make-where-entities conn))
           (def datoms->entities (sdu/make-datoms->entities conn))
           (defn transact!
             ([txs]
              (d/transact! conn txs))
             ([txs tx-meta]
              (d/transact! conn txs tx-meta)))
           (def transact-entity! (sdu/make-transact-entity-simple! conn))
           (defn get-schema [] (-> conn d/schema))
           (defn get-rschema [] (-> @conn sdu/rschema))
           (def update-schema (partial d/update-schema conn))
           conn)
  :stop (d/close conn))

(comment
  (mount/stop #'conn)
  (mount/start #'conn)
  (smount/with-restart ['conn]
    (su/delete-directory-recursive (db-dir))))

(comment
  ;; prod backups
  ;; sheet->edn
  (require '[clojure.walk :as walk])
  (require '[clojure.java.io :as io])
  (def sheet (d/pull @conn
                     '[*]
                     1))



  (:db/id sheet)
  #_(walk/postwalk (fn []) sheet)
  (defn transactable [sheet]
    (walk/postwalk (fn [x]
                     (cond
                       (map-entry? x) (case (key x)
                                        :db/id [:db/id (- (val x))]
                                        (:cell/ret-str :cell/ret) nil
                                        x)
                       :else x)) sheet))



  (stuffs.prepl/remote-eval!
    `(do
       (d/pull-many @conn
                    ["*"]
                    (map :e (datoms :ave :sheet/name)))))

  (.getName (io/file "data/sheet-backups"))
  (defn latest-backup []
    (->> (io/file "data/sheet-backups")
         file-seq
         (remove #(.isDirectory %))
         (mapv io/file)
         (sort-by #(.getName %))
         (last)))

  (stuffs.prepl/remote-eval!
    `(d/clear conn)
    )
  ;; restore local db
  (transact! (read-string (slurp (latest-backup))))

  (stuffs.prepl/remote-eval!
    `~(read-string (slurp (latest-backup))))

  (stuffs.prepl/remote-eval!
    {:foo 2}
    )

  ;; restore remote DB
  (stuffs.prepl/remote-eval!
    `(do (transact!
           ~(read-string (slurp (latest-backup))))
         :done))

  (stuffs.prepl/remote-eval!
    `(transact!
       [(dissoc (second (read-string (slurp (latest-backup))))
                :sheet/cells)]))

  (stuffs.prepl/remote-eval! '(range 10))


  (defn backup-remote []
    (when env/dev?
      (let [sheets   (->> (stuffs.prepl/remote-eval!
                            `(d/pull-many @conn
                                          ["*"]
                                          #_[1 25 85]
                                          (map :e (datoms :ave :sheet/name))))
                          :val
                          (mapv transactable))
            p        "data/sheet-backups/"
            filepath (str p (t/date-time) ".edn")]
        (def sh sheets)
        (io/make-parents filepath)
        (spit filepath (pr-str sheets))
        [::backup-success filepath])))

  (backup-remote)

  )

(comment

  (def test-conn
    (d/create-conn "data/test-conn"
                   schema
                   {:auto-entity-time? true
                    :validate-data?    true #_env/dev?}))

  (d/transact! test-conn [{:foo "bar"}])
  (d/entity @test-conn 1)
  (d/transact! test-conn [{:entity (d/touch (d/entity @test-conn 1))}])
  (d/touch (d/entity @test-conn 3))

  )



(ns tesserae.db
  (:require [mount.core :as mount :refer [defstate]]
            [medley.core :as md]
            [datalevin.core :as d]
            [stuffs.datalevin.util :as sdu]
            [stuffs.env :as env]
            [stuffs.util :as su]
            [stuffs.mount :as smount]
            [datalevin.search-utils :as dsu]))

(defn db-dir []
  (or (::dir (mount/args)) "data/tesserae/datalevin/db"))

(def schema
  (merge
    (when env/dev?
      {:dev-id {:db/valueType :db.type/string
                :db/unique    :db.unique/identity}})
    {:sheet/name                {:db/valueType :db.type/string}
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
     :schedule/text             {:db/valueType :db.type/string}
     :schedule/from             {}
     :schedule/repeat           {}
     :schedule/next             {}}))

(declare
  entity datoms datoms->entities q where-entity where-entities transact! transact-entity!
  get-schema get-rschema update-schema
  )

(defstate ^{:on-reload :noop} conn
  :start (let [conn
               (d/create-conn (db-dir)
                              schema
                              {:auto-entity-time? true
                               :validate-data?    env/dev?})]
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
    (su/delete-directory-recursive (db-dir)))
  )

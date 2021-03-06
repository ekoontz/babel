;; TODO: we don't use much korma functionality here; might as well simply
;; use a clojure wrapper around JDBC.
;; TODO: rename this file - misleading name 'korma.clj'.
;; It uses korma, but it is not itself part of korma.
(ns babel.korma
  (:refer-clojure :exclude [test update])
  (:require [clojure.data.json :as json]
            [clojure.string :as string]
            [clojure.tools.logging :as log]
            [korma.core :refer [exec-raw]]
            [korma.db :refer [default-connection defdb postgres]]))

(require '[environ.core :refer [env]])

(defonce _direct_connection (atom nil))

(declare convert-keys-from-string-to-keyword)

(defn prepare-array
  "Convert a Clojure sequence into (some implementation of) java.sql.Array.

   e.g. convert [{:a 42}] 
        into: #object[org.postgresql.jdbc.PgArray 0x3d73de4b '{'{'a':42}'}']

  Uses an implementation of java.sql.Connection.createArrayOf provided
  by the database driver.  For example, if _direct_connection is to a
  PostgreSQL database via org.postgresql.jdbc, then the implementation
  type will be org.postgresql.jdbc.PgArray
    (https://jdbc.postgresql.org/development/privateapi/org/postgresql/jdbc/PgArray.html).
   An implementation of java.sql.Connection.createArrayOf
    (https://docs.oracle.com/javase/7/docs/api/java/sql/Connection.html)
   will be used to do the actual conversion."

  [sequence]
  (if (nil? @_direct_connection)
    (reset! _direct_connection
            (-> korma.db/_default deref
                :pool deref
                :datasource .getConnection)))
  ;; infer type of input sequence based on first element, if any.
  (let [sql-type-and-map-fn
        (cond (string? (first sequence))
              {:type "text"
               :map-fn (fn [x] x)}
              
              (map? (first sequence))
              {:type "jsonb"
               :map-fn json/write-str}

              ;; TODO: add support integers or other types. On the other hand,
              ;; it seems to work fine to use {:type "text"} for integers, so no need to do
              ;; any additional work for them here.
              
              true
              (do
                (if (not (empty? sequence))
                  (log/debug (str "using default of 'text' as SQL type for"
                                  " sequence of type:" (type sequence)
                                  (str " whose first member is of type:"
                                       (type (first sequence)))
                                  ".")))
                {:type "text"
                 :map-fn (fn [x] x)}))
        sql-type (:type sql-type-and-map-fn)
        map-fn (:map-fn sql-type-and-map-fn)]
    (.createArrayOf
     @_direct_connection
     sql-type (into-array (map map-fn sequence)))))

(defn read-array [sql-array & {:keys [value-fn]
                               :or {value-fn (fn [k v] v)}}]
    "Convert (some implementation of) of java.sql.Array into a Clojure map.

     e.g. turn: #object[org.postgresql.jdbc.PgArray 0x3d73de4b '{{'a':42},{'b':43}}'] 
          into: [{:a 42}{:b 43}]

  Uses an implementation of java.sql.Connection.createArrayOf provided
  by the database driver.  For example, if _direct_connection is to a
  PostgreSQL database via org.postgresql.jdbc, then the implementation
  type will be org.postgresql.jdbc.PgArray
    (https://jdbc.postgresql.org/development/privateapi/org/postgresql/jdbc/PgArray.html).
   An implementation of java.sql.Connection.createArrayOf
    (https://docs.oracle.com/javase/7/docs/api/java/sql/Connection.html)
   will be used to do the actual conversion."

  (if (not (nil? sql-array))
    (->> (-> sql-array ;; (type = implementation of SqlArray (e.g. org.postgresql.jdbc.PgArray)
             .getArray ;; java array: (type %) = #object["Ljava.lang.String;" .."
             vec) ;; clojure.lang.PersistentVector
         
         (map #(try
                 ;; if input elements are parseable as JSON, read them, each of which will be a Clojure map.
                 ;; otherwise, return the input elements unmodified.
                 (json/read-str % :value-fn value-fn) ;; rely on json/read-str to throw an error if input is not JSON
                 (catch Exception e %))) ;; % is not JSON-parseable: just return it as-is.
       
         ;; Input is now a sequence of Clojure-native elements such as maps, but, if an
         ;; element is a map, then its keys are strings.
         ;; Final step, convert the keys from strings e.g. convert "foo" to :foo.
         (map convert-keys-from-string-to-keyword))))

(defn create-tables [db-connection]
  ;; older postgres versions (pre-9.6(?)) do not support CREATE SEQUENCE IF NOT EXISTS,
  ;; so that will fail here: catch the exception and continue.
  (try (exec-raw [(str "CREATE SEQUENCE IF NOT EXISTS lexeme_id_seq")])
       (catch Exception e
         (log/warn (str "ignoring error from database server when creating sequence: " e))))
  (exec-raw [(str "CREATE TABLE IF NOT EXISTS lexeme ("
                  " lexeme BIGINT NOT NULL DEFAULT nextval('lexeme_id_seq'::regclass),"
                  " created TIMESTAMP WITHOUT TIME ZONE DEFAULT now(),"
                  " language text,"
                  " canonical text,"
                  " structures jsonb[],"
                  " serialized text[])")
             []]))

(defn init-db [& [url]]
  (do
    (defdb korma-db 
      (let [default "postgres://localhost/babel"
            database-url (cond
                           url (do
                                 (log/info (str "using supplied url:" url))
                                 url)

                           (env :database-url)
                           (do (log/info (str "using environment's DATABASE_URL:"
                                              (env :database-url)))
                               (env :database-url))

                           true (do (log/info
                                     (str "using default database url:"
                                          url))
                                    default))]
        (log/info (str "initializing database connection with database-url: "
                       database-url))
        
        ;; this constructs the actual database connection which is used throughout the code base.
        (postgres
         ;; thanks to Jeroen van Dijk via http://stackoverflow.com/a/14625874
         (let [[_ user password host port db]
               (re-matches #"postgres://(?:([^:]+):?(.*)@)?([^:]+)(?::(\d+))?/(\S+).*"
                           database-url)
               
               redacted-database-url
               (if (and password (not (empty? password)))
                 (string/replace database-url
                                 (str ":" password)
                                 ":******")
                 database-url)
               ]
           (if (nil? db)
             (throw (Exception. (str "could not find database name in your database-url: '"
                                     database-url "'"))))
           
           (log/info (str "scanned DATABASE_URL:" redacted-database-url "; found:"
                          "(user,host,db)=(" user "," host "," db ")"))
           (merge
            {:db db
             :password password
             :host host
             :port (or port "5432")}
            (if user
              {:user user}
              {}))))))))

(defn convert-keys-from-string-to-keyword [input]
  (cond (map? input)
        (let [keys (map keyword
                        (keys input))
              vals (map convert-keys-from-string-to-keyword
                        (vals input))]
          (zipmap keys vals))
        true input))


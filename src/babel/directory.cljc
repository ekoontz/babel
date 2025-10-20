(ns babel.directory
  (:require [babel.english.grammar :as en]
            [babel.espanol.grammar :as es]
            [babel.francais.grammar :as fr]
            [babel.italiano.grammar :as it]
            [babel.italiano.lexicon :as it-lex]
            [babel.korma :as db]
            [babel.latin :as la]
            [babel.lexiconfn :refer [write-lexicon]]
            [clojure.tools.logging :as log]
            [dag_unify.core :as u]
            [korma.db :refer [transaction]]))

;; babel.directory provides a centralized way to discover and access language models.
;; 
;; To sync your language model after changing lexicon sources:
;;
;; 1. Write .edn files to database:
;;     (write-lexicon "it" (babel.italiano.lexicon/compile-lexicon))
;; 2. Update in-memory models from database:
;;    (babel.directory/refresh-models)
;; 3. Make model easily available within your own namespace:
;;    (def model @@(get models :it))
;; 4. Use new model via your local variable 'model':
;;    (first (get (get model :lexicon) "uomo"))

(def models
  {:en (atom (delay (en/model)))
   :es (atom (delay (es/small)))
   :fr (atom (delay (fr/model)))
   :it (atom (delay (it/model)))
   :la (atom (delay (la/model)))})

(defn refresh-models []
  (do
    (log/info (str "refreshing models.."))
    (swap! (:en models) (fn [old-model] (delay (en/model))))
    (swap! (:es models) (fn [old-model] (delay (es/small))))
    (swap! (:fr models) (fn [old-model] (delay (fr/model))))
    (swap! (:it models) (fn [old-model] (delay (it/model))))
    ;; (swap! (:it models) (fn [old-model] (delay (grammar/model-reloaded))))

    (swap! (:la models) (fn [old-model] (delay (la/model))))
    (log/info (str "refreshed models."))))

(defn write-lexicons []
  (db/init-db)
  (transaction
   
   (print (str "en.."))
   (println (write-lexicon "en" (en/compile-lexicon)))
   
   (print (str "es.."))
   (println (write-lexicon "es" (es/compile-lexicon)))
   
   (print (str "fr.."))
   (println (write-lexicon "fr" (fr/compile-lexicon)))

   (print (str "it.."))
   (println (write-lexicon "it" (it-lex/compile-lexicon)))))

(defn create-model-with-vocab-items
  "Create source and target language models from a set of vocab items. The model can then be used to generate or parse expressions."
  [target-language source-language target-vocab-items source-vocab-items generative-features]
  (let [{definite-articles? :definite-articles?
         possessive-articles? :possessive-articles?
         adjectives? :adjectives?} generative-features]
    (log/info (str "create-model: target-vocab-items count:" (count target-vocab-items) " and generative-feature switches: " generative-features))
    (let [target-model @@(get models target-language)
          source-model @@(get models source-language)
          filter-lexicon-fn
          (fn [lexeme]
            (or
             (and adjectives?
                  (= :adjective (u/get-in lexeme [:synsem :cat])))
             (and (= :det (u/get-in lexeme [:synsem :cat]))
                  (or (and definite-articles?
                           (= :def (u/get-in lexeme [:synsem :def]))
                           (= :definite (u/get-in lexeme [:synsem :sem :pred]))
                           (= nil (u/get-in lexeme [:synsem :sem :of :pred])))
                      (and possessive-articles?
                           (= :possessive (u/get-in lexeme [:synsem :def])))))))
          target-model ((:vocab2model target-model) target-vocab-items filter-lexicon-fn)
          source-model ((:vocab2model source-model) source-vocab-items filter-lexicon-fn)]
      (log/info (str "create-model-with-vocab-items: target-vocab-items: " (vec target-vocab-items)))
      (log/info (str "create-model-with-vocab-items: source-vocab-items: " (vec source-vocab-items)))
      {:source source-model
       :target target-model})))

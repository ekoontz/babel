(ns babel.test.translate
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.directory :refer [models]]
   [babel.generate :refer [generate]]
   [babel.test.it :as it]
   [babel.test.en :as en]
   [babel.test.test :as btest]
   [clojure.test :refer [deftest is]]
   [clojure.tools.logging :as log]
   [dag_unify.core :refer [get-in strip-refs]]))

;; In Italian, certain verbs, called "essere" verbs, when conjugated
;; in certain tenses, agree in gender and number with their subject:
;;
;; For example, compare:
;;  Italian: "loro sono andati"
;;  English: They (masculine plural) went.
;; but:
;;  Italian: "loro sono andate"
;;  English: They (feminine plural) went.
;;
;; In English we indicate feminine and masculine gender with ♀ and ♂,
;; respectively.

(btest/init-db)
(def italiano-model @@(get models :it))
(def english-model @@(get models :en))
(def latin-model @@(get models :la))

;; Test that gender agreement is correctly translated.
(deftest past-and-gender-agreement-feminine
  (let [italian "loro sono andate"

        italian-structure
        (-> italian
            (babel.italiano/parse italiano-model)
            first
            :parses
            first)
         
        semantics
        (-> italian-structure
            (get-in [:synsem :sem])
            strip-refs)
        
        english-structure
        (->  {:synsem {:sem semantics}}
             (generate english-model))

        english (babel.english/morph english-structure :model english-model)]

    (= "they (♀) went" english)))

(deftest past-and-gender-agreement-masculine
  (let [italian "loro sono andati"

        italian-structure
        (-> italian
            (babel.italiano/parse italiano-model)
            first
            :parses
            first)
         
        semantics
        (-> italian-structure
            (get-in [:synsem :sem])
            strip-refs)
        
        english-structure
        (->  {:synsem {:sem semantics}}
             (generate english-model))

        english (babel.english/morph english-structure :model english-model)]

    (= "they (♂) went" english)))

(deftest latin-to-english
  (let [latin "ardebam"
        latin-structure
        (-> latin
            (babel.latin/parse latin-model)
            first
            :parses
            first)
         
        semantics
        (-> latin-structure
            (get-in [:synsem :sem])
            strip-refs)
        
        english-structure
        (->  {:comp {:synsem {:agr (get-in latin-structure [:synsem :agr])}}
              :slash false ;; TODO: {:slash false,:synsem {:subcat '()}} should be defaults of English language model.
              :synsem {:sem semantics
                       :cat :verb
                       :subcat []}}
             (generate english-model))
        
        english (babel.english/morph english-structure
                                     :model english-model
                                     :show-notes false)]
    
    (log/debug (str "babel.translate/latin-to-english: english-structure" english-structure))
    (log/debug (str "babel.translate/latin-to-english: english:" english))
    (is (or (= "I was burning" english)
            (= "I used to burn" english)))))



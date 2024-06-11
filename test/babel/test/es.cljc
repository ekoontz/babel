(ns babel.test.es
  (:refer-clojure :exclude [get-in])
  (:require [babel.directory :refer [models]]
            [babel.espanol :as espanol :refer [analyze generate parse small]]
            [babel.espanol.grammar :as grammar]
            [babel.espanol.morphology :refer [fo]]
            [babel.lexiconfn :refer [write-lexicon]]
            [babel.test.test :as btest]
            [clojure.repl :refer [doc]]
            [clojure.string :as string]
            #?(:clj [clojure.test :refer [deftest is]])
            #?(:cljs [cljs.test :refer-macros [deftest is]])
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [babel.logjs :as log]) 
            [dag_unify.core :refer [get-in]]))

(btest/init-db)
(write-lexicon "es" (grammar/compile-lexicon))
(def model @@(get models :es))

(deftest generate-regular-conditional
  (let [result (generate {:synsem {:subcat '()
                                   :cat :verb
                                   :sem {:pred :sleep
                                         :subj {:pred :I}
                                         :tense :conditional}}}
                         model)]
    (is (= :1st (get-in result [:comp :synsem :agr :person])))
    (is (= :sing (get-in result [:comp :synsem :agr :number])))
    (is (or (= "yo dormiría" (fo result))
            (= "dormiría" (fo result))))))

(deftest generate-irregular-future
  (let [result (fo (generate {:synsem {:cat :verb
                                       :subcat '()
                                       :sem {:tense :future
                                             :subj {:pred :I}}}
                              :root {:espanol {:espanol "venir"}}}
                             model))]
    (is (or (= result
               "yo vendré")
            (= result
               "vendré")))))

(deftest zar-preterito
  (let [result (generate 
                {:root {:espanol {:espanol "abrazar"}}
                 :synsem {:cat :verb
                          :subcat '()
                          :sem {:subj {:pred :I}}
                          :infl :preterito}}
                model)]
    (is (or (= "yo abracé" (fo result))
            (= "abracé" (fo result))))))
                
(deftest llamarse
  (let [result (generate {:synsem {:subcat '() :cat :verb :sem {:pred :be-called}}} model)]
    (is (not (empty? (fo result))))))

(deftest llamo
  (let [spec {:synsem {:subcat '()
                       :cat :verb
                       :sem {:tense :present
                             :aspect :simple
                             :subj {:pred :I}
                             :pred :be-called
                             :obj {:pred :Juan}}}}
        result1 (generate spec model)
        result (fo result1)]
    (log/info (str "result1: " result1))
    (is (or (= result
               "yo me llamo Juan")
            (= result "me llamo Juan")))))

    

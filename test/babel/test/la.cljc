(ns babel.test.la
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.directory :refer [models]]
   [babel.english :as source]
   [babel.latin.morphology :refer [analyze conjugate]]
   [babel.latin :as target :refer [morph read-one]]
   [babel.test.test :as btest]
   [clojure.repl :refer [doc]]
   [clojure.test :refer [deftest is]]
   [clojure.tools.logging :as log]
   [dag_unify.core :refer [get-in strip-refs unify]]))

(btest/init-db)
(def source-language :en)
(def model @@(get models :la))

(defn generate [spec]
  ((-> model :generate-fn) spec))

;; https://en.wikipedia.org/wiki/Latin_conjugation#Present_indicative
(deftest analyze-ere
  (let [lexicon (:lexicon model)]
    (is (= :verb
           (-> (analyze "ardeo" lexicon)
               first
               (get-in [:synsem :cat]))))))

(deftest conjugate-ere
  (is (= "ardemus"
         (conjugate "ardēre"
                    {:synsem {:agr {:person :1st :number :plur}}}))))

(deftest generate-present
  (is (= "ardetis"
         (morph (generate
                 {:root "ardēre"
                  :synsem {:agr {:person :2nd :number :plur}
                           :sem {:tense :present}}})))))

(deftest generate-imperfect
  (is (= "ardebam"
         (morph (generate
                 {:root "ardēre"
                  :synsem {:agr {:person :1st :number :sing}
                           :sem {:tense :past
                              :aspect :progressive}}})))))
(deftest generate-future
  (is (= "ardebunt"
         (morph (generate
                 {:root "ardēre"
                  :synsem {:agr {:person :3rd :number :plur}
                           :sem {:tense :future}}})))))

(deftest reader2
  (let [source-model @@(-> (-> models :en))

        ;; use a specific :root and verb conjugation so that we can test
        ;; for specific literal strings in the result.
        results (take 10 (repeatedly #(read-one {:root "ardēre"
                                                 :synsem {:sem {:tense :past
                                                                :aspect :progressive}}}
                                                model source-model)))]
    (doall
     (->> results
          (map (fn [result]
                 (let [possible-answer (first (shuffle (get result :targets)))]
                   (log/debug (str "reader2 possible-answer:" possible-answer))
                   (is
                    (or
                     (and
                      (= "ardebam"
                         possible-answer))
                     (and
                      (= "ardebas"
                         possible-answer))
                     (and
                      (= "ardebamus"
                         possible-answer))
                     (and
                      (= "ardebatis"
                         possible-answer))
                     (and
                      (= "ardebat"
                         possible-answer))
                     (and
                      (= "ardebant"
                         possible-answer))
                     (or
                      (log/info (str "failsafe: result:" result))
                      (log/info (str "failsafe: possible-answer:" possible-answer))
                      false))))))))))

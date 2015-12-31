(ns babel.english.pos
  (:refer-clojure :exclude [get-in]))

(require '[babel.english.morphology :refer (fo)])
(require '[babel.lexiconfn :as lexiconfn :refer (map-function-on-map-vals)])
(require '[babel.pos :as pos])
(require '[clojure.tools.logging :as log])
(require '[dag_unify.core :as unify :refer (dissoc-paths get-in serialize unifyc)])

(def adjective pos/adjective)
(def animal pos/common-noun)
(def common-noun pos/common-noun)
(def countable-noun pos/common-noun)

(def agreement-noun
  (let [agr (ref :top)]
    {:english {:agr agr}
     :synsem {:agr agr}}))

;; A generalization of intransitive and transitive:
;; they both have a subject, thus "subjective".
(def verb-subjective
  (unifyc pos/verb-subjective
          (let [infl (ref :top)
                agr (ref :top)]
            {:english {:agr agr
                       :infl infl}
             :synsem {:infl infl
                      :subcat {:1 {:agr agr}}}})))

(def transitive
  (unifyc verb-subjective
          pos/transitive))

(def intransitive-unspecified-obj
  (unifyc verb-subjective
          pos/intransitive-unspecified-obj))

(def intransitive
  (unifyc verb-subjective
          pos/intransitive))

(defn intransitivize [lexicon]
  (lexiconfn/intransitivize lexicon intransitive transitive intransitive-unspecified-obj))

(defn transitivize [lexicon]
  (lexiconfn/transitivize lexicon transitive verb-subjective))

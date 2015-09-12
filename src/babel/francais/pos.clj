(ns babel.francais.pos)

(require '[babel.pos :as pos])
(require '[dag-unify.core :refer (unifyc)])
(require '[babel.lexiconfn :as lexiconfn :refer (map-function-on-map-vals)])

(def verb-aux
  (let [sem (ref {:tense :passe-compose})]
    (unifyc {:synsem {:sem sem
                      :subcat {:2 {:infl :past-p}}}}
            (let [aux (ref true)
                  pred (ref :top)
                  sem (ref {:pred pred})
                  subject (ref :top)]
              {:synsem {:aux aux
                        :sem sem
                        :subcat {:1 subject
                                 :2 {:cat :verb
                                     :aux false
                                     :subcat {:1 subject}
                                     :sem sem}}}}))))

(def agreement-noun pos/agreement-noun)
(def cat-of-pronoun pos/cat-of-pronoun)
(def common-noun pos/common-noun)
(def determiner pos/determiner)

(def noun-agreement
  (let [agr (ref :top)]
    {:français {:agr agr}
     :synsem {:agr agr}}))

(def feminine-noun (unifyc
                    noun-agreement (:feminine pos/noun)))

(def masculine-noun (unifyc
                     noun-agreement (:masculine pos/noun)))

(def adjective
  (unifyc pos/adjective
          (let [agr (ref :top)
                cat (ref :top)]
            {:français {:agr agr
                        :cat cat}
             :synsem {:agr agr
                      :cat cat}})))

;; A generalization of intransitive and transitive:
;; they both have a subject, thus "subjective".
(def verb-subjective
  (unifyc pos/verb-subjective
          (let [infl (ref :top)
                agr (ref :top)
                essere-type (ref :top)]
            {:français {:agr agr
                        :essere essere-type
                        :infl infl}
             :synsem {:infl infl
                      :essere essere-type
                      :subcat {:1 {:agr agr}}}})))
(def transitive
  (unifyc verb-subjective
          pos/transitive
          {:synsem {:essere false}}))

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

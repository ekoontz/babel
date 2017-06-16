(ns babel.francais.lexicon
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.francais.morphology :refer [exception-generator phonize]]
   [babel.francais.pos :refer [gender-pronoun-agreement intransitivize
                               transitivize verb-aux]]
   [babel.lexiconfn :as lexiconfn :refer [compile-lex default if-then
                                          map-function-on-map-vals]]
   [clojure.java.io :refer [reader resource]]
   [babel.pos :as pos :refer [pronoun-acc]]
   [dag_unify.core :refer [get-in unify]]))


;; TODO: create lexicon compilation rules for:
;(let [verb-aux
;  (let [sem (atom {:aspect :perfect
;                   :tense :past})
;        subject (atom :top)]
;    {:synsem {:sem sem
 ;             :aux true
  ;            :subcat {:1 subject
  ;;                     :2 {:infl :past-p
  ;                         :sem sem
  ;                         :cat :verb
  ;                         :aux false
  ;                         :subcat {:1 subject}}}}});
;
;;      gender-pronoun-agreement
 ;     (let [gender (atom :top)]
 ;       {:synsem {:cat :noun
  ;                :pronoun true
   ;               :agr {:gender gender}
   ;               :sem {:gender gender}
   ;               :subcat '()}})]

;; TODO: use lexiconfn/edn2lexicon
(declare edn2lexicon)

(def lexicon (promise))
(defn deliver-lexicon []
  (if (not (realized? lexicon))
    (deliver lexicon (edn2lexicon (resource "babel/francais/lexicon.edn")))))

(defn edn2lexicon [resource]
  (-> (lexiconfn/edn2lexicon resource)
      (compile-lex exception-generator phonize)


      (default {:gender-pronoun-agreement false
                :synsem {:cat :unspecified-should-be-fixed}})

      ;; TODO: put more pronoun agreement stuff here
      (default {:gender-pronoun-agreement true
                :synsem {:pronoun true}})

      ;; all pronouns are nouns
      (default {:synsem {:cat :noun
                         :pronoun true}})
      
      ;; make an intransitive version of every verb which has a path [:sem :obj].
      intransitivize

      ;; if verb does specify a [:sem :obj], then fill it in with subcat info.
      transitivize

      (if-then {:synsem {:cat :verb
                         :subcat {:3 '()}}}
               {:synsem {:subcat {:3 '()}}})

      ;; if object is not specified, then set to :unspec.
      ;; this prevents translations that may have actual objects - e.g. would allow translations like:
      ;; "Je mange" => "I eat the bread" whereas a better translation is "I eat".
      (if-then {:synsem {:cat :verb
                         :aux false
                         :sem {:obj :unspec
                               :reflexive false
                               }}}
               {:synsem {:sem {:obj :unspec}}})

      ;; default: reflexive=false.
      (if-then {:synsem {:cat :verb
                         :aux false
                         :sem {:reflexive false}}}
               {:synsem {:sem {:reflexive false}}})
      
      ;; TODO: use lexiconfn/if-then here, like espanol/lexicon does.
      ;; default: essere=false
      (map-function-on-map-vals
       (fn [k vals]
         (map (fn [val]
                ;; if: 1. the val's :cat is :verb
                ;;     2. it is not true that essere=true (either essere=false or essere is not defined)
                ;; then: essere=false
                (cond (and (= (get-in val [:synsem :cat])
                              :verb)
                           (not (= true (get-in val [:synsem :essere] false))))
                      (unify val {:synsem {:essere false}})

                      true ;; otherwise, leave the verb alone
                      val))
              vals)))

      ;; Cleanup functions can go here. Number them for ease of reading.
      ;; 1. this filters out any verbs without an inflection:
      ;; infinitive verbs should have inflection ':infinitive',
      ;; rather than not having any inflection.
      (map-function-on-map-vals
       (fn [k vals]
         (filter #(or (not (= :verb (get-in % [:synsem :cat])))
                      (not (= :none (get-in % [:synsem :infl] :none))))
                 vals)))))

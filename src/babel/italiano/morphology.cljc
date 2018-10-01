(ns babel.italiano.morphology
  (:refer-clojure :exclude [get-in resolve])
  (:require
   [babel.pos :refer (noun)]
   [babel.italiano.morphology.adjectives :as adjectives]
   [babel.italiano.morphology.determiners :as determiners]
   [babel.italiano.morphology.misc :as misc]
   [babel.italiano.morphology.nouns :as nouns]
   [babel.italiano.morphology.verbs :as verbs]
   [babel.morphology :as language-independent]
   [babel.stringutils :refer (replace-from-list)]
   [clojure.string :as string]
   [clojure.string :refer (trim)]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [dag_unify.core :as u :refer (copy dissoc-paths fail? get-in ref? strip-refs unify)]))

;; TODO: move all patterns into here eventually
;;   (preposition-plus-article, adjectives/patterns,etc).
(defonce patterns
  (map :g
       (misc/compile-morphology)))

;; TODO: move this to morphology/prepositions.edn,
;; following example in morphology/determiners.edn.
(defonce preposition-plus-article
  [["a il"   "al"]
   ["a lo"   "allo"]
   ["a la"   "alla"]
   ["a l'"   "all'"]
   ["a i"    "ai"]
   ["a gli"  "agli"]
   ["a le"   "alle"]
   
   ["da il"  "dal"]
   ["da lo"  "dallo"]
   ["da la"  "dalla"]
   ["da l'"  "dall'"]
   ["da i"   "dai"]
   ["da gli" "dagli"]
   ["da le"  "dalle"]

   ["de il"  "del"]
   ["de lo"  "dello"]
   ["de la"  "della"]
   ["de l'"  "dell'"]
   ["de i"   "dei"]
   ["de gli" "degli"]
   ["de le"  "delle"]

   ["di il"  "del"]
   ["di lo"  "dello"]
   ["di la"  "della"]
   ["di l'"  "dell'"]
   ["di i"   "dei"]
   ["di gli" "degli"]
   ["di le"  "delle"]

   ["in il"  "nel"]
   ["in lo"  "nello"]
   ["in la"  "nella"]
   ["in l'"  "nell'"]
   ["in i"   "nei"]
   ["in gli" "negli"]
   ["in le"  "nelle"]

   ["su il"  "sul"]
   ["su lo"  "sullo"]
   ["su la"  "sulla"]
   ["su l'"  "sull'"]
   ["su i"   "sui"]
   ["su gli" "sugli"]
   ["su le"  "sulle"]
   ])

;; TODO: pre-compile these rules rather than building regexp objects at runtime.
(defn apply-one-rule [string from-to-pair]
  (let [from (second from-to-pair)
        to (first from-to-pair)]
    (let [from-pattern (re-pattern
                        (str "\\b" from "\\b"))]
      (string/replace string from-pattern to))))

(defn replace-over [strings]
  ;; TODO: use mapcat rather than (reduce concat) for speed.
  (let [result (set (reduce concat
                            (map (fn [string]
                                   (map #(apply-one-rule string %)
                                        preposition-plus-article))
                                 strings)))]
    (if (not (= result strings))
      (replace-over result)
      strings)))

(defn tokenize-prepositions-in [string & [match-pairs]]
  string)

;; analysis-patterns are declarative data that determine how analysis (inflected form ->root form)
;; and conjugation (root form -> inflected form) are performed.
(defonce analysis-patterns
  (concat
   adjectives/patterns
   determiners/patterns
   nouns/patterns
   verbs/patterns))

(defonce ppa-tokens-to-surface
  (map (fn [pair]
         [(re-pattern
           (str "\\b" (first pair) "\\b"))
          (second pair)])
       preposition-plus-article))

(defonce ppa-surface-to-tokens
  (map (fn [pair]
         [(re-pattern
           (str "\\b" (second pair) "\\b"))
          (first pair)])
       preposition-plus-article))

(defn conjugate-italian-prep [prep np]
  (let [concat (str (get prep :italiano)
                    " "
                    (get np :italiano))]
    (replace-from-list
     preposition-plus-article
     concat)))

(defn analyze-regular [surface-form lexicon]
  "do regular (i.e. non-exceptional) morphological analysis to determine lexical information for a conjugated surface-form, using the (defonce analysis-patterns) defined above."
  (language-independent/analyze surface-form lexicon analysis-patterns))

(declare analyze-capitalization-variant)

(defn analyze
  "take the union of: 
      - analyzing _surface-form_ according to the (defonce analysis-patterns) above
      - looking up _surface-form_ in the supplied lexicon."
  [surface-form lexicon]
  (mapcat (fn [each-variant]
            (analyze-capitalization-variant each-variant lexicon))
          (set
           (list
            surface-form
            (string/capitalize surface-form)
            (string/capitalize (string/lower-case surface-form))
            (string/upper-case surface-form)
            (string/lower-case surface-form)
            (string/join " " (map #(if (not (= "e" %))
                                     (string/capitalize %)
                                     %)
                                  (string/split surface-form #"[ ]")))))))

(defn analyze-capitalization-variant [surface-form lexicon]
  "return an array of the maps, each of which represents the lexical information about a surface form."
  (concat
   (analyze-regular surface-form lexicon)

   ;; make canonical input forms fully inflected:
   (map (fn [lexeme]
          (cond (and (= :verb (get-in lexeme [:synsem :cat]))
                     (= :top (get-in lexeme [:synsem :infl])))
                ;; if a verb has no infl, it's :infinitive.
                (unify lexeme
                       {:synsem {:infl :infinitive}})

                (and (= :noun (get-in lexeme [:synsem :cat]))
                     (= :top (get-in lexeme [:synsem :agr :number])))
                ;; if a noun has no number, it's singular.
                (unify lexeme
                       {:synsem {:agr {:number :sing}}})
                true
                lexeme))
        (get lexicon surface-form))))

(defonce exceptions-rules
  (concat verbs/exceptions-rules
          adjectives/exceptions-rules
          nouns/exceptions-rules))

;; TODO: move this to babel.italiano.lexicon, since it is part of lexicon compilation
(defn exception-generator [lexicon]
  (->>
   (sort (keys lexicon))
   (mapcat (fn [k]
             (let [lexemes (get lexicon k)
                   lexeme-kv [k lexemes]]
               (->> exceptions-rules
                    (mapcat (fn [{path :path
                                  label :label
                                  surface-form :surface-form
                                  merge-fn :merge-fn}]
                              (let [surface-form-fn (or surface-form
                                                        (fn [lexeme]
                                                          (get-in lexeme path :none)))]
                                ;; a lexeme-kv is a pair of a key and value. The key is a string (the word's surface form)
                                ;; and the value is a list of lexemes for that string.
                                (->> lexemes
                                     (mapcat (fn [lexeme]
                                               (if (not (= :none (get-in lexeme path :none)))
                                                 (do (log/debug (str (first lexeme-kv) " generating lexeme exceptional surface form: " (surface-form-fn lexeme)))
                                                     (list {(surface-form-fn lexeme)
                                                            [(reduce
                                                              (fn [a b]
                                                                (cond
                                                                  (or (= a :fail)
                                                                      (= b :fail))
                                                                  :fail
                                                                  true
                                                                  (unify a b)))
                                                              [(dissoc-paths lexeme [[:italiano :italiano]])
                                                               (merge-fn lexeme)
                                                               {:italiano {:infinitive k
                                                                           :exception true}}])]})))))))))))))))
(defn phonize2 [lexicon]
  (into {}
        (for [[k vals] lexicon]
          [k 
           (map (fn [v]
                  (unify v
                         {:italiano {:italiano k}}))
                vals)])))

(def rules
  (reduce concat
          [(->> (-> (str "babel/italiano/morphology/verbs/conditional.edn")
                    clojure.java.io/resource
                    slurp
                    read-string)
                (map (fn [rule]
                       {:g (:g rule)
                        :p (:p rule)
                        :u {:agr (:agr rule)
                            :cat :verb
                            :infl :conditional}})))

           (->> (-> (str "babel/italiano/morphology/verbs/future.edn")
                    clojure.java.io/resource
                    slurp
                    read-string)
                (map (fn [rule]
                       {:g (:g rule)
                        :p (:p rule)
                        :u {:agr (:agr rule)
                            :cat :verb
                            :infl :future}})))

           (->> (-> (str "babel/italiano/morphology/verbs/gerund.edn")
                    clojure.java.io/resource
                    slurp
                    read-string)
                (map (fn [rule]
                       {:g (:g rule)
                        :p (:p rule)
                        :u {:agr (:agr rule)
                            :cat :verb
                            :infl :gerund}})))
           
           (->> (-> (str "babel/italiano/morphology/verbs/imperfetto.edn")
                    clojure.java.io/resource
                    slurp
                    read-string)
                (map (fn [rule]
                       {:g (:g rule)
                        :p (:p rule)
                        :u {:agr (:agr rule)
                            :cat :verb
                            :infl :imperfetto}})))

           (->> (-> (str "babel/italiano/morphology/verbs/passato.edn")
                    clojure.java.io/resource
                    slurp
                    read-string)
                (map (fn [rule]
                       {:g (:g rule)
                        :p (:p rule)
                        :u {:agr (:agr rule :top)
                            :essere (u/get-in rule [:u :essere] false)
                            :cat :verb
                            :infl :passato}})))

           (->> (-> (str "babel/italiano/morphology/verbs/present.edn")
                    clojure.java.io/resource
                    slurp
                    read-string)
                (map (fn [rule]
                       {:g (:g rule)
                        :p (:p rule)
                        :boot-verb (:boot-verb rule false)
                        :u {:agr (:agr rule)
                            :cat :verb
                            :infl :present}})))

           (->> (-> (str "babel/italiano/morphology/verbs/subjunctive.edn")
                    clojure.java.io/resource
                    slurp
                    read-string)
                (map (fn [rule]
                       {:g (:g rule)
                        :p (:p rule)
                        :boot-verb (:boot-verb rule false)
                        :u {:agr (:agr rule)
                            :cat :verb
                            :infl :subjunctive}})))]))

(defn find-matching-pair [input from-to-pairs]
  (if (not (empty? from-to-pairs))
    (let [[pattern-from pattern-to] from-to-pairs]
      (if (re-matches pattern-from input)
        (cons
         (string/replace input pattern-from pattern-to)
         (find-matching-pair input (rest (rest from-to-pairs))))
        (find-matching-pair input (rest (rest from-to-pairs)))))))

(declare irregular-conditional?)
(declare irregular-conditional)
(declare irregular-future?)
(declare irregular-future)
(declare irregular-gerund?)
(declare irregular-gerund)
(declare irregular-passato?)
(declare irregular-passato)
(declare irregular-present?)
(declare irregular-imperfetto?)
(declare irregular)

(defn morph [structure]
  (cond (or (= :fail structure) 
            (nil? structure)
            (string? structure)) structure
        
        (u/get-in structure [:synsem]) (morph (u/get-in structure [:italiano]))
        
        (and (not (nil? (u/get-in structure [:a])))
             (not (nil? (u/get-in structure [:b]))))
        (string/trim (string/join " "
                                  (map morph
                                       [(u/get-in structure [:a])
                                        (u/get-in structure [:b])])))

        (nil? (u/get-in structure [:italiano])) "<empty>"
        
        (and (irregular-conditional? structure)
             (not (= :use-regular (irregular structure :conditional))))
        (irregular structure :conditional)

        (and (irregular-future? structure)
             (not (= :use-regular (irregular structure :future))))
        (irregular structure :future)

        (irregular-gerund? structure)
        (irregular-gerund structure)

        (irregular-passato? structure)
        (irregular-passato structure)

        (and (irregular-present? structure)
             (not (= :use-regular (irregular structure :present))))
        (irregular structure :present)

        (and (irregular-imperfetto? structure)
             (not (= :use-regular (irregular structure :imperfetto))))
        (irregular structure :imperfetto)

        true
        (let [path-to-root
              (cond
                (and
                 (not (nil? (u/get-in structure [:future-stem])))
                 (or (not (= :fail
                             (unify structure
                                    {:cat :verb
                                     :infl :future})))
                     (not (= :fail
                             (unify structure
                                    {:cat :verb
                                     :infl :conditional})))))
                [:future-stem]
                true
                [:italiano])
              regexps
              (concat
               (mapcat :g
                       (filter #(not (= % :fail))
                               (map
                                #(unify %
                                        {:boot-verb (u/get-in structure [:boot-verb] false)
                                         :u structure})
                                rules)))
               [#"(.*)" "$1"])]
          (first (find-matching-pair (u/get-in structure path-to-root)
                                     regexps)))))

(defn irregular-conditional? [structure]
  (and
   (= :verb (u/get-in structure [:cat]))
   (= :conditional (u/get-in structure [:infl]))
   (map? (u/get-in structure [:conditional]))))

(defn irregular-future? [structure]
  (and
   (= :verb (u/get-in structure [:cat]))
   (= :future (u/get-in structure [:infl]))
   (map? (u/get-in structure [:future]))))

(defn irregular-present? [structure]
  (and
   (= :verb (u/get-in structure [:cat]))
   (= :present (u/get-in structure [:infl]))
   (map? (u/get-in structure [:present]))))

(defn irregular-imperfetto? [structure]
  (and
   (= :verb (u/get-in structure [:cat]))
   (= :imperfetto (u/get-in structure [:infl]))
   (map? (u/get-in structure [:imperfetto]))))

(defn irregular-gerund? [structure]
  (and
   (= :verb (u/get-in structure [:cat]))
   (= :gerund (u/get-in structure [:infl]))
   (string? (u/get-in structure [:gerund]))))

(defn irregular-gerund [structure]
  (u/get-in structure [:imperfetto]))

(defn irregular-passato? [structure]
  (and
   (= :verb (u/get-in structure [:cat]))
   (= :passato (u/get-in structure [:infl]))
   (string? (u/get-in structure [:passato]))))

(def essere-passato-regexps
  (-> (str "babel/italiano/morphology/verbs/new/essere-passato.edn")
      clojure.java.io/resource
      slurp
      read-string))

(defn irregular-passato [structure]
  (println (u/strip-refs structure))
  (first
   (find-matching-pair
    (u/get-in structure [:passato])
    (->>
     essere-passato-regexps
     (filter #(not (= :fail
                      (u/unify structure
                               {:agr (u/get-in % [:agr] :top)
                                :essere (u/get-in % [:u :essere] :top)}))))
     (mapcat :g)))))

(defn irregular-gerund [structure]
  (u/get-in structure [:gerund]))

(defn irregular [structure infl]
  (let [arg (u/get-in structure [:agr])
        irreg (u/get-in structure [infl])]
    (cond
      (and (not (= :fail (unify arg
                                {:person :1st
                                 :number :sing})))
           (u/get-in irreg [:1sing]))
      (u/get-in irreg [:1sing])

      (and (not (= :fail (unify arg
                                {:person :2nd
                                 :number :sing})))
           (u/get-in irreg [:2sing]))
      (u/get-in irreg [:2sing])
      
      (and (not (= :fail (unify arg
                                {:person :3rd
                                 :number :sing})))
           (u/get-in irreg [:3sing]))
      (u/get-in irreg [:3sing])

      (and (not (= :fail (unify arg
                                {:person :1st
                                 :number :plur})))
           (u/get-in irreg [:1plur]))
      (u/get-in irreg [:1plur])

      (and (not (= :fail (unify arg
                                {:person :2nd
                                 :number :plur})))
           (u/get-in irreg [:2plur]))
      (u/get-in irreg [:2plur])
      
      (and (not (= :fail (unify arg
                                {:person :3rd
                                 :number :plur})))
           (u/get-in irreg [:3plur]))
      (u/get-in irreg [:3plur])

      true :use-regular)))

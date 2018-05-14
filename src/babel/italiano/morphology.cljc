(ns babel.italiano.morphology
  (:refer-clojure :exclude [get-in resolve])
  (:require
   [babel.pos :refer (noun)]
   [babel.italiano.morphology.adjectives :as adjectives]
   [babel.italiano.morphology.adjectives
    :refer (plural-to-singular-adj-masc
            plural-to-singular-adj-fem-sing
            plural-to-singular-adj-fem-plur)]
   [babel.italiano.morphology.determiners :as determiners]
   [babel.italiano.morphology.nouns :as nouns]
   [babel.italiano.morphology.nouns
    :refer (plural-to-singular-noun-fem-1
            plural-to-singular-noun-masc-1
            plural-to-singular-noun-masc-2)]
   [babel.italiano.morphology.verbs :as verbs]
   [babel.morphology :as language-independent]
   [babel.stringutils :refer (replace-from-list)]
   [clojure.string :as string]
   [clojure.string :refer (trim)]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [dag_unify.core :refer (copy dissoc-paths fail? get-in ref? strip-refs unify)]))

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

(declare get-string)

;; TODO: this is an overly huge method that needs to be
;; reimplemented: instead, add a ':g' to: babel.italiano.morphology.nouns/patterns.
(defn get-string-1 [word]
  (let [person (get-in word [:agr :person])
        number (get-in word [:agr :number])]
    (cond
      (and (string? (get-in word [:a]))
           (string? (get-in word [:b])))
      (get-string (get-in word [:a])
                  (get-in word [:b]))
      (and (string? (get-in word [:a]))
           (map? (get-in word [:b])))
      (get-string (get-in word [:a])
                  (get-in word [:b]))
      (and (map? (get-in word [:a]))
           (map? (get-in word [:b])))
      (get-string
       (get-string (get-in word [:a]))
       (get-string (get-in word [:b])))

      (= :verb (get-in word [:cat]))
      (let [conjugated (verbs/conjugate word)]
        (if (or (nil? conjugated) (empty? conjugated))
          (do
            (log/debug (str "failed to conjugate: "
                            (dag_unify.core/strip-refs word)))
            "..")
          conjugated))

      ;; TODO: this rule is pre-empting all of the following rules
      ;; that look in :a and :b. Either remove those following rules
      ;; if they are redundant and not needed, or move this general rule
      ;; below the following rules.
      (and (not (= :none (get-in word [:a] :none)))
           (not (= :none (get-in word [:b] :none))))
      (get-string (get-in word [:a])
                  (get-in word [:b]))

      (and
       (string? (get-in word '(:a :italiano)))
       (string? (get-in word '(:b :italiano)))
       (or (= :none (get-in word '(:b :agr :number) :none))
           (= :top (get-in word '(:b :agr :number) :none)))
       )
      (str (string/trim (get-in word '(:a :italiano)))
           " "
           (string/trim (get-in word '(:b :italiano))))
      (and
       (string? (get-in word [:a]))
       (string? (get-in word '(:b :italiano)))
       (or (= :none (get-in word '(:b :agr :number) :none))
           (= :top (get-in word '(:b :agr :number) :none)))
       )
      (str (string/trim (get-in word [:a]))
           " "
           (string/trim (get-in word '(:b :italiano))))
      (and
       (string? (get-in word '(:a :italiano)))
       (get-in word '(:a :italiano))
       (or (= :none (get-in word '(:b :agr :number) :none))
           (= :top (get-in word '(:b :agr :number) :none)))
       (= (get-in word '(:a :infl)) :top))
      (string/trim (str (get-in word '(:a :italiano))
                        " " (get-string-1 (get-in word [:b]))))

      ;; TODO: all of the rules that handle exceptions should be removed:
      ;; exceptions are dealt with at compile-time now, via babel.italiano.morphology/exception-generator

      ;; handle lexical exceptions (plural feminine adjectives):
      (and
       (= (get-in word '(:agr :number)) :plur)
       (= (get-in word '(:agr :gender)) :fem)
       (= (get-in word '(:cat)) :adjective)
       (string? (get-in word '(:fem :plur))))
      (get-in word '(:fem :plur))

      ;; handle lexical exceptions (plural feminine adjectives):
      (and
       (= (get-in word '(:agr :number)) :plur)
       (= (get-in word '(:agr :gender)) :fem)
       (= (get-in word '(:cat)) :adjective)
       (string? (get-in word '(:fem :plur))))
      (get-in word '(:fem :plur))

      ;; handle lexical exceptions (plural masculine adjectives):
      (and
       (= (get-in word '(:agr :number)) :plur)
       (= (get-in word '(:agr :gender)) :masc)
       (= (get-in word '(:cat)) :adjective)
       (string? (get-in word '(:masc :plur))))
      (get-in word '(:masc :plur))

      (and
       (string? (get-in word [:italiano]))
       (or (= (get-in word [:agr :gender]) :masc)
           (= (get-in word [:agr :gender]) :top))
       (= (get-in word [:agr :number]) :plur)
       (= (get-in word [:cat]) :adjective))
      (string/replace (get-in word '[:italiano])
                      #"[eo]$" "i") ;; nero => neri
      
      (and
       (= (get-in word '(:agr :gender)) :fem)
       (= (get-in word '(:agr :number)) :plur)
       (= (get-in word '(:cat)) :adjective)
       (re-find #"o$" (get-in word [:italiano])))
      (string/replace (get-in word [:italiano])
                      #"[o]$" "e") ;; nero => nere
      (and
       (= (get-in word '(:agr :gender)) :fem)
       (= (get-in word '(:agr :number)) :plur)
       (= (get-in word '(:cat)) :adjective)
       (re-find #"e$" (get-in word [:italiano])))
      (string/replace (get-in word [:italiano])
                      #"[e]$" "i") ;; difficile => difficili
     
      ;; handle lexical exceptions (plural nouns):
      (and
       (= (get-in word '(:agr :number)) :plur)
       (= (get-in word '(:cat)) :noun)
       (string? (get-in word '(:plur))))
      (get-in word '(:plur))

      ;; regular masculine nouns
      (and
       (string? (get-in word [:italiano]))
       (= (get-in word '(:agr :gender)) :masc)
       (= (get-in word '(:agr :number)) :plur)
       (= :noun (get-in word '(:cat)))
       (not (= true (get-in word '(:pronoun))))
       (get-in word [:italiano]))
      (string/replace (get-in word [:italiano])
                      #"[eo]$" "i") ;; dottore => dottori; medico => medici

      ;; regular feminine nouns ending in 'e':
      (and
       (string? (get-in word [:italiano]))
       (= (get-in word '(:agr :gender)) :fem)
       (= (get-in word '(:agr :number)) :plur)
       (= (get-in word '(:cat)) :noun)
       (get-in word [:italiano])
       (re-find #"e$" (get-in word [:italiano])))
      (string/replace (get-in word [:italiano])
                      #"[e]$" "i") ;; madre => madri
      
      ;; regular feminine nouns not ending in 'e'
      (and
       (string? (get-in word [:italiano]))
       (= (get-in word '(:agr :gender)) :fem)
       (= (get-in word '(:agr :number)) :plur)
       (= (get-in word '(:cat)) :noun)
       (get-in word [:italiano]))
      (string/replace (get-in word [:italiano])
                      #"[aà]$" "e") ;; donna => donne

      ;; TODO: move this down to other adjectives.
      ;; this was moved up here to avoid
      ;; another rule from matching it.
      (and
       (string? (get-in word [:italiano]))
       (= (get-in word '(:agr :gender)) :fem)
       (= (get-in word '(:agr :number)) :plur)
       (= (get-in word '(:cat)) :adjective))
      (string/replace (get-in word [:italiano])
                      #"[eo]$" "e") ;; nero => nere

      ;; TODO: move this down to other adjectives.
      ;; this was moved up here to avoid
      ;; another rule from matching it.
      ;; exceptional feminine singular adjectives
      (and
       (= (get-in word '(:agr :gender)) :fem)
       (= (get-in word '(:agr :number)) :sing)
       (= (get-in word '(:cat)) :adjective)
       (string? (get-in word '(:fem :sing))))
      (get-in word '(:fem :sing))

      ;; TODO: move this down to other adjectives.
      ;; this was moved up here to avoid
      ;; another rule from matching it.
      ;; regular feminine singular adjectives
      (and
       (string? (get-in word [:italiano]))
       (= (get-in word '(:agr :gender)) :fem)
       (= (get-in word '(:agr :number)) :sing)
       (= (get-in word '(:cat)) :adjective))
      (string/replace (get-in word [:italiano])
                      #"[eo]$" "a") ;; nero => nera
     
      (and (= :infinitive (get-in word '(:infl)))
           (string? (get-in word [:italiano])))
      (get-in word [:italiano])

      (and
       (get-in word [:a])
       (get-in word [:b]))
      (str
       (trim (get-string-1 (get-in word [:a]))) " "
       (trim (get-string-1 (get-in word [:b]))))
     
     (and
      (string? (get-in word [:italiano]))
      (= :top (get-in word '(:agr :sing) :top)))
     (str (get-in word [:italiano]))

     ;; TODO: possibly remove this: not sure it's doing anything.
     (= true (get-in word [:exception]))
     (get-in word [:italiano])

     (= (get-in word '(:infl)) :top)
     (str (get-in word [:italiano]))

     (and
      (get-in word [:a])
      (get-in word [:b]))
     (get-string
      (get-in word [:a])
      (get-in word [:b]))

     (= (get-in word [:a]) :top)
     (str
      ".." " " (get-string-1 (get-in word [:b])))

     (and
      (= (get-in word [:b]) :top)
      (string? (get-string-1 (get-in word [:a]))))
     (str
      (get-string-1 (get-in word [:a]))
      " " "..")

     (and
      (= (get-in word [:b]) :top)
      (string? (get-in word '(:a :italiano))))
     (str
      (get-string-1 (get-in word '(:a :italiano)))
      " " "..")

     (and
      (= (get-in word '(:agr :gender)) :fem)
      (= (get-in word '(:agr :number)) :sing)
      (= (get-in word '(:cat)) :noun))
     (get-in word [:italiano])

     (and
      (= (get-in word '(:agr :gender)) :masc)
      (= (get-in word '(:agr :number)) :sing)
      (= (get-in word '(:cat) :adjective)))
     (get-in word [:italiano]) ;; nero

     (and
      (= (get-in word '(:agr :gender)) :masc)
      (= (get-in word '(:agr :number)) :plur)
      (= (get-in word '(:cat)) :adjective)
      ;; handle lexical exceptions.
      (string? (get-in word '(:masc :plur))))
     (get-in word '(:masc :plur))

     (and
      (= (get-in word '(:agr :gender)) :fem)
      (= (get-in word '(:agr :number)) :plur)
      (= (get-in word '(:cat)) :adjective)
      ;; handle lexical exceptions.
      (string? (get-in word '(:fem :plur))))
     (get-in word '(:fem :plur))
     
     (string? (get-in word [:italiano]))
     (get-in word [:italiano])

     (or
      (not (= :none (get-in word [:a] :none)))
      (not (= :none (get-in word [:b] :none))))
     (get-string (get-in word [:a])
                 (get-in word [:b]))

     (and (map? word)
          (nil? (:italiano word)))
     ".."

     (or
      (= (get-in word '(:case)) {:not :acc})
      (= (get-in word '(:agr)) :top))
     ".."

     ;; TODO: throw exception rather than returning _word_, which is a map or something else unprintable.
     ;; in other words, if we've gotten this far, it's a bug.
     :else
     word)
    ))

;; TODO: replace 'a' and 'b' with 'left' and 'right' for clarity.
;; TODO: make b required so that function is easier to understand and refactor.
(defn get-string [a & [ b ]]
  (if (nil? a)
    (throw (Exception. (str "get-string: given nil."))))
  (if (= a "")
    (throw (Exception. (str "get-string: given empty string."))))

  (let [a (or a "")
        b (or b "")

        a (get-string-1 a)
        b (get-string-1 b)

        ;; TODO: eventually move all of (get-string) into rules of this kind:
        [applied-determiner-rules determiner-rules-result]
        (determiners/apply-determiner-rules (string/join " " [a b]))]
    (cond
      applied-determiner-rules
      determiner-rules-result

      (and (= a "ci")
           (string? b)
           (re-find #"^[eè]" b))
      (str "c'" b)
      
      ;; handle e.g. "io lo ho visto" => "io l'ho visto"
      (and (string? a)
           (re-find #"^l[ao]$" a)
           (string? b)
           (re-find #"^[aeiouh]" b))
      (str "l'" b)
      
      ;; 4) handle e.g. "aiutari + ti" => "aiutarti"
      (and (string? a)
           (or (re-find #"are$" a)
               (re-find #"ere$" a)
               (re-find #"ire$" a))
           (or (= b "ci")
               (= b "mi")
               (= b "la")
               (= b "le")
               (= b "li")
               (= b "lo")
               (= b "ti")
               (= b "vi")))
      (str (string/replace a #"[e]$" "")
           b)
      
      ;; prepositional phrases
      (and (= a "a")
           (string? b)
           (re-find #"^il " b))
      (str "al " (string/replace b #"^il " ""))
      
      (and (= a "a")
           (string? b)
           (re-find #"^i " b))
      (str "ai " (string/replace b #"^i " ""))
      
      (and (= a "a")
           (string? b)
           (re-find #"^le " b))
      (str "alle " (string/replace b #"^le " ""))
      
      (and (= a "a")
           (string? b)
           (re-find #"^la " b))
      (str "alla " (string/replace b #"^la " ""))
      
      (and (string? a) (string? b))
      (trim (str a " " b))
      
      (and (string? a) (string? (get-in b [:italiano])))
      (trim (str a " " (get-in b [:italiano])))
      
      (and (string? (get-in a [:italiano]))
           (string? b))
      (trim (str (get-in a [:italiano]) " " b))
      
      (and (string? a)
           (map? b))
      (throw (Exception. (str "couldn't determine how to stringify this map:(b): " b)))
      
      (and (string? b)
           (map? a))
      (throw (Exception. (str "couldn't determine how to stringify this map:(a): " a)))
      
      true
      {:a (if (nil? a) :top a)
       :b (if (nil? b) :top b)})))

(defn fo [input]
  (cond 

   (= input :fail)
   (str input)

   (string? input)
   input

   (:italiano input)
   ;; get-string should always return a string, but sometimes it (incorrectly) does not (FIXME)
   (string/trim (str (get-string (get-in input [:italiano]))))
   
   (and (map? input)
        (get-in input [:a])
        (get-in input [:b]))
   (str (string/join " " 
                     (list (fo (get-in input [:a]))
                           (get-in input [:punctuation :middle])
                           (fo (get-in input [:b])))))
                     
   (or (seq? input)
       (vector? input))
   (str "(" (string/join " , " 
                         (remove #(= % "")
                                 (map #(let [f (fo %)] (if (= f "") "" (str "" f ""))) input)))
        ")")

   true
   ""))

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

;; TODO: nouns do not need {:essere false}
(ns babel.italiano.lexicon
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.encyclopedia :as encyc]
   [babel.lexiconfn
    :as lexfn
    :refer [apply-unify-key default evaluate
            filter-vals listify map-function-on-map-vals
            new-entries rewrite-keys noun-pred-defaults
            verb-pred-defaults]]

   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [babel.italiano.morphology :as morph
    :refer [phonize2]]

   [clojure.edn :as edn]
   [clojure.java.io :refer [resource]]
   [clojure.repl :refer [doc]]
   [dag_unify.core :refer [dissoc-paths fail? get-in strip-refs unify]]))

(declare defaults)
(declare edn2lexicon)
(declare exception-generator)

(defn merge-lexicons [lexicons]
  (reduce (fn [a b]
            (merge-with concat a b))
          lexicons))

;; TODO: rename this to (defn compile-lexicon) or similar:
;; it does not convert an .edn file to a lexicon as its name suggests, but
;; rather applies transformations to an existing lexicon.
(defn edn2lexicon
  "Apply Italian-specific lexical rules to enhance input lexicon map into fully-specified lexical entries."
  [input-lexicon]
  (-> input-lexicon

      ;; if :vcat = :noun-invariable-{feminine,masculine}, then add plural exception.
      (map-function-on-map-vals
       (fn [k lexemes]
         (map (fn [lexeme]
                (cond (= :noun-invariable-feminine (get-in lexeme [:vcat]))
                      (unify
                       {:synsem {:cat :noun
                                 :agr {:gender :fem}}
                        :italiano {:plur k}}
                       lexeme)
                      (= :noun-invariable-masculine (get-in lexeme [:vcat]))
                      (unify
                       {:synsem {:cat :noun
                                 :agr {:gender :masc}}
                        :italiano {:plur k}}
                       lexeme)
                      true
                      lexeme))
              lexemes)))
      
      ;; <noun default rules>
      (default ;; a noun by default is neither a pronoun nor a propernoun.
       {:synsem {:cat :noun
                 :agr {:person :3rd}
                 :pronoun false
                 :propernoun false}})

      (default ;; a common noun takes a determiner as its only argument.
       {:synsem {:cat :noun
                 :pronoun false
                 :propernoun false
                 :subcat {:1 {:cat :det}
                          :2 []}}})

      (default ;; how a determiner modifies its head noun's semantics.
       (let [def (atom :top)]
         {:synsem {:cat :noun
                   :pronoun false
                   :propernoun false
                   :sem {:spec {:def def}}
                   :subcat {:1 {:def def}}}}))
      
      (default ;; a pronoun takes no args.
       {:synsem {:cat :noun
                 :pronoun true
                 :propernoun false
                 :subcat []}})

      (default ;; a propernoun takes no args.
       {:synsem {:cat :noun
                 :pronoun false
                 :propernoun true
                 :subcat []}})

      (default ;; a propernoun is agr=3rd singular
       {:synsem {:cat :noun
                 :pronoun false
                 :propernoun true
                 :agr {:number :sing
                       :person :3rd}}})

      (default  ;; pronouns are non-reflexive by default...
       {:synsem {:cat :noun
                 :pronoun true
                 :reflexive false}})

      (default  ;; ..but reflexive pronouns are case=acc
       {:synsem {:case :acc
                 :cat :noun
                 :pronoun true
                 :reflexive true}})
      
      (default
       ;; pronoun case and subcat: set sharing within :italiano so
       ;; that morphology can work as expected.
       (let [cat (atom :noun)
             case (atom :top)]
         {:synsem {:case case
                   :cat cat
                   :pronoun true
                   :subcat []}
          :italiano {:cat cat
                     :case case}}))
      
      (default ;; determiner-noun agreement
       (unify {:synsem {:cat :noun
                        :pronoun false
                        :propernoun false
                        :subcat {:1 {:cat :det}
                                 :2 []}}}
              (let [agr (atom :top)
                    cat (atom :top)]
                {:italiano {:agr agr
                            :cat cat}
                 :synsem {:cat cat
                          :agr agr
                          :subcat {:1 {:agr agr}}}})))

      ;; pronouns have semantic number and gender.
      (default
       (let [gender (atom :top)
             number (atom :top)]
         {:synsem {:cat :noun
                   :pronoun true
                   :agr {:gender gender
                         :number number}
                   :sem {:gender gender
                         :number number}}}))

      ;; propernouns have semantic number and gender.
      (default
       (let [gender (atom :top)
             number (atom :top)]
         {:synsem {:cat :noun
                   :propernoun true
                   :agr {:gender gender
                         :number number}
                   :sem {:gender gender
                         :number number}}}))

      ;; nouns are semantically non-null by default
      (default
       {:synsem {:cat :noun
                 :sem {:null false}}})

      (noun-pred-defaults)

      ;; nouns are not propernouns by default.
      (default
       {:synsem {:cat :noun
                 :propernoun false}})
      
      ;; </noun default rules>            

      ;; <verb default rules>
      (default (let [cat (atom :verb)
                     agr (atom :top)
                     essere (atom :top)
                     infl (atom :top)]
                 {:applied {:subject-agreement true}
                  :synsem {:agr agr
                           :cat cat
                           :subcat {:1 {:agr agr
                                        :sem {:can-be-subject true}}}
                           :essere essere
                           :infl infl}}))
      
      (default ;; aux defaults to false:
       {:synsem {:cat :verb
                 :aux false}})

      (default ;; ..but if aux is true:
       (let [;; whether a verb has essere or avere as its
             ;; auxiliary to form its past form:
             pred (atom :top)
             sem (atom :top)
             subject (atom :top)]
         {;; useful for diagnostics: can check for
          ;; this being set within a parse or generation tree.
          :applied {:aux-is-true-1 true}

          :synsem {:aux true
                   :cat :verb
                   :sem sem
                   :subcat {:1 subject
                            :2 {:cat :verb
                                :aux false
                                :subcat {:1 subject}
                                :sem sem}}}}))

      (default ;; a verb's first argument's case is nominative.
       {:applied {:subcat-1-is-nom true}
        :synsem {:cat :verb
                 :subcat {:1 {:cat :noun
                              :case :nom}}}})

      (default ;; a verb's second argument's case is accusative.
       {:applied {:subcat-2-is-acc true}
        :synsem {:cat :verb
                 :subcat {:2 {:cat :noun
                              :case :acc}}}})
      
      (default ;;  a verb's first argument defaults to the semantic subject of the verb.
       (let [subject-semantics (atom :top)]
         {:synsem {:cat :verb
                   :subcat {:1 {:sem subject-semantics}}
                   :sem {:subj subject-semantics}}}))

      (verb-pred-defaults encyc/verbs)

      (new-entries
       ;; remove the second argument and semantic object to make verbs intransitive,
       ;; if it is {:cat :verb} and {:allow-intransitive true}, etc.

       ;; if verb allows intransitivization:
       {:allow-intransitivize true
        :synsem {:cat :verb
                 :aux false
                 :sem {:obj {:top :top}
                       :reflexive false}
                 :subcat {:2 {:cat :noun}
                          :3 []}}}
       ;; then create new intransitive entry:
       (fn [lexeme]
         (unify (dissoc-paths lexeme [[:synsem :sem :obj]
                                      [:synsem :subcat :2]
                                      [:synsem :subcat :3]])
                {:applied {:intransitivize true}
                 :synsem {:subcat {:2 []
                                   :3 []}
                          :sem {:obj :unspec}}})))

      (default ;; reflexive defaults to false..
       {:synsem {:cat :verb
                 :aux false
                 :sem {:reflexive false}}})

      (default ;; ..but if a verb *is* reflexive:
       (let [subject-semantics (atom :top)
             subject-agr (atom :top)]
         {:synsem {:aux false
                   :cat :verb
                   :essere true
                   :sem {:subj {:prop subject-semantics}
                         :obj {:prop subject-semantics}
                         :reflexive true}
                   :subcat {:1 {:agr subject-agr
                                :sem {:prop subject-semantics}}
                            :2 {:agr subject-agr
                                :pronoun true
                                :reflexive true
                                :sem {:prop subject-semantics}}}}}))

      ;; if a verb has a second argument (i.e. the object),
      ;; then it is not reflexive. this is to prevent
      ;; generation of sentences like 'io mi ho'.
      (default
       {:synsem {:aux false
                 :cat :verb
                 :essere false
                 :sem {:reflexive false}
                 :subcat {:2 {:case :acc
                              :cat :noun
                              :reflexive false
                              :subcat []}}}})
      
      (default ;; a verb defaults to intransitive.
       {:synsem {:cat :verb
                 :subcat {:1 {:top :top}
                          :2 []}}})
      
      (default ;; intransitive verbs' :obj is :unspec.
       {:synsem {:cat :verb
                 :subcat {:1 {:top :top}
                          :2 []}
                 :sem {:obj :unspec}}})

      (default ;;  a verb's second argument (if there is one)
       ;; defaults to the semantic object of the verb.
       (let [object-semantics (atom :top)]
         {:synsem {:cat :verb
                   :subcat {:2 {:sem object-semantics}}
                   :sem {:obj object-semantics}}}))

      (default ;; a verb defaults to transitive if not intransitive..
       {:synsem {:cat :verb
                 :subcat {:1 {:top :top}
                          :2 {:top :top}
                          :3 []}}})

      (default ;;  but if there *is* a third argument, it defaults
       ;; to the semantic indirect object of the verb.
       (let [indirect-object-semantics (atom :top)]
         {:synsem {:cat :verb
                   :subcat {:3 {:sem indirect-object-semantics}}
                   :sem {:iobj indirect-object-semantics}}}))
      
      (default ;; a verb agrees with its first argument
       (let [subject-agreement (atom :top)]
         {:synsem {:cat :verb
                   :subcat {:1 {:agr subject-agreement}}
                   :agr subject-agreement}}))

      (default ;; essere defaults to false.
       {:synsem {:cat :verb
                 :essere false}})

      (default ;; subject is semantically non-null by default.
       {:synsem {:cat :verb
                 :aux false
                 :subcat {:1 {:sem {:null false}}}}}) 

      (default ;; subject is semantically non-null by default.
       {:synsem {:cat :verb
                 :aux false
                 :subcat {:1 {:sem {:null true}}}}})
      
      ;; </verb default rules>

      ;; <preposition default rules>
      (default ;;  a preposition's semantic object defaults to its first argument.
       (let [object-semantics (atom :top)]
         {:synsem {:cat :prep
                   :subcat {:1 {:cat :noun
                                :subcat []
                                :sem object-semantics}
                            :2 []}
                   :sem {:obj object-semantics}}}))

      ;; a preposition's object cannot be a reflexive pronoun.
      (default
       {:synsem {:cat :prep
                 :subcat {:1 {:reflexive false}}}})

      ;; </preposition default rules>
      
      ;; <adjective default rules>
      (default ;; an adjective is comparative=false by default..
       (-> defaults
           :adjective
           :non-comparative))
      ;; ..but, if comparative:
      (default
       (let [complement-sem (atom :top)
             subject-sem (atom :top)]
         {:synsem {:sem {:comparative true
                         :arg1 subject-sem
                         :arg2 complement-sem}
                   :cat :adjective
                   :subcat {:1 {:cat :noun
                                :sem subject-sem}
                            :2 {:cat :prep
                                :sem {:pred :di ;; Italian name for pred, for now: TODO: change to English :than.
                                      :obj complement-sem}
                                :subcat {:1 {:sem complement-sem}
                                         :2 []}}}}}))
      ;; </adjective default rules>

      ;; <sent-modifier default rules>
      (default
       (let [sentential-sem (atom :top)]
         {:synsem {:cat :sent-modifier
                   :sem {:subj sentential-sem}
                   :subcat {:1 {:sem sentential-sem}}}}))
      ;; </sent-modifier default rules>

      ;; <adverb default rules>
      (default
       (let [verb-sem (atom :top)]
         {:synsem {:cat :adverb
                   :sem verb-sem
                   :subcat {:1 {:cat :verb
                                :sem verb-sem}
                            :2 []}}}))
      ;; </adverb default rules>
      
      ;; <determiner default rules>
      (default
       (let [def (atom :top)]
         {:synsem {:cat :det
                   :def def
                   :sem {:def def}}}))

      ;; <category-independent> 
      ;; This rule needs to be before exception-generator; otherwise
      ;; exception will be generated that fail to match this agreement rule. Putting it
      ;; before prevents these bad exceptions from existing.
      (default ;; morphology looks in :italiano, so share relevant grammatical pieces of
       ;; info (:agr, :cat, :infl, and :essere) there so it can see them.
       (unify (:agreement defaults)
              {:applied {:agreement-defaults true}}))
      ;; </category-independent>
      
      exception-generator ;; add new keys to the map for all exceptions found.

      ;; <category-independent> 
      (default ;; morphology looks in :italiano, so share relevant grammatical pieces of
       ;; info (:agr, :cat, :infl, and :essere) there so it can see them.
       (unify (:agreement defaults)
              {:applied {:agreement-defaults true}}))
      ;; </category-independent>

      phonize2 ;; for each value v of each key k, set the [:italiano :italiano] of v to k, if not already set
      ;; e.g. by exception-generator2.

      ;; TODO: throw error or warning in certain cases:
      ;; (= true (fail? value))
      ;;
      
      ;; common nouns need a gender (but propernouns do not need one).
      ;; TODO: throw error rather than just throwing out entry.
      (filter-vals
       #(or (not (and (= :noun (get-in % [:synsem :cat]))
                      (= :none (get-in % [:synsem :agr :gender] :none))
                      (= false (get-in % [:synsem :propernoun] false))
                      (= false (get-in % [:synsem :pronoun] false))))
            (and (log/warn (str "ignoring common noun with no gender specified: " %))
                 false)))

      (filter-vals
       #(not (= :fail %)))
      
      ;; filter out entries with no :cat.
      (filter-vals
       #(or (and (not (= :none (get-in % [:synsem :cat] :none)))
                 (or (log/debug (str "lexical entry has a cat - good : " (strip-refs %)))
                     true))
            (and (log/warn (str "ignoring lexical entry with no :cat: " (strip-refs %)))
                 false)))))

      ;; end of language-specific grammar rules
      

(defn compile-lexicon
  "convert source lexicon to a Clojure map."
  []
  (let [lexicon-sources ["babel/italiano/lexicon/adjectives.edn"
                         "babel/italiano/lexicon/adverbs.edn"
                         "babel/italiano/lexicon/determiners.edn"
                         "babel/italiano/lexicon/exclamations.edn"
                         "babel/italiano/lexicon/modifiers.edn"
                         "babel/italiano/lexicon/nouns.edn"
                         "babel/italiano/lexicon/prepositions.edn"
                         "babel/italiano/lexicon/pronouns.edn"
                         "babel/italiano/lexicon/propernames.edn"
                         "babel/italiano/lexicon/verbs.edn"]]
    (merge-lexicons
     (map (fn [lexicon-source]
            (->
             lexicon-source
             resource
             lexfn/edn2lexicon
             edn2lexicon))
          lexicon-sources))))

;; The values in this map in (defonce defaults) are used for lexical
;; compilation but also available for external use.
(defonce defaults
  {:adjective
   {:non-comparative
    (let [subject (atom :top)]
      {:synsem {:cat :adjective
                   :sem {:arg1 subject
                         :comparative false}
                :subcat {:1 {:sem subject}
                         :2 []}}})}

   :agreement
   (let [agr (atom :top)
         cat (atom :verb)
         essere (atom :top)
         infl (atom :top)]
     {:italiano {:agr agr
                 :cat cat
                 :essere essere
                 :infl infl}
      :synsem {:agr agr
               :cat cat
               :essere essere
               :infl infl}})})

(defn exception-generator [lexicon]
  (let [exception-maps (morph/exception-generator lexicon)]
    (if (not (empty? exception-maps))
      (merge-with concat
                  lexicon
                  (reduce (fn [m1 m2]
                            (merge-with concat m1 m2))
                          (morph/exception-generator lexicon)))
      lexicon)))

(defn vocab-entry-to-lexeme [{surface :surface
                              pred :pred
                              vocab-cat :vocab_cat
                              structure :structure}]
  (log/info (str "calling vocab-entry-to-lexeme with "
                 "surface=" surface "; structure=" structure
                 "; vocab-cat=" vocab-cat))
  (let [structure (or structure :top)
        debug (log/debug (str "structure (pre-dissoc): " structure))
        structure (if (= "" (dag_unify.core/get-in structure [:italiano :plur]))
                    (dag_unify.core/dissoc-paths structure [[:italiano :plur]])
                    structure)
        debug (log/debug (str "structure (post-dissoc): " structure))
        ends-with (str (nth surface (- (count surface) 1)))

        structure ;; remove [:synsem :cat] so that
        ;; vocab-cat will be allowed to override :synsem :cat, if any:
        (dag_unify.core/dissoc-paths structure [[:synsem :cat]])]

    (cond
      (= vocab-cat "noun1")
      {surface
       (map #(unify structure %)
            [{:vocab-cat vocab-cat
              :synsem {:sem {:pred (keyword pred)}
                       :cat :noun
                       :agr {:gender (cond (= "o" ends-with)
                                           :masc
                                           (= "a" ends-with)
                                           :fem
                                           true
                                           :top)}}}])}
      (= vocab-cat "noun2m")
      {surface
       (map #(unify structure %)
            [{:vocab-cat vocab-cat
              :synsem {:sem {:pred (keyword pred)}
                       :cat :noun
                       :agr {:gender :masc}}}])}
      
      (= vocab-cat "noun2f")
      {surface
       (map #(unify structure %)
            [{:vocab-cat vocab-cat
              :synsem {:sem {:pred (keyword pred)}
                       :cat :noun
                       :agr {:gender :fem}}}])}

      (= vocab-cat "noun3")
      {surface
       (map #(unify structure %)
            [{:vocab-cat vocab-cat
              :synsem {:sem {:pred (keyword pred)}
                       :cat :noun
                       :agr {:gender :masc}}
              :italiano {:plur (clojure.string/replace surface #"a$" "i")}}])}

      ;; nounneutr: singular is masculine; plural is feminine: e.g. il braccio/le braccia
      (= vocab-cat "nounneutr")
      {surface
       (map #(unify structure %)
            [{:vocab-cat vocab-cat
              :synsem {:sem {:pred (keyword pred)}
                       :agr {:gender :masc
                             :number :sing}}}
             {:vocab-cat vocab-cat
              :synsem {:sem {:pred (keyword pred)}
                       :agr {:gender :fem
                             :number :plur}}
              :italiano {:plur (clojure.string/replace surface #"o$" "a")}}])}

      (= vocab-cat "nounsingm")
      {surface
       (map #(unify structure %)
            [{:vocab-cat vocab-cat
              :synsem {:sem {:pred (keyword pred)}
                       :agr {:gender :masc
                             :number :sing}}}])}

      (= vocab-cat "nounsingf")
      {surface
       (map #(unify structure %)
            [{:vocab-cat vocab-cat
              :synsem {:sem {:pred (keyword pred)}
                       :agr {:gender :fem
                             :number :sing}}}])}

      (or (= vocab-cat "nounplurm")
          (= vocab-cat "nounplm"))
      {surface
       (map #(unify structure %)
            [{:vocab-cat vocab-cat
              :italiano {:plur surface}
              :synsem {:sem {:pred (keyword pred)}
                       :agr {:gender :masc
                             :number :plur}}}])}

      (or (= vocab-cat "nounplurf")
          (= vocab-cat "nounplf"))
      {surface
       (map #(unify structure %)
            [{:vocab-cat vocab-cat
              :italiano {:plur surface}
              :synsem {:sem {:pred (keyword pred)}
                       :agr {:gender :fem
                             :number :plur}}}])}
      true
      (do
        (log/warn (str "(vocab-entry-to-lexeme: "
                       "unable to create lexeme for: '" surface "' with "
                       " vocab_cat: " vocab-cat) ": returning empty map.")
        {}))))

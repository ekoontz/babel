(ns babel.english.lexicon
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.encyclopedia :as encyc]
   [babel.english.morphology :as morph]
   [babel.lexiconfn :refer [apply-unify-key compile-lex default
                            edn2lexicon listify
                            map-function-on-map-vals new-entries
                            noun-pred-defaults
                            remove-vals verb-pred-defaults]]
   [clojure.java.io :refer [resource]]
   [clojure.tools.logging :as log]
   [dag_unify.core :as u :refer [dissoc-paths fail? get-in strip-refs unify]]))

(declare exception-generator)
(declare phonize)
(declare transform-with-english-lexical-rules)

;; TODO: allow a filter of lexemes
(defn deliver-lexicon []
  (->
   (edn2lexicon (resource "babel/english/lexicon.edn"))
   (compile-lex)

   (transform-with-english-lexical-rules)))


(defn transform-with-english-lexical-rules [lexicon]
  (->
   lexicon
   (map-function-on-map-vals
    (fn [lexical-string lexical-val]
      (phonize lexical-val lexical-string)))
   
   ((fn [lexicon]
      (merge-with concat lexicon
                  (listify 
                   (let [tmp (map #(listify %)
                                  (exception-generator lexicon))]
                     (if (empty? tmp)
                       nil
                       (reduce #(merge-with concat %1 %2)
                               tmp)))))))
   
   ;; for nouns with exceptional plural forms (e.g. "men","women"),
   ;; exception-generator has generated both the plural and the singular forms
   ;; as separate lexemes, so remove original lexeme.
   (remove-vals #(and (= :top (get-in % [:synsem :agr :number]))
                      (= :noun (get-in % [:synsem :cat]))
                      (string? (get-in % [:english :plur]))))

   apply-unify-key
   
   ;; <category-independent rules>
   
   (default
    (let [cat (atom :top)]
      {:english {:cat cat}
       :synsem {:cat cat}}))
   
   ;; </category-independent rules>

   ;; <adjective default rules>
   (default
    {:synsem {:cat :adjective
              :subcat {:1 {:cat :det}
                       :2 []}
              :sem {:comparative false}}})
   
   ;; </adjective default rules>
   
   ;; <noun default rules>
   
   ;; make :propernoun and :pronoun available to morphological rules
   ;; to prevent e.g. (they -> *theys) or (ourselves -> *ourselvess)
   (default
    (let [pronoun (atom :top)
          propernoun (atom :top)]
      {:english {:pronoun pronoun
                 :propernoun propernoun}
       :synsem {:cat :noun
                :pronoun pronoun
                :propernoun propernoun}}))
   
   ;; pronouns have semantic number and gender.
   (default
    (let [gender (atom :top)
          number (atom :top)
          gendered (atom :top)]
      {:english {:number number
                 :gender gender
                 :gendered gendered}
       :synsem {:cat :noun
                :pronoun true
                :agr {:gender gender
                      :gendered gendered
                      :number number}
                :sem {:gender gender
                      :number number}}}))

   ;; pronouns take no arguments: e.g. "she", not "the she".
   (default
    {:synsem {:cat :noun
              :pronoun true
              :subcat []}})
   
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
   
   ;; nouns have number-agreement morphology: 'the dog sleeps' vs 'the dogs sleep'
   ;; english.morphology needs to see the :cat=noun as well, so share that within :english.
   (default
    (let [agr (atom :top)]
      {:english {:agr agr}
       :synsem {:cat :noun
                :agr agr}}))
   
   ;; A pronoun is either reflexive or not reflexive, but
   ;; a non-pronoun is never reflexive.
   (default
    {:synsem {:cat :noun
              :pronoun false
              :reflexive false}})

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

   (default
    ;; a propernoun is agr=3rd singular, though in British English,
    ;; a propernoun can be 3rd plur, e.g.: "Manchester have won the final".
    {:synsem {:cat :noun
              :pronoun false
              :propernoun true
              :agr {:number :sing
                    :person :3rd}}})

   (default ;; a common-noun is agr=3rd person.
    {:synsem {:cat :noun
              :pronoun false
              :propernoun false
              :agr {:person :3rd}}})
   
   ;; common nouns' articles agree with their articles:
   ;; e.g. "a dog" but *"a dogs".
   ;; they also agree in definiteness.
   (default
    (let [agr (atom {:person :3rd})
          def (atom :top)]
      {:synsem {:cat :noun
                :def def
                :pronoun false
                :propernoun false
                :agr agr
                :subcat {:1 {:cat :det
                             :def def
                             :agr agr}}}}))
   
   ;; </noun default rules>
   
   ;; <verb default rules>
   ;; add a second argument to every verb, unless it's explicitly disallowed with {:2 []}.
   (default
    {:synsem {:cat :verb
              :subcat {:2 {:cat :top}}}})
   
   ;; this key: :modal-with determines inflection of argument of a modal verb
   ;; values can be :infinitive,:root, or false
   ;; TODO: should be possible to do with a modal verb's subcat instead of a whole new key.
   (default
    {:modal-with false
     :synsem {:cat :verb}})
   
   (default
    (let [modal-subject (atom {:cat :noun})]
      {:modal-with :infinitive
       :synsem {:cat :verb
                :subcat {:1 modal-subject
                         :2 {:cat :verb
                             :infl :infinitive
                             :subcat {:1 modal-subject
                                      :2 []}}}}}))
   (default
    (let [modal-subject (atom {:cat :noun})]
      {:modal-with :root
       :synsem {:cat :verb
                :subcat {:1 modal-subject
                         :2 {:cat :verb
                             :infl :root
                             :subcat {:1 modal-subject
                                      :2 []}}}}}))
   
   ;; prevent :shared-semantics :obj unless it's already set
   (default
    {:share-sem false
     :synsem {:cat :verb}})
   
   ;; semantic object of lexical verb is the same as the object of verb's prepositional phrase.
   (default
    (let [obj (atom :top)]
      {:applied {:prep-obj-is-verb-obj true}
       :share-sem :obj
       :synsem {:cat :verb
                :sem {:obj obj}
                :subcat {:2 {:cat :prep
                             :sem {:obj obj}}}}}))
   
   ;; add :sem :obj if necessary, so that intransitivize is triggered.
   (default {:modal-with false
             :applied {:add-sem-obj-if-necess true}
             :synsem {:cat :verb
                      :subcat {:2 {:cat :noun}}
                      :sem {:obj {:pred :top}}}})
   
   ;; intransitivize:  remove the second argument and semantic object to make verbs intransitive.
   (new-entries
    {:intransitivize false
     :phrasal-verb false
     :synsem {:cat :verb
              :aux false
              :sem {:obj {:top :top}
                    :shared-with-obj false
                    :reflexive false}
              ;; likely to be :noun or :prep but could be others
              :subcat {:2 {:cat :top}
                       :3 []}}}
    (fn [lexeme]
      (unify
       (dissoc-paths lexeme [[:synsem :sem :obj]
                             [:synsem :subcat :2]])
       {:applied {:intransitivize true}
        :synsem {:subcat {:2 []}}})))

   (default {:synsem {:cat :verb}
             :phrasal-verb false})
   
   ;; phrasal-verb-intransitivize: remove the third argument,
   ;; but not the second, to make it intransitive.
   (new-entries
    {:applied {:ditrans false} ;; don't apply to ditransitive verbs.
     :intransitivize false
     :phrasal-verb true
     :synsem {:cat :verb
              :aux false
              :subcat {:2 {:cat :prep}}}}
    (fn [lexeme]
      (unify
       (dissoc-paths
        (unify lexeme {:applied {:phrasal-verb-intransitivize true}})
        [[:synsem :subcat :3]
         [:synsem :sem :obj]])

       ;; prevent filling-in this
       ;; in a later processing step below.
       {:synsem {:subcat {:3 []}
                 :sem {:obj :unspec}}})))
   
   (default ;; intransitive verbs' :obj is :unspec.
    {:modal-with false
     :applied {:2 true}
     :synsem {:cat :verb
              :subcat {:1 {:top :top}
                       :2 []}
              :sem {:reflexive false
                    :shared-with-obj false
                    :obj :unspec}}})
   
   ;; subject-verb agreement
   (default (let [infl (atom :top)
                  agr (atom :top)]
              {:english {:agr agr
                         :infl infl}
               :synsem {:infl infl
                        :cat :verb
                        :subcat {:1 {:agr agr}}}}))

   (default {:synsem {:cat :verb
                      :subcat {:3 []}}})
   
   ;; TODO: this subject-object agreement should apply to
   ;; transitive reflexive verbs as well as ditransitive reflexive verbs.
   (default (let [agr (atom :top)
                  human (atom :top)]
              {:applied {:ditrans true
                         :ditrans-reflexive true}
               :synsem {:aux false
                        :cat :verb
                        :sem {:reflexive true
                              :obj {:prop {:human human}}
                              :subj {:prop {:human human}}}
                        :subcat {:1 {:agr agr}
                                 :3 {:agr agr}}}}))
   
   (verb-pred-defaults encyc/verbs)
   (noun-pred-defaults)
   ;; if a verb has a subject,
   ;; and the subject is {:cat :noun},
   ;; then the subject is {:synsem {:case :nom}}.
   (default {:synsem {:cat :verb
                      :subcat {:1 {:cat :noun
                                   :case :nom}}}})
   
   ;; if a verb has an object,
   ;; and the object is {:cat :noun},
   ;; then the object is {:synsem {:case :acc}}.
   (default {:synsem {:cat :verb
                      :subcat {:2 {:cat :noun
                                   :case :acc}}}})
   
   ;; aux default: false
   (default {:synsem {:cat :verb
                      :aux false}})
   
   ;; phrasal-verbs: false
   (default {:synsem {:cat :verb}
             :phrasal-verb false})
   
   (default
    (let [subject (atom :top)]
      {:synsem {:cat :verb
                :sem {:subj subject}
                :subcat {:1 {:sem subject}}}}))

   ;; by default, subjects cannot be null: e.g.
   ;; "he eats" but
   ;; not *"there eats"
   (default
    (let [subject (atom {:null false})]
      {:synsem {:cat :verb
                :sem {:subj subject}}}))
   
   ;; TODO: show an example of a verb for which
   ;; this default rule is justified.
   (default
    (let [object (atom :top)]
      {:applied {:iob-rule-1 true}
       :phrasal-verb false
       :synsem {:cat :verb
                :sem {:obj object
                      :iobj nil}
                :subcat {:2 {:sem object}
                         :3 []}}}))
   
   ;; TODO: show an example of a verb for which
   ;; this default rule is justified.
   (default
    (let [object (atom :top)]
      {:phrasal-verb true
       :applied {:prep-rule-1 true}
       :synsem {:cat :verb
                :sem {:obj object}
                :subcat {:2 {:cat :prep}
                         :3 {:cat :noun
                             :pronoun false
                             :subcat []
                             :sem object}}}}))
   (default
    (let [object (atom :top)]
      {:phrasal-verb true
       :applied {:prep-rule-2 true}
       :synsem {:cat :verb
                :sem {:obj object}
                :subcat {:2 {:cat :noun
                             :pronoun false
                             :subcat []
                             :sem object}
                         :3 {:cat :prep}}}}))
   
   ;; reflexive=false
   (default {:synsem {:cat :verb
                      :sem {:reflexive false}}})

   ;; for transitive, non-reflexive verbs, the 2nd arg is non-reflexive by default.
   (default {:synsem {:cat :verb
                      :sem {:reflexive false}
                      :subcat {:2 {:reflexive false}}}})
   
   ;; for transitive, reflexive verbs, the 2nd arg is a reflexive pronoun by default.
   (default
    (let [subject-agr (atom :top)
          subject-semantics (atom :top)]
      {:synsem {:sem {:reflexive true
                      :subj subject-semantics
                      :obj subject-semantics}
                :cat :verb
                :subcat {:1 {:agr subject-agr
                             :cat :noun
                             :sem subject-semantics}
                         :2 {:reflexive true
                             :pronoun true
                             :cat :noun
                             :sem subject-semantics
                             :agr subject-agr}}}}))

   ;; some English verbs e.g. "climb" are reflexive, but without an
   ;; explicit object, so they have a [:sem :obj], but no [:subcat :2]
   ;; we want to add a semantic object so that the semantics
   ;; are the same compared to Italian.
   (default
    (let [subject-semantics (atom :top)]
      {:synsem {:cat :verb
                :sem {:reflexive true
                      :subj subject-semantics
                      :obj subject-semantics}
                :subcat {:2 []}}}))

   ;; by default, verbs are intransitive.
   (default
    {:synsem {:cat :verb
              :subcat {:2 {:subcat []}}}})
   
   ;; note that {:english {:exception true}} is
   ;; set by (babel.english.morphology/exception-generator)
   (default
    {:english {:exception false}
     :synsem {:cat :verb}})
   
   (default
    {:english {:exception true}
     :synsem {:cat :verb
              :participle true
              :infl :participle}})
   
   (default
    {:english {:exception true}
     :synsem {:cat :verb
              :aux false
              :infl :past
              :sem {:tense :past}}})
   ;; Not sure why or if this (default) rule is needed?
   ;; Why set the :aspect to :perfect by default?
   ;; TODO: remove if not needed.
   (default
    {:english {:exception true}
     :synsem {:cat :verb
              :aux false
              :infl :past
              :sem {:aspect :perfect
                    :tense :past}}})

   (default
    {:english {:exception true}
     :synsem {:cat :verb
              :aux false
              :infl :present
              :sem {:aspect :simple
                    :tense :present}}})
   
   ;; </verb default rules>
   
   ;; <prep default rules>
   (default
    (let [obj-sem (atom :top)]
      {:synsem {:cat :prep
                :subcat {:1 {:cat :noun
                             :case :acc
                             :subcat []
                             :sem obj-sem}
                         :2 []}
                :sem {:obj obj-sem}}}))))
   
   ;; </prep default rules>
   
   

(defn exception-generator
  "_lexicon_ is a map where each key is a root form (a string) mapped to a set of lexical entries (maps) for that root form. 
  For each such lexical entry, generate all possible exceptions, where the exception-generation rules are given below as 'path-and-merge-fn' tuples."
  [lexicon]
  (->>
   (sort (keys lexicon))
   (mapcat
    (fn [root]
      (let [lexemes (get lexicon root)]
        (log/debug (str "exception generator: " root))
        (mapcat (fn [path-and-merge-fn]
                  (let [path (:path path-and-merge-fn)
                        surface (:surface path-and-merge-fn)
                        merge-fn (:merge-fn path-and-merge-fn)]
                    (log/debug (str "root: " root))
                    (log/debug (str "path: " path))

                    ;; a lexeme-kv is a pair of a key and value. The key is a string (the word's surface form)
                    ;; and the value is a list of lexemes for that string.
                    (->> lexemes
                         (mapcat (fn [lexeme]
                                   ;; this is where a unify/dissoc that supported
                                   ;; non-maps like :top and :fail, would be useful:
                                   ;; would not need the (if (not (fail? lexeme)..)) check
                                   ;; to avoid a difficult-to-understand error:
                                   ;; "java.lang.ClassCastException:
                                   ;;   clojure.lang.Keyword cannot be cast to clojure.lang.IPersistentMap"
                                   ;; TODO: call (merge-fn root lexeme) once and save in a let, not over and over.
                                   (let [merge (merge-fn root lexeme)
                                         lexeme (cond (= lexeme :fail)
                                                      :fail
                                                      (= lexeme :top)
                                                      :top
                                                      true lexeme)
                                         synsem-check
                                         (if (string? (get-in lexeme path :none))
                                           (unify (get-in lexeme [:synsem])
                                                  (get-in merge [:synsem] :top))
                                           :fail)]
                                     (if (and (string? (get-in lexeme path :none))
                                              (not (= :fail synsem-check)))
                                       (list {(if (= surface :use-root)
                                                root
                                                (get-in lexeme path))
                                              (unify
                                               (dissoc-paths lexeme [path
                                                                     [:english :english]])
                                               merge
                                               {:exception true}
                                               {:synsem synsem-check}
                                               {:english {:root root
                                                          :exception true}})}))))))))
                [
                 ;; 1. plural exceptions: e.g. "men","women":
                 {:path [:english :plur]
                  :merge-fn
                  (fn [root val]
                    {:synsem {:cat :noun}
                     :english {:agr {:number :plur}
                               :english (get-in val [:english :plur])}})}

                 {:path [:english :plur]
                  :surface :use-root
                  :merge-fn
                  (fn [root val]
                    {:synsem {:cat :noun}
                     :english {:agr {:number :sing}
                               :english root}})}

                 ;; <2. past exceptions: e.g. "sleep" -> "slept">
                 {:path [:english :past :1sing]
                  :merge-fn
                  (fn [root val]
                    {:synsem {:cat :verb
                              :subcat {:1 {:agr {:number :sing
                                                 :person :1st}}}}
                     :english {:infl :past
                               :english (get-in val [:english :past :1sing])}})}
                 
                 {:path [:english :past :2sing]
                  :merge-fn
                  (fn [root val]
                    {:synsem {:cat :verb
                              :subcat {:1 {:agr {:number :sing
                                                 :person :2nd}}}}
                     :english {:infl :past
                               :english (get-in val [:english :past :2sing])}})}
                 
                 {:path [:english :past :3sing]
                  :merge-fn
                  (fn [root val]
                    {:synsem {:cat :verb
                              :subcat {:1 {:agr {:number :sing
                                                 :person :3rd}}}}
                     :english {:infl :past
                               :english (get-in val [:english :past :3sing])}})}
                 
                 {:path [:english :past :1plur]
                  :merge-fn
                  (fn [root val]
                    {:synsem {:cat :verb
                              :subcat {:1 {:agr {:number :plur
                                                 :person :1st}}}}
                     :english {:infl :past
                               :english (get-in val [:english :past :1plur])}})}
                 
                 {:path [:english :past :2plur]
                  :merge-fn
                  (fn [root val]
                    {:synsem {:cat :verb
                              :subcat {:1 {:agr {:number :plur
                                                 :person :2nd}}}}
                     :english {:infl :past
                               :english (get-in val [:english :past :2plur])}})}
                 
                 {:path [:english :past :3plur]
                  :merge-fn
                  (fn [root val]
                    {:synsem {:cat :verb
                              :subcat {:1 {:agr {:number :plur
                                                 :person :3rd}}}}
                     :english {:infl :past
                               :english (get-in val [:english :past :3plur])}})}
                 
                 {:path [:english :past]
                  :merge-fn
                  (fn [root val]
                    {:synsem {:cat :verb}
                     :english {:infl :past
                               :english (get-in val [:english :past])}})}
                 ;; </2. past exceptions: e.g. "sleep" -> "slept">
                 
                 ;; <3. present exceptions: e.g. "be" -> "am">
                 {:path [:english :present :1sing]
                  :merge-fn
                  (fn [root val]
                    {:synsem {:cat :verb
                              :subcat {:1 {:agr {:number :sing
                                                 :person :1st}}}}
                     :english {:infl :present
                               :english (get-in val [:english :present :1sing])}})}
                 
                 {:path [:english :present :2sing]
                  :merge-fn
                  (fn [root val]
                    {:synsem {:cat :verb
                              :subcat {:1 {:agr {:number :sing
                                                 :person :2nd}}}}
                     :english {:infl :present
                               :english (get-in val [:english :present :2sing])}})}
                 
                 {:path [:english :present :3sing]
                  :merge-fn
                  (fn [root val]
                    {:synsem {:cat :verb
                              :subcat {:1 {:agr {:number :sing
                                                 :person :3rd}}}}
                     :english {:infl :present
                               :english (get-in val [:english :present :3sing])}})}
                 
                 {:path [:english :present :1plur]
                  :merge-fn
                  (fn [root val]
                    {:synsem {:cat :verb
                              :subcat {:1 {:agr {:number :plur
                                                 :person :1st}}}}
                     :english {:infl :present
                               :english (get-in val [:english :present :1plur])}})}
                 
                 {:path [:english :present :2plur]
                  :merge-fn
                  (fn [root val]
                    {:synsem {:cat :verb
                              :subcat {:1 {:agr {:number :plur
                                                 :person :2nd}}}}
                     :english {:infl :present
                               :english (get-in val [:english :present :2plur])}})}
                 
                 {:path [:english :present :3plur]
                  :merge-fn
                  (fn [root val]
                    {:synsem {:cat :verb
                              :subcat {:1 {:agr {:number :plur
                                                 :person :3rd}}}}
                     :english {:infl :present
                               :english (get-in val [:english :present :3plur])}})}
                 
                 {:path [:english :present]
                  :merge-fn
                  (fn [root val]
                    {:synsem {:cat :verb}
                     :english {:infl :present
                               :english (get-in val [:english :present])}})}
                 ;; </3. present exceptions: e.g. "be" -> "am"
                 
                 {:path [:english :participle]
                  :merge-fn
                  (fn [root val]
                    {:synsem {:cat :verb}
                     :english {:infl :participle
                               :english (get-in val [:english :participle])}})}]))))))


(defn phonize [a-map a-string]
  ;; TODO: does :phrasal mean the same thing
  ;; as :phrasal-verb? if so, remove :phrasal
  ;; and consolidate on :phrasal-verb.
  (let [common {:phrasal false}]
    ;; TODO: remove support for either list-of-maps - too confusing. Instead, just require a list of maps.
    ;; TODO: compare with counterpart function: (italiano/phonize): there is an additional cond stanza in the latter
    ;; that is not present here.
    (cond (or (vector? a-map) (seq? a-map))
          (map (fn [each-entry]
                 (phonize each-entry a-string))
               a-map)

          true
          (let [warn (if (and (u/get-in a-map [:english :english])
                              (not (= (u/get-in a-map [:english :english])
                                      a-string)))
                       (log/warn (str "ignoring existing: [:english :english] value:" (u/get-in a-map [:english :english])
                                      " in favor of input parameter's value:" a-string)))
                result
                (unify
                 ;; remove any existing [[:english :english]].
                 (u/dissoc-paths a-map [[:english :english]])
                 {:english {:english a-string}}
                 common)]
            (if (= :fail result)
              (let [error-message (str "failed to unify: " (u/strip-refs a-map) " with " {:english {:english a-string}} ".")]
                (log/error error-message)
                (throw (Exception. (str error-message)))))
            result))))

(defn vocab-entry-to-lexeme [{surface :surface
                              pred :pred
                              vocab-cat :vocab_cat
                              structure :structure}]
  (log/debug (str "calling vocab-entry-to-lexeme with "
                  "surface=" surface " and structure=" structure))
  (let [surface surface] ;; (clojure.string/replace surface #"\s*\(.*$" "")
    (cond (clojure.string/starts-with? vocab-cat "noun")
          (let [base-unify 
                {:synsem {:sem {:pred (keyword pred)}
                          :cat :noun
                          ;; add some additional constraints that we should not need
                          ;; (should be in compile-lex).
                          ;; TODO: remove need to add these constraints.
                          :propernoun false
                          :pronoun false
                          :subcat {:1 {:cat :det}}}}

                structure (if (= "" (dag_unify.core/get-in structure [:english :plur]))
                            (dag_unify.core/dissoc-paths structure [[:english :plur]])
                            structure)

                structure (if (or
                               (= "" (u/get-in structure [:synsem :cat]))
                               (= :noun1 (u/get-in structure [:synsem :cat]))
                               (= :unspec (u/get-in structure [:synsem :cat])))
                            (do (log/warn (str "removing vocabcoach-convention category from [:synsem :cat]:"
                                               (u/get-in structure [:synsem :cat])))
                                (dag_unify.core/dissoc-paths structure [[:synsem :cat]])))

                with-structure
                (if structure (unify base-unify structure)
                    base-unify)]
            (log/debug (str "creating lexemes for surface: '" surface "' and with-structure: "(u/strip-refs with-structure)))
            (if (= :fail with-structure)
              (let [message (str "fail to unify vocab-entry information: base-unify:" (u/strip-refs base-unify)
                                        "; vocab structure:" structure)]
                (log/error message)
                (throw (Exception. message))))
            (cond
              (or (= vocab-cat "nounplurf")
                  (= vocab-cat "nounplurm")
                  (= vocab-cat "nounplf")
                  (= vocab-cat "nounplm"))
              {surface
               [(unify with-structure
                       {:synsem {:agr {:number :plur}}
                        :english {:inherently-plural true}})]}
              true
              {surface
               [with-structure]}
              true {})))))




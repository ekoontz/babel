(ns babel.test.en
  ;; TODO: use u/get-in and u/assoc-in; remove these :excludes.
  (:refer-clojure :exclude [assoc-in get-in])
  (:require [babel.directory :refer [models]]
            [babel.english :as english :refer [morph morph-ps]]
            [babel.english.grammar :as grammar :refer [head-first head-last]]
            [babel.english.morphology :refer [get-string]]
            [babel.generate :as generate]
            [babel.lexiconfn :refer [write-lexicon]]
            [babel.over :refer [over overc overh]]
            [babel.parse :as parse]
            [babel.test.test :as btest]
            [babel.ug :refer [head-principle unify-check]]
            
            [clojure.math.combinatorics :as combo]
            [clojure.repl :refer [doc]]
            [clojure.string :as string]
            #?(:clj [clojure.test :refer [deftest is]])
            #?(:cljs [cljs.test :refer-macros [deftest is]])
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [babel.logjs :as log]) 

            [dag_unify.core :as u
             :refer [assoc-in dissoc-paths fail?
                     fail-path-between get-in pprint
                     strip-refs unify]]))
(btest/init-db)
(def model @@(get models :en))

(defn parse [expression]
  (english/parse expression model false))

(defn generate [expression]
  (generate/generate expression model))

(defn display-expression [expr]
  {:en (morph expr)
   :subj (get-in expr [:synsem :sem :subj :pred])
   :pred (get-in expr [:synsem :sem :pred])
   :tense (get-in expr [:synsem :sem :tense])
   :infl (get-in expr [:synsem :infl])
   :aspect (get-in expr [:synsem :sem :aspect])
   :obj (get-in expr [:synsem :sem :obj :pred])})

(deftest generate-irregular-present-1
  (let [form {:english {:a {:cat :verb,
                            :infl :present,
                            :present {:1sing "am",
                                      :3plur "are",
                                      :1plur "are",
                                      :2plur "are",
                                      :3sing "is",
                                      :2sing "are"},
                            :past {:1sing "was",
                                   :3plur "were",
                                   :1plur "were",
                                   :2plur "were",
                                   :3sing "was",
                                   :2sing "were"},
                            :agr {:person :3rd,
                                  :gender :masc,
                                  :number :sing},
                            :english "be"},
                        :b {:agr {:person :3rd,
                                  :gender :masc,
                                  :number :sing},
                            :english "Antonio",
                            :cat :noun,
                            :pronoun false}}}]
    (is (= (morph form)
           "is Antonio"))))

(deftest generate-irregular-present-2
  (let [form {:english {:past {:1sing "was",
                               :3plur "were",
                               :1plur "were",
                               :2plur "were",
                               :3sing "was",
                               :2sing "were"},
                        :agr {:person :3rd
                              :gender :fem
                              :pronoun false
                              :number :sing}
                        :english "be",
                        :cat :verb,
                        :infl :present,
                        :present {:1sing "am",
                                  :3plur "are",
                                  :1plur "are",
                                  :2plur "are",
                                  :3sing "is",
                                  :2sing "are"}}}]
    (is (= (morph form)
           "is"))))

(deftest generate-irregular-present-3
  (let [form {:english {:a {:infl :present,
                            :past {:2sing "were",
                                   :1sing "was",
                                   :3sing "was",
                                   :3plur "were",
                                   :2plur "were",
                                   :1plur "were"},
                            :agr {:gender :masc,
                                  :person :3rd,
                                  :number :sing},
                            :cat :verb,
                            :present {:2sing "are",
                                      :1sing "am",
                                      :3sing "is",
                                      :3plur "are",
                                      :2plur "are",
                                      :1plur "are"},
                            :english "be"},
                        :b {:pronoun false,
                            :agr {:gender :masc,
                                  :person :3rd,
                                  :number :sing},
                            :cat :noun,
                            :english "Juan"}}}]
    (is (= (morph form)
           "is Juan"))))

(deftest generate-irregular-imperfect
  (let [form {:english
              {:a {:english "you all",
                   :agr {:person :2nd, :gender :fem, :number :plur},
                   :cat :noun, :pronoun true},
               :b {:past "went downstairs",
                   :participle "going downstairs"
                   :present {:3sing "goes downstairs"},
                   :english "go downstairs",
                   :cat :verb,
                   :infl :imperfect,
                   :agr {:person :2nd, :gender
                         :fem, :number
                         :plur}}}}]
    (let [surface-form (morph form)]
      (is (or (= surface-form
                 "you all were going downstairs")
              (= surface-form
                 "you all used to go downstairs"))))))

(deftest present-tense-parse
  (let [parses (parse "she sleeps")]
    (is (not (empty? parses)))
    (is (= (get-in (first parses) [:synsem :sem :tense])
           :present))))

(deftest future-tense-parse
  (let [parses (parse "he will speak")]
    (is (not (empty? parses)))
    (is (= (get-in (first parses) [:synsem :sem :tense])
           :future))))

(deftest conditional-parse
  (is (not (empty? (parse "she would speak")))))

(deftest imperfect-parse
  (let [parses (parse "she used to speak")]
    (is (not (empty? parses)))
    (is (= (get-in (first parses) [:synsem :sem :tense])
           :past))
    (is (= (get-in (first parses) [:synsem :sem :aspect])
           :progressive)))
  (let [parses (parse "she was loving")]
    (is (not (empty? parses)))
    (is (= (get-in (first parses) [:synsem :sem :tense])
           :past))
    (is (= (get-in (first parses) [:synsem :sem :aspect])
           :progressive)))
  (let [parses (parse "she was speaking")]
    (is (not (empty? parses)))
    (is (= (get-in (first parses) [:synsem :sem :tense])
           :past))
    (is (= (get-in (first parses) [:synsem :sem :aspect])
           :progressive))))

(deftest parse-with-gender-symbols
  (is (not (empty? (parse "I (♀) speak"))))
  (is (not (empty? (parse "I (♂) speak"))))
  (is (not (empty? (parse "you (♀) speak"))))
  (is (not (empty? (parse "you (♂) speak"))))
  (is (not (empty? (parse "you all (♀) speak"))))
  (is (not (empty? (parse "you all (♂) speak")))))

(deftest go-upstairs
  (is (not (empty? (parse "I go upstairs"))))
  (is (not (empty? (parse "you go upstairs"))))
  (is (not (empty? (parse "she goes upstairs"))))
  (is (not (empty? (parse "we go upstairs"))))
  (is (not (empty? (parse "you all go upstairs"))))
  (is (not (empty? (parse "they go upstairs")))))

(deftest be-called
  (is (= 10
         (count
          (filter #(not (= % ""))
                  (take
                   10
                   (repeatedly #(let [expression (generate {:modified false
                                                            :synsem {:cat :verb
                                                                     :sem {:pred :be-called
                                                                           :obj {:top :top}}
                                                                     :subcat []}})]
                                  (is (not (= "" (morph expression))))
                                  (log/info (display-expression expression))
                                  (morph expression)))))))))

(deftest mod-is-empty-list
  (let [result (generate {:modified false
                          :synsem {:cat :noun
                                   :sem {:pred :name}
                                   :mod []}})]
    (is (= (get-in result [:synsem :mod]
                   ::undefined-should-not-return-this)
           []))))

(deftest her-name-is-luisa
  (is (= "her name is Luisa"
         (morph (generate {:modified false
                           :synsem {:cat :verb
                                    :sem {:iobj {:pred :luisa}
                                          :pred :be-called
                                          :subj {:pred :lei}
                                          :tense :present}
                                    :subcat []}
                           :comp {:synsem {:mod []}}})))))

(deftest jean-s
  (is (not (empty? (parse "Jean's")))))

(deftest the-dog-s-house
  (is (not (empty? (parse "the dog's house")))))

(deftest the-dogs-house
  (is (not (empty? (parse "the dogs' house")))))

(deftest generate-with-possessive-1
  (let [result
        (generate {:synsem {:pronoun false
                            ;; TODO: subcat [] should be part of language model's top-level generate defaults;
                            ;; c.f. TODO on babel.test.translate/latin-to-english
                            :subcat []
                            :cat :noun
                            :sem {:mod []
                                  :number :sing
                                  :spec {:pred :of
                                         :of {:pred :Juana}}
                                  :pred :dog}}})]
    (is (not (nil? result)))
    (is (= "Juana's dog" (morph result)))))

(deftest generate-with-possessive-2
  (let [result
        (generate {:synsem {:cat :noun
                            :pronoun false
                            :sem {:number :sing
                                  :spec {:pred :of
                                         :of {:pred :Juana}}
                                  :pred :dog}
                            :subcat []}
                   :head {:synsem {:mod {:first {:pred :red}
                                         :rest []}}}})]
    (is (not (nil? result)))
    (is (= "Juana's red dog" (morph result)))))

(deftest no-failed-bolts
  (let [result
        (->>
         (repeatedly #(let [generated
                            (generate 
                             {:comp {:synsem {:pronoun true}}
                              :modified false
                              :synsem {:sem {:pred :wash-oneself
                                             :reflexive true}}})]
                        {:f (english/morph generated :model model :from-language "it")
                         :sem (get-in generated [:synsem :sem :mod])}))
         (take 5))]
    (= (count result) 5)))

(deftest in-front-of
  (let [expr (generate {:synsem {:cat :prep
                                 :subcat []
                                 :reflexive false
                                 :sem {:pred :in-front-of
                                       :obj {:pred :table
                                             :mod []
                                             :number :sing
                                             :spec {:def :def
                                                    :pred :definite}}}}
                        :comp {:synsem {:mod []}}})]
    (is (= (morph expr)
           "in front of the table"))))

(defn sentences [n]
  (let [specs
        [

         {:phrasal true
          :synsem {:subcat []}
          :head {:phrasal false}
          :comp {:phrasal false}}

         {:phrasal true
          :synsem {:subcat []}
          :head {:phrasal true
                 :head {:phrasal false}}
          :comp {:phrasal false}}

         {:phrasal true
          :synsem {:subcat []}
          :head {:phrasal true
                 :head {:phrasal false}}
          :comp {:phrasal false}}

         {:phrasal true
          :synsem {:subcat []}
          :head {:phrasal true
                 :head {:phrasal false}}
          :comp {:phrasal true}}

         {:phrasal true
          :synsem {:subcat []}
          :head {:phrasal true
                 :head {:phrasal true}}
          :comp {:phrasal true}}

         
         ]]
    (doall (take (Integer. n)
                 (repeatedly #(btest/sentences 1
                                               model
                                               (first (shuffle specs))))))))
(deftest furniture-sentence
  (let [expr (generate {:modified false
                        :synsem {:cat :verb
                                 :sem {:obj {:pred :table
                                             :mod []
                                             :number :sing
                                             :spec {:def :def
                                                    :pred :definite}}
                                       :pred :in-front-of
                                       :tense :present
                                       :subj {:pred :chair
                                              :mod []
                                              :number :sing
                                              :spec {:def :def
                                                     :pred :definite}}}
                                 :subcat []}
                        :comp {:synsem {:mod []}}
                        :head {:comp {:comp {:synsem {:mod []}}}}})]
    (log/info (str "furniture-sentence: " 
                   (display-expression expr)))

    (is (= (morph expr)
           "the chair is in front of the table"))))

(deftest reflexive-furniture-sentence
  (let [expr (generate {:modified false
                        :synsem {:cat :verb
                                 :sem {:pred :in-front-of
                                       :tense :present
                                       :reflexive true
                                       :subj {:pred :chair
                                              :mod []
                                              :number :sing
                                              :spec {:def :def
                                                     :pred :definite}}}
                                 :subcat []}
                        :comp {:synsem {:mod []}}})]
    (log/info (str "reflexive furniture expression:" (display-expression expr)))
    (is (= (morph expr)
           "the chair is in front of itself"))))

(deftest infinitive-vp
  (let [vp-infinitive (:vp-infinitive (:grammar-map model))
        expr (-> vp-infinitive
                 (overh (get (:lexicon model) "speak"))
                 (overc (generate {:synsem {:sem {:pred :word
                                                  :mod []
                                                  :spec {:def :def
                                                         :pred :definite}}
                                            :cat :noun}})))]
    (is true)))

(deftest past-and-gender-agreement
  (= (morph (generate {:synsem {:sem {:pred :go
                                      :aspect :perfect
                                      :tense :present
                                      :subj {:gender :fem
                                             :pred :loro}}}}))
     "they (♀) went"))

(deftest noun-number-agreement
  (= (morph (generate {:synsem {:agr {:number :plur}
                                :cat :noun
                                :sem {:pred :cat :spec {:def :def}
                                      :mod []}}}))
     "the cats"))

(deftest phrasal-verbs
  (is (not (empty? (parse "I turned on the radio"))))
  (is (not (empty? (parse "I turned the radio on"))))
  (is (not (empty? (parse "I turned off the radio"))))
  (is (not (empty? (parse "I turned the radio off"))))
  (let [spec {:synsem {:cat :verb
                       :subcat []
                       :sem {:pred :turn-on
                             :tense :present
                             :obj {:mod []
                                   :number :sing
                                   :pred :radio
                                   :spec {:def :def
                                          :of {:pred nil}}}
                             :subj {:pred :lei
                                    :prop {:human true}}}}}
        generated (generate spec)]
    (log/info (str "generated: " (morph generated)))
    (is (or (= "she turns the radio on"
               (morph generated))
            (= "she turns on the radio"
               (morph generated))))))

;; cats cannot read: generating with this spec
;; should quickly return with nil (rather than
;; running indefinitely trying to generate a matching
;; expression).
(def spec1 {:synsem {:cat :verb
                     :sem {:pred :read
                           :subj {:pred :woman}}
                     :subcat []}})

(def spec2 {:synsem {:cat :verb
                     :sem {:pred :read
                           :subj {:pred :cat}}
                     :subcat []}})

(deftest take-advantage-present
  (let [result (generate {:synsem {:sem {:pred :take-advantage-of
                                         :tense :present
                                         :obj :unspec
                                         :aspect :progressive
                                         :subj {:pred :I}}}})]
    (not (nil? result))))

(deftest generate-with-put
  (let [result (generate {:synsem {:sem {:obj :unspec
                                         :subj {:pred :gianluca}, 
                                         :pred :put}
                                   :cat :verb}})]
    (is (not (nil? result)))))

(deftest generate-with-sleep
  (let [result (generate {:synsem {:sem {:obj :unspec
                                         :subj {:pred :gianluca}, 
                                         :pred :sleep}
                                   :cat :verb}})]
    (is (not (nil? result)))))

(deftest generate-with-wash-oneself-1
  (let [result (generate {:synsem {:sem {:subj {:pred :gianluca}, 
                                         :pred :wash-oneself
                                 ;;        :reflexive true
                                         ;;:obj {:pred :gianluca}
                                         }
                                   :cat :verb}})]
    (is (not (nil? result)))))

(deftest generate-with-comb-oneself-1
  (let [result (generate {:synsem {:sem {:subj {:pred :gianluca}, 
                                         :pred :comb-oneself
                                 ;;        :reflexive true
                                         ;;:obj {:pred :gianluca}
                                         }
                                   :cat :verb}})]
    (is (not (nil? result)))))

(deftest generate-with-comb-oneself-2
  (let [result (generate {:synsem {:sem {:subj {:gender :masc, :number :sing, :null false, :animate true, :pred :gianluca,
                                                :human true},
                                         :pred :comb-oneself,
                                         :reflexive true,
                                         :obj {:gender :masc, :number :sing, :null false,
                                               :animate true, :pred :gianluca, :human true},
                                         :aspect :progressive, :tense :past}}})]
    (is (not (nil? result)))))

(deftest past-simple
  (let [result (generate {:root {:english {:english "drink"}}
                          :modified false
                          :synsem {:cat :verb
                                   :subcat ()
                                   :sem {:aspect :perfect
                                         :obj :unspec
                                         :subj {:pred :I}
                                         :tense :present}}})]
    (is (not (nil? result)))
    (is (= "I drank" (morph result)))))

(deftest pluperfect-irregular ;; c.f.: babel.test.it/trapassato-prossimo
  (let [result (generate {:root {:english {:english "drink"}}
                          :modified false
                          :synsem {:cat :verb
                                   :subcat ()
                                   :sem {:aspect :pluperfect
                                         :obj :unspec
                                         :subj {:pred :I}
                                         :tense :past}}})]
    (is (not (nil? result)))
    (is (= "I had drunk" (morph result)))))


(deftest pluperfect-regular-with-irregular-past ;; c.f.: babel.test.it/trapassato-prossimo
  (let [result (generate {:root {:english {:english "study"}}
                          :modified false
                          :synsem {:cat :verb
                                   :subcat []
                                   :sem {:aspect :pluperfect
                                         :obj :unspec
                                         :subj {:pred :I}
                                         :tense :past}}})]
    (is (not (nil? result)))
    (is (= "I had studied" (morph result)))))

(deftest pluperfect-regular ;; c.f.: babel.test.it/trapassato-prossimo
  (let [result (generate {:root {:english {:english "talk"}}
                          :modified false
                          :synsem {:cat :verb
                                   :subcat []
                                   :sem {:aspect :pluperfect
                                         :iobj :unspec
                                         :obj :unspec
                                         :subj {:pred :I}
                                         :tense :past}}})]
    (is (not (nil? result)))
    (is (= "I had talked" (morph result)))))

(deftest pluperfect-born
  (let [result (generate {:root {:english {:english "be born"}}
                          :modified false
                          :synsem {:cat :verb
                                   :subcat ()
                                   :sem {:aspect :pluperfect
                                         :obj :unspec
                                         :subj {:pred :I}
                                         :tense :past}}})]
    (is (not (nil? result)))
    (is (= "I had been born" (morph result)))))

(deftest present-progressive-ends-with-e
  (let [spec1 {:modified false
               :root {:english {:english "participate"}}
               :synsem {:cat :verb
                        :subcat []
                        :sem {:tense :present
                              :aspect :progressive
                              :subj {:pred :I}}}}
        spec2 {:modified false
               :root {:english {:english "hope"}}
               :synsem {:cat :verb
                        :subcat []
                        :sem {:tense :present
                              :aspect :progressive
                              :subj {:pred :I}
                              :obj :unspec}}}]
    
    (is (= "I am participating (right now)" (morph (generate spec1) :show-notes true)))
    (is (= "I am hoping (right now)" (morph (generate spec2) :show-notes true)))))
  
(deftest past-participle-orthography
  (is (= (babel.english.morphology/present-participle-of "hope") "hoping"))
  (is (= (babel.english.morphology/present-participle-of "hop") "hopping"))
  (is (= (babel.english.morphology/present-participle-of "knot") "knotting"))
  (is (= (babel.english.morphology/present-participle-of "note") "noting")))

(deftest present-progressive-vs-present-simple
  (let [base-spec {:modified false
                   :synsem {:cat :verb
                            :subcat ()
                            :sem {:pred :eat
                                  :tense :present
                                  :subj {:pred :I}
                                  :obj :unspec}}}
        progressive (unify base-spec
                           {:synsem {:sem {:aspect :progressive}}})
        simple (unify base-spec
                      {:synsem {:sem {:aspect :simple}}})]
    ;; default should be simple present:
    (is (= "I eat" (morph (generate base-spec))))

    ;; explicitly set to simple present:
    (is (= "I eat" (morph (generate simple)))) 

    ;; explicitly set to progressive present:
    (is (= "I am eating (right now)" (morph (generate progressive) :show-notes true)))))

(deftest benchmark-test []
  (let [med model
        to-run #(time (println (morph (generate
                                       {:synsem {:cat :verb, :sem {:pred :read
                                                                   :subj {:pred :woman}}
                                                 :subcat []}}))))]
    (is (= 1 (count (take 1 (repeatedly to-run)))))))

(deftest relative-clause []
  (let [parse (first (parse "the man you see"))]
    (is (not (nil? parse)))
    (is (= (u/get-in parse [:synsem :cat])
           :noun))
    (is (= (u/get-in parse [:synsem :sem :pred])
           :man))
    (is (= (u/get-in parse [:synsem :sem :mod :first :subj :pred])
           :tu))
    (is (= (u/get-in parse [:synsem :sem :mod :first :pred])
           :see))))

(deftest generate-for-italian
  (let [p (->> (parse "I speak")
               (filter #(and (= :verb (u/get-in % [:synsem :cat]))
                             (= [] (u/get-in % [:synsem :subcat]))))
               first)
        spec (strip-refs (-> (u/assoc-in
                              {:synsem {:cat :verb
                                        :subcat []}}
                              [:synsem :sem]
                              (unify
                               {:subj {:gender :masc}}
                               (u/get-in p [:synsem :sem])))))
        generated (generate spec)
        surface-1 (morph generated)
        surface-2 (english/morph generated :model model :from-language "it")]
    (is (not (nil? p)))
    (is (not (fail? spec)))
    (is (not (nil? generated)))
    (is (= "I speak" surface-1))
    (is (= "I (♂) speak" surface-2))))

(deftest irregular-present-progressive
  (let [spec {:modified false
              :root {:english {:english "get dressed"}}
              :comp {:synsem {:agr {:gender :fem}}}
              :synsem {:cat :verb
                       :subcat ()
                       :sem {:tense :present
                             :aspect :progressive
                             :subj {:pred :I}}}}]
    (is (= "I am getting dressed"
           (english/morph (generate spec)
                          :model model
                          :show-notes false)))
    (is (= "I am getting dressed (right now)"
           (english/morph (generate spec)
                          :model model
                          :show-notes true)))
    (is (= "I (♀) am getting dressed (right now)"
           (english/morph (generate spec)
                          :model model
                          :show-notes true
                          :from-language "it")))))

(deftest generate-as-writer-does
  (= "I (♀) speak"
     ((:morph model) (babel.generate/generate {:synsem {:cat :verb
                                                        :sem {:pred :speak
                                                              :tense :present}}
                                               :comp {:synsem {:agr {:gender :fem}
                                                               :sem {:pred :I}}}}
                                              model)
      :from-language "it")))

;; generate "the woman she sees"
(def spec-for-the-woman-she-sees
  {:synsem {:agr {:number :sing}
            :pronoun false
            :cat :noun
            :subcat []
            :sem {:pred :woman
                  :spec {:def :def}
                  :mod {:first {:pred :see
                                :tense :present
                                :subj {:pred :lei
                                       :mod []
                                       :prop {:human true}}}
                        :rest []}}}})

(deftest generate-with-relative-clause
  (let [result (generate spec-for-the-woman-she-sees)]
    (is (or
         (= "the woman she sees"
            (morph result))
         (= "the woman that she sees"
            (morph result))))))

(deftest generate-from-vocab-items
  (let [vocab-items [{:surface "acre", :pred "acre", :vocab_cat "noun1"}
                     {:surface "county", :pred "county", :vocab_cat "noun1"}
                     {:surface "damage", :pred "damage", :vocab_cat "noun1"}
                     {:surface "fire", :pred "fire", :vocab_cat "noun1"}
                     {:surface "flame", :pred "flame", :vocab_cat "noun1"}
                     {:surface "industry", :pred "industry", :vocab_cat "noun1"}
                     {:surface "wine", :pred "wine", :vocab_cat "noun1"}]
        filter-lexicon-fn #(= :det (get-in % [:synsem :cat]))
        original-model @@(get babel.directory/models :en)
        new-model ((:vocab2model original-model) vocab-items filter-lexicon-fn)]
    (let [expression (generate/generate {:synsem {:subcat []
                                                  :cat :noun}}
                                        new-model)]
      (is (not (nil? expression))))))

(def spec
  {:synsem {:cat :verb
            :sem {:pred :sleep
                  :subj {:pred :cat}}
            :subcat []}})

(def cspec {:modified false
            :synsem {:cat :verb
                     :sem {:mod ()
                           :iobj {:pred :luisa}
                           :pred :be-called
                           :subj {:pred :lei
                                  :human true
                                  :number :sing} :tense :present}
                     :subcat ()}})

;; TODO: use these examples in tests
;; An expensive and also one with a wide
;; divergence in timing: e.g. could be
;; as fast as 0.433 sec or as slow as 1.7 sec.
(def foo
  (take 
   10 
   (repeatedly 
    #(println 
      (morph 
       (time (generate 
              {:synsem {:subcat []
                        :cat :verb
                        :sem {:pred :be-called
                              :subj {:pred :tu}
                              :obj {:top :top}}}})))))))



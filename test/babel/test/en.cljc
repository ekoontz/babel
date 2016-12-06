(ns babel.test.en
  (:refer-clojure :exclude [assoc-in get-in])
  (:require [babel.directory :refer [models]]
            [babel.english :refer [analyze fo-ps generate medium morph parse sentences]]
            [babel.english.grammar :as grammar]
            [babel.english.morphology :refer [fo get-string]]

            [babel.generate :refer [add-comps
                                    get-lexemes
                                    lightning-bolts]]
            
            [babel.over :refer [overc overh]]
            
            ;; TODO: add parsing tests
            [babel.parse :as parse]

            [clojure.repl :refer [doc]]
            [clojure.string :as string]
            #?(:clj [clojure.test :refer [deftest is]])
            #?(:cljs [cljs.test :refer-macros [deftest is]])
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [babel.logjs :as log]) 
            [dag_unify.core :refer [assoc-in fail? fail-path-between get-in strip-refs unify]]))

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
    (is (= (fo form)
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
    (is (= (fo form)
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
    (is (= (fo form)
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
    (let [surface-form (fo form)]
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
                                                                     :sem {:pred :be-called}}})]
                                  (is (not (= "" (fo expression))))
                                  (log/info (display-expression expression))
                                  (fo expression)))))))))

(deftest mod-is-empty-list
  (let [result (generate {:modified false
                          :synsem {:cat :noun
                                   :sem {:pred :name
                                         :mod '()
                                         }}})]
    (is (= (get-in result [:synsem :sem :mod]
                   ::undefined-should-not-return-this)
           '()))))

(deftest her-name-is-luisa
  (is (= "her name is Luisa"
         (fo (generate {:modified false
                        :synsem {:cat :verb
                                 :sem {:mod '()
                                       :iobj {:pred :luisa}
                                       :pred :be-called
                                       :subj {:pred :lei}
                                       :tense :present}}}
                       :truncate false)))))
                                       
(deftest jean-s
  (is (not (empty? (parse "Jean's")))))

(deftest the-dog-s-house
  (is (not (empty? (parse "the dog's house")))))

(deftest the-dogs-house
  (is (not (empty? (parse "the dogs' house")))))

(deftest generate-with-possessive-1
  (let [result
        (generate {:synsem {:cat :noun
                            :sem {:number :sing
                                  :mod '()
                                  :spec {:pred :of
                                         :of {:pred :Juana}}
                                  :pred :dog}}})]
    (is (not (nil? result)))
    (is (= "Juana's dog" (fo result)))))

(deftest generate-with-possessive-2
  (let [result
        (generate {:synsem {:cat :noun
                            :sem {:mod {:pred :rosso}
                                  :number :sing
                                  :spec {:pred :of
                                         :of {:pred :Juana}}
                                  :pred :dog}}})]
    (is (not (nil? result)))
    (is (= "Juana's red dog" (fo result)))))

(deftest no-failed-bolts
  (let [result
        (->>
         (repeatedly #(let [generated
                            (generate 
                             {:comp {:synsem {:pronoun true}}
                              :modified false
                              :synsem {:sem {:pred :wash
                                             :mod nil
                                             :reflexive true}}})]
                        {:f (fo generated :from-language "it")
                         :sem (get-in generated [:synsem :sem :mod])}))
         (take 5))]
    (= (count result) 5)))

(deftest in-front-of
  (let [expr (generate {:synsem {:cat :prep
                                 :reflexive false
                                 :sem {:pred :in-front-of
                                       :obj {:pred :table
                                             :mod '()
                                             :number :sing
                                             :spec {:def :def
                                                    :pred :definite}}}}})]
    (is (= (fo expr)
           "in front of the table"))))

(deftest furniture-sentence
  (let [expr (generate {:modified false
                        :synsem {:cat :verb
                                 :sem {:obj {:pred :table
                                             :mod '()
                                             :number :sing
                                             :spec {:def :def
                                                    :pred :definite}}
                                       :pred :in-front-of
                                       :tense :present
                                       :subj {:pred :chair
                                              :mod '()
                                              :number :sing
                                              :spec {:def :def
                                                     :pred :definite}}}}})]
    (log/info (str "furniture-sentence: " 
                   (display-expression expr)))

    (is (= (fo expr)
           "the chair is in front of the table"))))

(deftest reflexive-furniture-sentence
  (let [expr (generate {:modified false
                        :synsem {:cat :verb
                                 :sem {:pred :in-front-of
                                       :tense :present
                                       :reflexive true
                                       :subj {:pred :chair
                                              :mod '()
                                              :number :sing
                                              :spec {:def :def
                                                     :pred :definite}}}}})]
    (log/info (str "reflexive furniture expression:" (display-expression expr)))
    (is (= (fo expr)
           "the chair is in front of itself"))))

(deftest infinitive-vp
  (let [medium (medium)
        vp-infinitive (:vp-infinitive (:grammar-map medium))
        expr (-> vp-infinitive
                 (overh (get (:lexicon medium) "speak"))
                 (overc (generate {:synsem {:sem {:pred :word
                                                  :mod '()
                                                  :spec {:def :def
                                                         :pred :definite}}
                                            :cat :noun}})))]
    (is true)))

(deftest past-and-gender-agreement
  (= (fo (generate {:synsem {:sem {:pred :go
                                   :aspect :perfect
                                   :tense :present
                                   :subj {:gender :fem
                                          :pred :loro}}}}))
     "they (♀) went"))

(deftest noun-number-agreement
  (= (fo (generate {:synsem {:agr {:number :plur}
                             :cat :noun
                             :sem {:pred :cat :spec {:def :def}
                                   :mod '()}}}))
     "the cats"))

(deftest phrasal-verbs
  (is (not (empty? (parse "I turned on the radio"))))
  (is (not (empty? (parse "I turned the radio on"))))
  (is (not (empty? (parse "I turned off the radio"))))
  (is (not (empty? (parse "I turned the radio off"))))
  (let [generated (morph (generate {:synsem {:sem {:pred :turn-on
                                                   :tense :present
                                                   :obj {:mod '()
                                                         :number :sing
                                                         :pred :radio
                                                         :spec {:def :def
                                                                :of {:pred nil}}}
                                                   :subj {:pred :lei}}
                                             :cat :verb}}))]
    (is (or (= "she turns the radio on"
                generated)
            (= "she turns on the radio"
               generated)))))

;; cats cannot read: generating with this spec
;; should quickly return with nil (rather than
;; running indefinitely trying to generate a matching
;; expression).
(def spec1 {:synsem {:cat :verb
                     :sem {:pred :read
                           :subj {:pred :woman}}
                     :subcat '()}})

(def spec2 {:synsem {:cat :verb
                     :sem {:pred :read
                           :subj {:pred :cat}}
                     :subcat '()}})

(deftest rathole-check-1
  (let [med (medium)
        spec {:synsem {:cat :verb
                       :sem {:pred :read
                             :subj {:pred :cat}}
                       :subcat '()}}
        lbs (babel.generate/lightning-bolts med spec 0 6)
        good-lb (first (filter #(and (= (get-in % [:head :rule])
                                        "transitive-vp-nonphrasal-head")
                                     (= (get-in % [:head :head :english :english])
                                        "read"))
                               lbs))

        comp-map (comp-paths-to-bolts-map good-lb med 0 6)]
    (is (some empty? (vals comp-map)))))

(deftest rathole-check-2
  (let [med (medium)
        spec {:synsem {:cat :verb
                       :sem {:pred :read
                             :subj {:pred :woman}}
                       :subcat '()}}
        lbs (lightning-bolts med spec 0 6)
        good-lb (first (filter #(and (= (get-in % [:head :rule])
                                        "transitive-vp-nonphrasal-head")
                                     (= (get-in % [:head :head :english :english])
                                        "read"))
                               lbs))
        comp-comp-spec (get-in good-lb [:comp :comp])
        comp-spec (get-in good-lb [:comp])]

    (is (not (empty? (babel.generate/lightning-bolts med (get-in good-lb [:head :comp]) 0 0))))
    (is (not (empty? (babel.generate/lightning-bolts med (get-in good-lb [:comp]) 0 0))))))

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
                                   :subcat ()
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
                                   :subcat ()
                                   :sem {:aspect :pluperfect
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

(deftest irregular-present-progressive
  (let [spec {:modified false
              :root {:english {:english "get dressed"}}
              :synsem {:cat :verb
                       :subcat ()
                       :sem {:tense :present
                             :aspect :progressive
                             :subj {:pred :I}}}}]
    (is (= "I am getting dressed" (morph (generate spec))))))

(deftest present-progressive-ends-with-e
  (let [spec1 {:modified false
              :root {:english {:english "participate"}}
              :synsem {:cat :verb
                       :subcat ()
                       :sem {:tense :present
                             :aspect :progressive
                             :subj {:pred :I}}}}
        spec2 {:modified false
               :root {:english {:english "hope"}}
               :synsem {:cat :verb
                        :subcat ()
                        :sem {:tense :present
                              :aspect :progressive
                              :subj {:pred :I}
                              :obj :unspec}}}]
                                     
    (is (= "I am participating" (morph (generate spec1))))
    (is (= "I am hoping" (morph (generate spec2))))))
  
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
    (is (= "I am eating" (morph (generate progressive))))))

    
(deftest lb1
  (let [comp-paths (babel.generate/find-comp-paths good-lb)
        paths-to-specs (zipmap
                        comp-paths
                        (map #(get-in good-lb %)
                             comp-paths))
        paths-to-lbs
        (zipmap
         comp-paths
         (map #(babel.generate/lightning-bolts
                med
                (get-in good-lb %)
                0 0)
              comp-paths))]

    (is (not (empty? (get paths-to-lbs [:head :comp]))))
    (is (not (empty? (get paths-to-lbs [:comp]))))))

(deftest benchmark []
  (let [med (medium)
        to-run #(time (println (fo (generate
                                    {:synsem {:cat :verb, :sem {:pred :read
                                                                :subj {:pred :woman}}
                                              :subcat '()}}))))]
    (is (= 50 (count (take 50 (repeatedly to-run)))))))

(defn over-vals [key vals]
  (map (fn [val]
         {key val})
       vals))


(def as [{:a 1}{:a 2}])
(def bs [{:b 4}{:b 5}])
(def cs [{:c 6}{:c 7}{:c 8}])

(defn foo [n]
  (take n
        (->> as
             (mapcat (fn [a] 
                       (->> bs
                            (mapcat (fn [b]
                                      (->> cs
                                           (map (fn [c]
                                                  (->> c
                                                       (reduce merge b)
                                                       (reduce merge a)))))))))))))




       
    

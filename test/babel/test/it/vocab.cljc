(ns babel.test.it.vocab
  (:require
   [babel.directory :refer [models]]
   [babel.generate :refer [generate]]   
   [babel.test.it :as it-test :refer [create-model]]
   [dag_unify.core :as u]
   [clojure.test :as realtest :refer [deftest is]]
   [babel.italiano :as it :refer [morph]]
   [babel.italiano.lexicon :as lexicon :refer [edn2lexicon vocab-entry-to-lexeme]]
   [babel.lexiconfn :as l :refer [filtered-lexicon write-lexicon]]))

(defn setup []
  (let [inquilino {:surface "inquilino" :pred "male tenant" :vocab_cat "noun1"}
        buffo     {:surface "buffo"     :pred "funny"       :vocab_cat "adj1"}

        male-tenant {:surface "male tenant"
                     :structure {:sets []
                                 :synsem {:cat "unspec"}
                                 :english {:english "male tenant"}
                                 :phrasal false
                                 :italiano {:italiano "inquilino"}
                                 :vocabcoach-category "noun1"}
                     :pred "male tenant", :vocab_cat "noun1"}
        funny {:surface "funny"
               :structure {:sets [],
                           :synsem {:cat "unspec"}
                           :english {:english "funny"}
                           :phrasal false
                           :italiano {:italiano "buffo"}
                           :vocabcoach-category "adj1"}
               :pred "funny", :vocab_cat "adj1"}
        tutti {:surface "tutti"
               :pred "all"
               :vocab_cat "pre-det"}
        
        target-vocab-items [inquilino buffo tutti]
        source-vocab-items [male-tenant funny]]
    (is (= {"inquilino"
            [{:vocab-cat "noun1",
              :synsem
              {:sem {:pred :male-tenant}, :cat :noun, :agr {:gender :masc}}}]}
           (-> inquilino vocab-entry-to-lexeme)))

    (is (= {"buffo" [{:vocab-cat "adj1",
                      :synsem {:cat :adjective, :sem {:pred :funny, :comparative false}}}]}
           (-> buffo vocab-entry-to-lexeme)))
    ;; c.f.: babel.italiano.grammar/model-with-vocab-items
    ;; c.f.: verbcoach.question.babel/create-model
    (let [vocab-items [inquilino buffo tutti]
          input-lexicon (reduce merge (map vocab-entry-to-lexeme vocab-items))
          synthetic-noun (edn2lexicon input-lexicon)
          definite-articles? true
          possessive-articles? true
          adjectives? true
          filter-lexicon-fn (fn [lexeme]
                              (or
                               (and adjectives?
                                    (= :adjective (u/get-in lexeme [:synsem :cat])))
                               (and (= :det (u/get-in lexeme [:synsem :cat]))
                                    (or (and definite-articles?
                                             (= :def (u/get-in lexeme [:synsem :def]))
                                             (= :definite (u/get-in lexeme [:synsem :sem :pred]))
                                             (= nil (u/get-in lexeme [:synsem :sem :of :pred])))
                                        (and possessive-articles?
                                             (= :possessive (u/get-in lexeme [:synsem :def])))))))
          model @@(get models :it)
          new-lexicon (merge-with concat
                                  synthetic-noun
                                  (filtered-lexicon
                                   (:lexicon model)
                                   filter-lexicon-fn))]

      ;; cf verbcoach.question.babel/expression
      (let [base-target-model @@(get models :it)
            base-source-model @@(get models :en)
            generative-features {:definite-articles? true
                                 :possessive-articles? true
                                 :adjectives? true}
            filter-lexicon-fn
            (fn [lexeme]
              (or
               (and (= :det (u/get-in lexeme [:synsem :cat]))
                    (or (and definite-articles?
                             (= :def (u/get-in lexeme [:synsem :def]))
                             (= :definite (u/get-in lexeme [:synsem :sem :pred]))
                             (= nil (u/get-in lexeme [:synsem :sem :of :pred])))
                        (and possessive-articles?
                             (= :possessive (u/get-in lexeme [:synsem :def])))))))
            target-model (create-model base-target-model target-vocab-items filter-lexicon-fn)
            source-model (create-model base-source-model source-vocab-items filter-lexicon-fn)]
        [target-model source-model]))))

(def target-model (first (setup)))
(def source-model (second (setup)))

(defn analyze [str]
  (-> str (it/analyze target-model)))

(defn parse [str]
  (-> str (it/parse target-model)))

(defn syntax-tree [parse-tree]
  ((:morph-ps target-model) parse-tree))

(deftest buffo-analyze
  (is (seq (it/analyze "buffo" target-model))))

(def spec {:word-of-interest {:italiano {:italiano "buffo"}},
           :synsem {:cat :noun, :subcat ()}})

(deftest generate-with-model
  (is (seq
       (-> spec
           (babel.generate/generate target-model)
           it/morph))))

(deftest parse-with-model
  (is (seq (->> "il inquilino"
                parse
                (mapcat :parses))))
  (is (= (->> "il inquilino buffo"
              parse
              (mapcat :parses)
              (take 1)
              (map syntax-tree))
         '("[noun-phrase2  'il'  'inquilino buffo']")))
  (is (= (->> "tutti gli inquilini"
              parse
              (mapcat :parses)
              (take 1)
              (map syntax-tree))
         '("[tutti-phrase  'tutti'  'gli inquilini']"))))


  





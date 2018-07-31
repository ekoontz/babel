(ns babel.english.grammar
  (:refer-clojure :exclude [get-in])
  (:require 
   [babel.english.lexicon :refer [deliver-lexicon transform-with-english-lexical-rules
                                  vocab-entry-to-lexeme]]
   [babel.english.morphology :refer (analyze fo)]
   [babel.generate :as generate]
   [babel.html :refer [local-timestamp]]
   [babel.index :refer [create-indices lookup-spec]]
   [babel.lexiconfn :refer [edn2lexicon filtered-lexicon read-lexicon]]
   [babel.parse :as parse]
   [babel.ug :as ug
    :refer [apply-default-if comp-modifies-head
            comp-specs-head head-semantics
            head-principle root-is-comp
            root-is-head root-is-head-root
            subcat-1-principle subcat-1-1-principle
            subcat-1-1-principle-comp-subcat-1
            subcat-2-principle subcat-2-2-principle
            subcat-5-principle unify-check
            verb-default?]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [clojure.core.cache :as cache]
   [dag_unify.core :refer [fail? get-in remove-matching-keys strip-refs unify]]))

(def index-lexicon-on-paths
  [[:synsem :agr :gender]
   [:synsem :agr :number]
   [:synsem :agr :person]
   [:synsem :aux]
   [:synsem :cat]
   [:synsem :pronoun]
   [:synsem :sem :pred]
   [:synsem :sem :human]])

(defn noun-default? [tree]
  (and (= :noun (get-in tree [:synsem :cat]))
       (= :top (get-in tree [:synsem :agr :number] :top))))

(defn default-fn [tree]
  (log/debug (str "English: do-defaults (pre) on tree: " (parse/fo-ps tree fo)))
  (log/trace (str "aspect (pre): " (strip-refs (get-in tree
                                                       [:synsem :sem :aspect]
                                                       ::unset))))
  (log/trace (str "infl   (pre): " (strip-refs (get-in tree
                                                       [:synsem :infl]
                                                       ::unset))))  
  (log/trace (str "tense  (pre): " (strip-refs (get-in tree
                                                       [:synsem :sem :tense]
                                                       ::unset))))
  (let [result
        (-> tree
            (apply-default-if
             noun-default?
             {:synsem {:agr {:number :sing}}})
            
            (apply-default-if
             verb-default?
             {:synsem {:cat :verb
                       :sem {:tense :present
                             :aspect :simple}
                       :infl :present}})

            (apply-default-if
             verb-default?
             {:synsem {:cat :verb
                       :sem {:tense :present
                             :aspect :progressive}
                       :infl :present-progressive}})

            (apply-default-if
             verb-default?
             {:synsem {:cat :verb
                       :sem {:tense :future}
                       :infl :future}})

            (apply-default-if
             verb-default?
             {:synsem {:cat :verb
                       :sem {:tense :conditional}
                       :infl :conditional}})

            ;; past progressive
            (apply-default-if
             verb-default?
             {:synsem {:cat :verb
                       :sem {:aspect :progressive
                             :tense :past}
                       :infl :imperfect}})

            ;; simple past
            (apply-default-if
             verb-default?
             {:synsem {:cat :verb
                       :sem {:aspect :perfect
                             :tense :present}
                       :infl :past}})

            ;; pluperfect
            (apply-default-if
             verb-default?
             {:synsem {:cat :verb
                       :sem {:aspect :pluperfect
                             :tense :past}
                       :infl :pluperfect}}))]
    (log/debug (str "English: do-defaults (post) on tree: " (parse/fo-ps result fo)))
    (log/trace (str "aspect (post): " (strip-refs (get-in result
                                                         [:synsem :sem :aspect]
                                                         ::unset))))
    (log/trace (str "infl   (post): " (strip-refs (get-in result
                                                          [:synsem :infl]
                                                          ::unset))))  
    (log/trace (str "tense  (post): " (strip-refs (get-in result
                                                          [:synsem :sem :tense]
                                                          ::unset))))
    [result]))

(declare cache)
(declare model-with-vocab-items)

(defn fo-ps [expr]
  (parse/fo-ps expr fo))

(def hc-agreement
  (let [agr (atom :top)]
    {:synsem {:agr agr}
     :head {:synsem {:agr agr}}
     :comp {:english {:agr agr}
            :synsem {:agr agr}}}))

(def head-first
  (let [agr (atom :top)
        head-english (atom {:agr agr})
        comp-english (atom :top)]
    {:head {:english head-english}
     :comp {:english comp-english}
     :english {:a head-english
               :agr agr
               :b comp-english}
     :first :head}))
               
(def head-last
  (let [agr (atom :top)
        head-english (atom {:agr agr})
        comp-english (atom :top)]

    {:head {:english head-english}
     :comp {:english comp-english}
     :english {:a comp-english
               :agr agr
               :b head-english}
     :first :comp}))

;; -- BEGIN SCHEMA DEFINITIONS
(def c10
  (unify-check
   ug/c10
   head-last))

(def c00
  (unify-check
   head-last
   {:comment "c00"
    :schema-symbol 'c00 ;; used by over-each-parent to know where to put children.
    :first :comp
    :comp {:synsem {:subcat []}}
    :head {:synsem {:subcat []}}}))

(def h00
  (unify-check
   head-first
   {:comment "h00"
    :schema-symbol 'h00 ;; used by over-each-parent to know where to put children.
    :comp {:synsem {:subcat []}}
    :head {:synsem {:subcat []}}}))

(def c11
  (unify-check
   subcat-1-1-principle
   hc-agreement
   head-principle
   comp-modifies-head
   head-last
   {
    :schema-symbol 'c11 ;; used by over-each-parent to know where to put children.
    :comment "c11"}))

;; TODO: names like "c11-comp-subcat-1" have no human-discernible meaning:
;; sound like droid names or something..
(def c11-comp-subcat-1
  (let [subcat (atom :top)]
    (unify-check
     {:head {:synsem {:subcat {:1 subcat}}}
      :comp {:synsem {:subcat {:1 subcat}}}}
     subcat-1-1-principle-comp-subcat-1
     hc-agreement
     head-principle
     head-last
     {:schema-symbol 'c11-comp-subcat-1
      :first :comp
      :comment "c11-comp-subcat-1"})))

(def h10
  (unify-check
   subcat-1-principle
   head-principle
   head-first
   {:comment "h10"
    :schema-symbol 'h10 ;; used by over-each-parent to know where to put children.
    }))

(def h21
  (unify-check
   subcat-2-principle
   head-principle
   head-first
   {:comment "h21"
    :schema-symbol 'h21})) ;; used by over-each-parent to know where to put children.

(def h22
  (unify-check
   subcat-2-2-principle
   head-principle
   head-first
   {:comment "h22"
    :schema-symbol 'h22 ;; used by over-each-parent to know where to put children.
    }))

(def h32
  (unify-check
   subcat-5-principle
   head-principle
   head-first
   {:comment "h32"
    :schema-symbol 'h32 ;; used by over-each-parent to know where to put children.
    }))

;; </TODO: move to ug>
;; -- END SCHEMA DEFINITIONS

(def modified {:modified true})
(def unmodified {:modified false})

(def grammar
  [(unify-check h21
                {:rule "adjective-phrase"
                 :synsem {:cat :adjective}})
   
   (unify-check h21
                (let [head-synsem {:cat :intensifier}]
                  {:rule "intensifier-phrase"
                   :synsem head-synsem}))
   
   (unify-check h10
                (let [semantics (atom :top)
                      comp-type (atom :top)]
                  {:rule "complementizer-phrase-nonslash"
                   :synsem {:cat :comp
                            :comp-type comp-type
                            :sem semantics}
                   :comp {:synsem {:slash false}}
                   :head {:synsem {:comp-type comp-type
                                   :sem semantics}}}))
   (unify-check
    head-first
    (let [semantics (atom :top)
          comp-type (atom :top)
          argument (atom :top)
          mod (atom :top)
          cat (atom :comp)]
      {:rule "complementizer-phrase-slash"
       :phrasal true
       :synsem {:slash true
                :cat cat
                :mod mod
                :subcat {:1 argument
                         :2 []}
                :sem semantics}
       :comp {:phrasal true
              :synsem {:cat :verb
                       :mod mod
                       :sem semantics
                       :slash true
                       :subcat {:1 argument
                                :2 []}}}
       :head {:phrasal false
              :synsem {:cat cat
                       :sem semantics}}}))
   
   
   (unify-check c10
                {:rule "determiner-phrase"
                 :synsem {:cat :det}
                 :head {:synsem {:slash false}}})

   ;;         nbar   |sem  [1] |
   ;;       /      \ |mod <[2]>|
   ;;      /        \
   ;; adj |sem [2]|  n |sem [1]|
   ;; 
   (unify-check c11-comp-subcat-1
                (let [head-constraint (atom :top)
                      adj-sem (atom {:prop head-constraint})
                      head-mod (atom :top)]
                  {:rule "nbar"
                   :synsem {:mod {:first adj-sem
                                  :rest head-mod}}
                   :comp {:synsem {:cat :adjective
                                   :sem adj-sem}}
                   :head {:phrasal false
                          :synsem {:cat :noun
                                   :sem {:prop head-constraint}
                                   :mod head-mod}}}))
   ;; noun-phrase -> det nbar
   (unify-check c10
                comp-specs-head
                (let [number-agreement (atom :top)
                      mod (atom :top)]
                  {:rule "noun-phrase"
                   :aliases (list "np")
                   :synsem {:agr {:number number-agreement}
                            :reflexive false
                            :cat :noun
                            :mod mod
                            :sem {:number number-agreement}}
                   :head {:phrasal :top
                          :synsem {:mod mod}}}))

   (let [sem (atom :top)
         agr (atom :top)
         reflexive (atom :top)]
     (unify-check h10
                  subcat-1-principle
                  head-first
                  {:comment "h10"
                   :schema-symbol 'h10
                   :first :head
                   :rule "prepositional-phrase"
                   :synsem {:agr agr
                            :cat :prep
                            :reflexive reflexive
                            :sem sem}
                   :head {:synsem {:sem sem
                                   :subcat {:1 {:agr agr
                                                :reflexive reflexive}}}}}))
   (unify-check c10
                unmodified
                root-is-head
                {:rule "sentence-nonphrasal-head"
                 :synsem {:cat :verb
                          :slash false}
                 :head {:phrasal false
                        :slash false
                        :synsem {:participle false}}})
   (unify-check c10
                unmodified
                root-is-head-root
                {:head {:phrasal true
                        :synsem {:slash false}}
                 :rule "sentence-phrasal-head"
                 :synsem {:cat :verb
                          :slash false}})
   
   (let [obj-mod (atom :top)]
     (unify-check h21
                  root-is-head
                  {:rule "transitive-vp-nonphrasal-head"
                   :comp {:synsem {:mod obj-mod}}
                   :synsem {:aux false
                            :sem {:obj {:mod obj-mod}}
                            :slash false
                            :cat :verb}}))

   (let [obj-mod (atom :top)]
     (unify-check h21
                  root-is-head-root
                  {:rule "transitive-vp-phrasal-head"
                   :comp {:synsem {:mod obj-mod}}
                   :head {:phrasal true
                          :synsem {:mod obj-mod}}
                   :synsem {:aux false
                            :sem {:obj {:mod obj-mod}}
                            :slash false
                            :cat :verb}}))
   
   ;; TODO: enforce the facts that:
   ;; 1. {:head {:phrasal true}} => root-is-head-root
   ;; 2. {:head {:phrasal false}} => root-is-head
   (unify-check h32
                root-is-head
                {:rule "vp32"
                 :head {:phrasal false
                        :phrasal-verb true}
                 :synsem {:aux false
                          :cat :verb
                          :slash false}})
   (unify-check h10
                {:head {:phrasal false
                        :synsem {:cat :sent-modifier}}
                 :rule "s-modifier"})
   
   ;;      noun-phrase3      ->  noun-phrase[1,2] slash-obj
   ;; e.g. "the man you saw" ->  "the man"        "you saw"
   (unify-check
    {:rule "noun-phrase3"}
    head-principle
    head-first
    (let [sem (atom {:spec {:pred :definite}})
          subject-of-complement (atom :top)
          comp-sem (atom {:subj subject-of-complement})
          head-mod (atom :top)
          head-def (atom :def)
          head-synsem (atom {:subcat []
                             :def head-def
                             :mod head-mod
                             :sem sem})]
      {:phrasal true
       :synsem {:cat :noun
                :def head-def
                :mod {:first comp-sem
                      :rest head-mod}
                :sem sem
                :slash false
                :subcat []}
       :head {:rule "noun-phrase2"
              :phrasal true
              :synsem head-synsem}
       :comp {:rule "slash-obj"
              :phrasal true
              :synsem {:sem comp-sem
                       :slash true
                       :subcat {:1 head-synsem
                                :2 []}}
              :head {:synsem {:subcat {:1 {:synsem {:sem subject-of-complement}}}}}}}))

   (unify-check
    {:rule "slash-obj"}
    head-principle
    head-last head-semantics
    (let [first-arg (atom :top)
          second-arg (atom {:reflexive false})]
      {:phrasal true
       :synsem {:slash true
                :subcat {:1 second-arg
                         :2 []}}
       :comp {:synsem first-arg}
       :head {:synsem {:subcat {:1 first-arg
                                :2 second-arg}}}})
    {:synsem {:aux false}
     :head {:synsem {:aux false}}
     :comp {:synsem {:subcat []
                     :case :nom
                     :cat :noun}}}

    (let [infl (atom :top)
          participle (atom false)]
      {:synsem {:infl infl
                :participle participle}
       :head {:synsem {:infl infl
                       :participle participle}}}))])
    

(defn aux-is-head-feature [phrase]
  (cond (= :verb (get-in phrase [:synsem :cat]))
        (unify-check phrase
                (let [ref (atom :top)]
                  {:synsem {:aux ref}
                   :head {:synsem {:aux ref}}}))
        true phrase))

(defn modal-is-head-feature [phrase]
  (cond (= :verb (get-in phrase [:synsem :cat]))
        (unify-check phrase
                (let [ref (atom :top)]
                  {:synsem {:modal ref}
                   :head {:synsem {:modal ref}}}))
        true phrase))

;; TODO: rewrite this and and above 2 functions into
;; a single threaded (->) function.
(def grammar
  (map (fn [phrase]
         (modal-is-head-feature
          (aux-is-head-feature phrase)))
       grammar))

(defn morph-walk-tree [tree]
  (log/debug (str "morph-walk-tree: " (fo tree)))
  (merge
   {:surface (fo (get-in tree [:english]))}
   (if (get-in tree [:comp])
     {:comp (morph-walk-tree (get-in tree [:comp]))}
     {})
   (if (get-in tree [:head])
     {:head (morph-walk-tree (get-in tree [:head]))})))

(defn compile-lexicon []
  (into {}
        (for [[k v] (deliver-lexicon)]
          (let [filtered-v v]
            (if (not (empty? filtered-v))  ;; TODO: this empty-filtering should be done in lexicon.cljc, not here.
              [k filtered-v])))))

(defn model []
  (let [debug (log/info "  loading lexicon..")
        lexicon (read-lexicon "en")
        debug (log/info "  indices..")
        indices (create-indices lexicon index-lexicon-on-paths)
        ;; this function 'morph' is identical to: babel.english/morph
        debug (log/info "  morph..")
        morph (fn [expr & {:keys [from-language show-notes]
                           :or {from-language nil
                                show-notes false}}]
                (fo expr
                    :from-language from-language :from-notes show-notes
                    :lexicon lexicon))
        debug (log/info "  finalizing..")
        model
        {:name (str "English language model created with ❤ by babel.english.grammar/model "
                    "at: " (local-timestamp))
         :default-fn default-fn
         :index-fn (fn [spec] (lookup-spec spec indices index-lexicon-on-paths))
         ;; Will throw a clojure/core-level exception if more than 1 rule has the same :rule value:
         :grammar-map (zipmap
                       (map #(keyword (get-in % [:rule])) grammar)
                       grammar)
         
         :grammar grammar
         
         :language "en"
         :language-keyword :english
         
         ;; TODO: unused; remove.
         :lexical-cache (atom (cache/fifo-cache-factory {} :threshold 1024))
         :lexicon lexicon
         :lookup (fn [arg]
                   (analyze arg lexicon))
         :morph morph
         :morph-ps fo-ps}]
    (merge model
           {:vocab2model (fn [vocab-items filter-lexicon-fn]
                           (model-with-vocab-items vocab-items filter-lexicon-fn model))
            :generate-fn
            (fn [spec]
              (generate/generate spec model))})))

;;(def source-model @@(get babel.directory/models :en))
;;(def filter-lexicon-fn #(= :det (get-in % [:synsem :cat])))
;;(def new-model ((:vocab2model source-model) source-vocab-items filter-lexicon-fn))
;;(clojure.pprint/pprint (get (:lexicon new-model) "wine"))

(defn model-with-vocab-items [vocab-items filter-lexicon-fn model]
  (let [input-lexicon (transform-with-english-lexical-rules
                       (reduce merge (map vocab-entry-to-lexeme vocab-items)))
        lexicon (merge-with concat
                            input-lexicon
                            (filtered-lexicon
                             (:lexicon model)
                             filter-lexicon-fn))
        indices (create-indices lexicon index-lexicon-on-paths)
        model (merge model
                     {:lexicon lexicon})]
    (merge model
           {:index-fn (fn [spec] (lookup-spec spec indices index-lexicon-on-paths))})))

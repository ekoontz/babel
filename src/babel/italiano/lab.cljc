(ns babel.italiano.lab
  (:require
   [babel.directory] ;; this is needed even though there are no references to directory in here.
   [babel.generate :refer [bolt lightning-bolts]]
   [babel.italiano :as italiano :refer [analyze generate model morph morph-ps parse]]
   [babel.italiano.grammar :refer [model-plus-lexicon]]
   [babel.test.test :as btest]
   #?(:cljs [babel.logjs :as log])
   [clojure.pprint :refer [pprint]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:clj [clojure.repl :refer [doc]])
   [clojure.set :as set]
   [dag_unify.core :as u :refer [strip-refs unify]]))

(defn generate-speed-test [spec & [times]]
  (btest/generate-speed-test spec model times))

(defn run-passato-prossimo-test []
  (generate-speed-test {:synsem {:cat :verb :subcat []
                                 :sem {:reflexive true
                                       :tense :present
                                       :aspect :perfect}}}))
;; roundtrip parser testing
(defn roundtrip-parsing [n]
  (take n
        (repeatedly #(let [generated
                           (morph (generate {:synsem {:cat :verb
                                                      :subcat []}}))
                           parsed (reduce concat (map :parses (parse generated)))]
                       (log/info (str "generated: " generated))
                       (log/info (str "semantics: "
                                      (or
                                       (strip-refs
                                        (u/get-in (first parsed)
                                                  [:synsem :sem]))
                                       (str "NO PARSE FOUND FOR: " generated))))
                       {:generated generated
                        :pred (u/get-in (first parsed) [:synsem :sem :pred])
                        :subj (u/get-in (first parsed) [:synsem :sem :subj :pred])}))))

(def transitive-spec 
  {:synsem {:cat :verb
            :subcat []
            :sem {:subj {:pred :top}
                  :obj {:pred :top}}}})


(def non-aux-spec
  {:synsem {:aux false}})

(defn transitive-sentence []
  (let [spec transitive-spec]
    (repeatedly #(-> spec generate morph time println))))

(def phrasal-spec
  {:head {:head {:phrasal false}
          :comp {:phrasal false}}
   :comp {:phrasal false}})

(defn sentences-with-pronoun-objects-hints
  "supply a spec enhanced with syntactic info to speed-up generation."  
  []
  (let [spec (unify non-aux-spec transitive-spec phrasal-spec)]
    (repeatedly #(-> spec generate morph time println))))

(defn sentences-with-pronoun-objects-small []
  (let [lexicon (:lexicon model)
        transitive? (fn [lexeme]
                      (and (= (u/get-in lexeme [:synsem :cat])
                              :verb)
                           (= (u/get-in lexeme [:synsem :aux] false)
                              false)
                           (= (u/get-in lexeme [:synsem :infl] :top)
                              :top)
                           (not (empty? (get-in lexeme [:synsem :subcat] [])))
                           (map? (get-in lexeme [:synsem :subcat :2]))
                           (empty? (get-in lexeme [:synsem :subcat :3] []))))
        
        transitive-verbs
        (filter (fn [k]
                  (let [lexemes (->> (get lexicon k)
                                     (filter transitive?))]
                    (not (empty? lexemes))))
                (keys lexicon))

        pronoun? (fn [lexeme]
                   (and (= (u/get-in lexeme [:synsem :cat])
                           :noun)
                        (= (u/get-in lexeme [:synsem :pronoun])
                           true)))
        propernoun? (fn [lexeme]
                   (and (= (u/get-in lexeme [:synsem :cat])
                           :noun)
                        (= (u/get-in lexeme [:synsem :propernoun])
                           true)))

        spec (unify transitive-spec phrasal-spec non-aux-spec
                    {:head {:comp {:synsem {:pronoun true}}}})]
    (repeatedly (fn []
                  (let [chosen-subset (set (take 1000 (shuffle transitive-verbs)))
                        model
                        (model-plus-lexicon
                         ;; filtered lexicon: all pronouns and a small subset of transitive verbs.
                         (into {}
                               (for [[k lexemes] lexicon]
                                 (let [filtered-lexemes
                                       (filter (fn [lexeme]
                                                 (or (and
                                                      (contains? chosen-subset k)
                                                      (transitive? lexeme))
                                                     (propernoun? lexeme)
                                                     (pronoun? lexeme)))
                                               lexemes)]
                                   (if (not (empty? filtered-lexemes))
                                     [k filtered-lexemes])))))]
                    (-> spec (generate model) morph time println))))))

(defn screen-out-false [m]
  (->>
   (dag_unify.core/paths m)
   (filter #(let [v (dag_unify.core/get-in m %)]
              (and (not (= false v))
                   (not (map? v)))))
   (map (fn [path]
          [path (u/get-in m path)]))
   (map (fn [[path v]]
          (assoc-in {} path v)))
   (reduce (fn [a b] (merge-with merge a b)))))

(defn grab-bag []
  (let [semantics (-> "io lo ho" parse first :parses
                      first (dag_unify.core/get-in [:synsem :sem]))

        semantics-any-subject
        (dag_unify.core/dissoc-paths semantics [[:subj]])

        semantics-any-object
        (unify (dag_unify.core/dissoc-paths semantics [[:obj]])
               {:obj {:pred :top}})]
    (count
     (take 5 (repeatedly
              #(-> {:synsem {:subcat []
                             :cat :verb
                             :sem (screen-out-false semantics)}}
                   generate
                   morph time
                   println))))
    (println "----")
    (count
     (take 5 (repeatedly
              #(-> {:synsem {:subcat []
                             :cat :verb
                             :sem (screen-out-false semantics-any-subject)}}
                   generate
                   morph time
                   println))))

    (println "----")
    (count
     (take 5 (repeatedly
              #(-> {:synsem {:subcat []
                             :cat :verb
                             :sem (screen-out-false semantics-any-object)}}
                   generate
                   morph time
                   println))))
    (-> "loro vedono casa"
        parse
        ((fn [tokenizations]
           (mapcat :parses tokenizations)))
        first
        (u/get-in [:synsem :sem])
        clojure.pprint/pprint)))

(def basic
  {:head {:comp {:synsem {:pronoun true}}}
   :synsem {:cat :verb
            :subcat []
            :aux false}})
;;   H
;;  / \
;; C   H
;;    / \
;;   C   H
(def lexical-subject
  (unify basic {:head {:comp {:phrasal false}}
                :comp {:phrasal false}}))
;;      H
;;    /   \
;;   C     H
;;  / \   / \
;; C   H C   H
(def phrasal-subject
  (unify basic {:head {:comp {:phrasal false}}
                :comp {:phrasal true}}))

(defn new-generate [spec]
  (-> (bolt model spec 0 2)
      ((fn [tree]
         (u/assoc-in! tree [:comp]
                      (bolt model (u/get-in tree [:comp]) 0 0))))
      ((fn [tree]
         (u/assoc-in! tree [:head :comp]
                      (bolt model (u/get-in tree [:head :comp]) 0 0))))))

(defn one-sentence-with-lexical-subj []
  (new-generate lexical-subject))

(defn one-sentence-with-np-subj []
  (-> (bolt model phrasal-subject 0 2)
      ((fn [tree]
         (u/assoc-in! tree [:comp]
                      (bolt model (u/get-in tree [:comp]) 0 2))))
      ((fn [tree]
         (u/assoc-in! tree [:comp :comp]
                      (bolt model (u/get-in tree [:comp :comp]) 0 0))))
      ((fn [tree]
         (u/assoc-in! tree [:head :comp]
                      (bolt model (u/get-in tree [:head :comp]) 0 0))))))

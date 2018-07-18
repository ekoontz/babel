(ns babel.generate
  (:refer-clojure :exclude [assoc-in get-in deref resolve find parents])
  (:require
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [clojure.math.combinatorics :as combo]
   [clojure.string :as string]
   [dag_unify.core :as u
    :refer [assoc-in assoc-in! copy create-path-in
            dissoc-paths fail-path get-in fail? strip-refs unify unify!]]))
          
;; the higher the constant below,
;; the more likely we'll first generate leaves
;; (terminal nodes) rather than trees.
(def ^:const branching-factor #(+ % 5))

(def ^:const truncate? false)

(declare gen)
(declare get-lexemes)
(declare grow)
(declare minitrees)

(defn generate
  "Return one expression matching spec _spec_ given the model _model_."
  [spec language-model]
  ;; 
  ;; Return the tree of an expression, generated by the given model,
  ;; that satisfies given spec:
  ;;
  ;; The tree will look like:
  ;; 
  ;; Where H is the head of a certain node in the tree, and C
  ;; is the complement. Terminal nodes are lexemes given by the model.
  ;;
  ;;       H   
  ;;      / \  
  ;;     H   C 
  ;;        / \  
  ;;       H   C 
  ;; 
  ;; A convenient wrapper around (defn gen) (below).

  (log/debug (str "(generate) with model named: " (:name language-model)))
  (first (gen spec language-model)))

(defn gen [spec model]
  (grow (minitrees spec model) model))

(defn minitrees-1 [spec model parent-rules]
  (if (not (empty? parent-rules))
    (let [parent-rule (first parent-rules)
          child-spec
          (unify
           (get-in spec [:head] :top)
           (get-in parent-rule [:head] :top))]
      (lazy-cat
       
       ;; get all the things to be added
       ;; as the head child of parent-rule:
       ;; 1. lexemes that could be the head child:
       (map (fn [child]
              (assoc-in parent-rule [:head] child))
            (get-lexemes (unify
                          (get-in spec [:head] :top)
                          (get-in parent-rule [:head] :top))
                         model))
       
       ;; 2. rules that could be the head child:
       (map (fn [child]
              (assoc-in parent-rule [:head] child))
            (:grammar model))
       
       (minitrees-1 spec model (rest parent-rules))))))

(defn minitrees
  "Return every possible tree of depth 1 from the given spec and model."
  [spec model]
  ;; get all rules that match input _spec_:
  (if (nil? spec) (throw (Exception. (str "nope: spec was nil."))))
  (log/debug (str "minitrees: spec:" (strip-refs spec)))
  (->>
   ;; 1: get all rules that satisfy _spec_  and then shuffle them.
   (shuffle
    (->> (:grammar model)
         (map #(unify % spec))
         (filter #(not (= :fail %)))
         (map #(unify % {::started? true}))))
   
   ;; 2. try to add heads to each matching rule.
   (minitrees-1 spec model)
   
   (filter #(not (= % :fail)))
   (map #(assoc-in % [::started?] true))))

(defn get-lexemes [spec model]
  "Get lexemes matching the spec. Use a model's index if available, where the index 
   is a function that we call with _spec_ to get a set of indices. 
   Otherwise use the model's entire lexeme."
  (->>
   (if-let [index-fn (:index-fn model)]
     (index-fn spec)
     (do
       (log/warn (str "get-lexemes: no index found: using entire lexicon."))
       (flatten (vals
                 (or (:lexicon (:generate model)) (:lexicon model))))))
   (filter #(or (= false (get-in % [:exception] false))
                (not (= :verb (get-in % [:synsem :cat])))))
   (map #(unify % spec))
   (filter #(not (= :fail %)))
   (map #(assoc-in! % [::done?] true))))
  
(defn frontier
  "get the next path to which to adjoin within _tree_."
  [tree]
  (cond

    (= (get-in tree [::done?]) true)
    []
    
    (and (= (u/get-in tree [:phrasal]) true)
         (not (u/get-in tree [::done?]))
         (= true (u/get-in tree [::started?]))
         (not (u/get-in tree [:head ::done?])))
    (cons :head (frontier (u/get-in tree [:head])))

    (and (= (u/get-in tree [:phrasal]) true)
         (= true (u/get-in tree [::started?])))
    (cons :comp (frontier (u/get-in tree [:comp])))
    
    true []))

(defn grow [trees model]
  (if (not (empty? trees))
    (let [tree (first trees)
          f (frontier tree)
          depth (count f)
          child-spec (u/get-in tree f)
          child-lexemes #(get-lexemes child-spec model)
          child-trees #(minitrees child-spec model)]
      (if false (println (str "tree: " ((:morph-ps model) tree) ": f:" f ";"
                              "child phrasal?:"
                              (u/get-in child-spec [:phrasal]))))
      (lazy-cat
       (if (not (empty? f))
         (grow
          (->> (cond
                 (= true (u/get-in child-spec [:phrasal]))
                 (child-trees)

                 (= false (u/get-in child-spec [:phrasal]))
                 (child-lexemes)
                 
                 (= 0 (rand-int (branching-factor depth)))
                 ;; generate children that are trees before children that are leaves.
                 (lazy-cat (child-trees) (child-lexemes))
                 
                 true ;; generate children that are leaves before children that are trees.
                 (lazy-cat (child-lexemes) (child-trees)))

               (map (fn [child]
                      (let [tree-with-child (u/assoc-in tree f child)]
                        (if false (println (str "twc:" ((:morph-ps model) tree-with-child))))
                        (if (and (= :comp (last f))
                                 (= true (u/get-in child [::done?])))
                          (u/assoc-in! tree-with-child (butlast f) {::done? true})
                          tree-with-child)))))
          model)
         [tree])
       (grow (rest trees) model)))))

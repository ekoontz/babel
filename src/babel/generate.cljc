(ns babel.generate
  (:refer-clojure :exclude [assoc-in get-in])
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
(def ^:const branching-factor 5)
(def ^:const branch? #(= 0 (rand-int (+ % branching-factor))))
(def ^:const truncate? false)

(declare gen)
(declare get-lexemes)
(declare grow)
(declare parent-with-head)
(declare parent-with-head-1)

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
  (grow (parent-with-head spec model 0) model))

(defn parent-with-head
  "Return every possible tree of depth 1 from the given spec and model."
  [spec model depth]
  ;; get all rules that match input _spec_:
  (if (nil? spec) (throw (Exception. (str "nope: spec was nil."))))
  (->>
   ;; 1: get all rules that satisfy _spec_.
   (->> (shuffle (:grammar model))
        (map #(unify % spec))
        (filter #(not (= :fail %))))
    
    ;; 2. try to add heads to each matching rule.
    (parent-with-head-1 spec model depth)
    
    (filter #(not (= % :fail)))
    (map #(assoc-in! % [::started?] true))))

(defn parent-with-head-1 [spec model depth parent-rules]
  (if (not (empty? parent-rules))
    (let [parent-rule (first parent-rules)
          phrases-with-phrasal-head #(map (fn [child]
                                           (u/assoc-in parent-rule [:head] child))
                                         (:grammar model))
          phrases-with-lexical-heads #(map (fn [child]
                                           (u/assoc-in parent-rule [:head] child))
                                         (get-lexemes (unify
                                                       (get-in spec [:head] :top)
                                                       (get-in parent-rule [:head] :top))
                                                      model))]
      (cond
        (branch? depth)
        (lazy-cat
         ;; get all the things to be added
         ;; as the head child of parent-rule:
         ;; 1. phrases that could be the head child:
         (phrases-with-phrasal-head)
         ;; 2. lexemes that could be the head child:
         (phrases-with-lexical-heads)
         (parent-with-head-1 spec model depth (rest parent-rules)))

        true
        (lazy-cat
         ;; 1. lexemes that could be the head child:
         (phrases-with-lexical-heads)
         ;; 2. phrases that could be the head child:
         (phrases-with-phrasal-head)
         (parent-with-head-1 spec model depth (rest parent-rules)))))))

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

(defn assoc-children [tree children f model]
  (if (not (empty? children))
    (let [child (first children)]
      (lazy-cat
       (->
        (let [tree-with-child (u/assoc-in tree f child)]
          (log/debug (str "a-c:"
                          ((:morph-ps model) tree-with-child)))
          (if (= true (u/get-in child [::done?]))
            (-> tree-with-child
                (u/assoc-in! 
                 (concat (butlast f) [::done?])
                 true)
                (u/dissoc-paths (if truncate? [f] [])))
            tree-with-child))
        ((:default-fn model)))
       (assoc-children tree (rest children) f model)))))

(defn grow [trees model]
  (if (not (empty? trees))
    (let [tree (first trees)
          f (frontier tree)
          depth (count f)
          child-spec (u/get-in tree f)
          child-lexemes #(get-lexemes child-spec model)
          child-trees #(parent-with-head child-spec model depth)]
      (lazy-cat
       (if (not (empty? f))
         (grow
          (let [children
                (cond
                  (= true (u/get-in child-spec [:phrasal]))
                  (child-trees)
                  
                  (= false (u/get-in child-spec [:phrasal]))
                  (child-lexemes)
                  
                  (branch? depth)
                  ;; generate children that are trees before children that are leaves.
                  (lazy-cat (child-trees) (child-lexemes))
                  
                  true ;; generate children that are leaves before children that are trees.
                  (lazy-cat (child-lexemes) (child-trees)))]
            (assoc-children tree children f model))
          model)
         [tree])
       (grow (rest trees) model)))))


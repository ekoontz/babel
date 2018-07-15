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
                                        
;; during generation, will not decend deeper than this when creating a tree:
;; TODO: should also be possible to override per-language.
(def ^:const max-depth 10)

(declare add-comps-to-bolt)
(declare add-to-bolt-at-path)
(declare comp-paths)
(declare gen)
(declare get-bolts-for)
(declare get-lexemes)
(declare lightning-bolts)

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

;; Wrapper around (defn lightning-bolts) to provide a way to
;; test indexing and memoization strategies.
(defn get-bolts-for
  "Return every possible bolt for the given model and spec."
  [model spec depth]
  (let [search-for-key
        (strip-refs
         {:synsem {:sem {:aspect (get-in spec [:synsem :sem :aspect] :top)
                         :reflexive (get-in spec [:synsem :sem :reflexive] :top)
                         :tense (get-in spec [:synsem :sem :tense] :top)}
                   :subcat (get-in spec [:synsem :subcat] :top)
                   :cat (get-in spec [:synsem :cat] :top)}
          :depth depth})

        debug (log/trace (str "looking for key: "
                             search-for-key))
        
        bolts ;; check for bolts compiled into model
        (get (-> model :bolts)
             search-for-key)]
    (cond
      (not (nil? bolts))
      (do
        (log/debug (str "found compiled bolts."))
        (shuffle (->> bolts
                      (map #(unify spec %))
                      (filter #(not (= :fail %))))))
      true
      (do
        (log/trace (str "get-bolts-for: no compiled bolts."))
        (lightning-bolts model spec 0 depth)))))

;; a 'lightning bolt' is a dag that
;; has among its paths, paths like [:head :head :head] and
;; pictorially look like:
;; 
;; 
;;   H        H    H
;;    \      /      \
;;     H    H        H        ...
;;    /      \        \
;;   H        ..       ..
;;    \
;;     ..
;; 
;; Each bolt has has a head child.
;; Each head child may be a leaf or
;; otherwise has a child with the same two
;; possibilities: either it's a leaf or itself
;; a bolt, up to the maximum depth.
(defn lightning-bolts
  "Return every possible bolt for the given model and spec. Start at the given depth and
   keep generating until the given max-depth is reached."
  [model spec depth max-depth]
  (cond (and (< depth max-depth)
             (not (= false (get-in spec [:phrasal] true))))

        ;; get all rules that match input _spec_:
        (->> (shuffle
              (->> (:grammar model)
                   (map #(unify % spec))
                   (filter #(not (= :fail %)))))

             (mapcat (fn [grammar-rule]
                       ;; for each such rule,
                       ;; descend to the head child and
                       ;; find all the lightning-bolts
                       ;; that match the rule's head child.
                       (->> (lightning-bolts model
                                             (get-in grammar-rule [:head])
                                             (+ 1 depth)
                                             max-depth)
                            ;; add each such sub-bolt to
                            ;; the grammar rule as the head,
                            ;; yielding one bolt rooted at _grammar-rule_.
                            (map (fn [head]
                                   (assoc-in grammar-rule [:head] head)))))))
        ;; can't descend further, so 
        ;; get the leaves that match _spec_.
        true (get-lexemes spec model)))

(defn sprouts
  "Return every possible tree of depth 1 from the given spec and model."
  [spec model]
  ;; get all rules that match input _spec_:
  (if (nil? spec) (throw (Exception. (str "nope: spec was nil."))))
  (log/info (str "sprouts: spec:" (strip-refs spec)))
  (->>
   ;; 1: get all rules that satisfy _spec_  and then shuffle them.
   (shuffle
    (->> (:grammar model)
         (map #(unify % spec))
         (filter #(not (= :fail %)))))
   
   ;; 2. try to add heads to each matching rule.
   (mapcat (fn [parent-rule]
             (let [child-spec
                   (unify
                    (get-in spec [:head] :top)
                    (get-in parent-rule [:head] :top))]

               ;; for each such rule,
               ;; descend to the head child and
               ;; find all the children
               ;; that match the rule's head child.
               (->>
                
                ;; get all the things to be added
                ;; as the head chid of parent-rule:
                ;; 1. lexemes that could be the head.
                ;; 2. rules that could be the head.
                (lazy-cat
                 
                 ;; 2.1. lexemes that could be the head.
                 (get-lexemes (unify
                               (get-in spec [:head] :top)
                               (get-in parent-rule [:head] :top))
                              model)
                 
                 ;; 2.2. grammar rules that could be the head.
                 (:grammar model))
                
                ;; for each such child in {2.1. + 2.2},
                ;; adjoin it as the :head.
                (map (fn [child]
                       (assoc-in parent-rule [:head] child)))))))
   
   (filter #(not (= % :fail)))))

(defn bolts
  "Return every possible bolt for the given model and spec. Start at the given depth and
   keep generating until max-depth is reached."
  [spec model]
  (get-bolts-for model spec max-depth))

(defn bolt
  "Return the first possible bolt for the given model and spec. Start at the given depth and
   keep generating until max-depth is reached."
  [spec model]
  (first (bolts spec model)))

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
   (map #(assoc-in! % [] {::done true}))))
  
(defn add-comps-to-bolt
  "bolt + paths => trees"
  [bolt model comp-paths]
  (if (not (empty? comp-paths))
    (mapcat (fn [bolt]
              (add-to-bolt-at-path bolt (first comp-paths) model))
            (add-comps-to-bolt bolt model (rest comp-paths)))
    [bolt]))

(defn add-to-bolt-at-path
  "generate all complements for bolt at given path, and create a partial tree: bolt + complement => partial tree"
  [bolt path model]
  (->>
   (gen (get-in bolt path) model 0) ;; generate all complements for _bolt_ at _path_.
   (map #(let [partial-tree
               (assoc-in! (copy bolt) path %)] ;; add the complement to the bolt at _path_.
           ;; apply model's :default-fn, if any.
           ;; TODO: default-fn should return a sequence of partial trees,
           ;; not just one.
           (if (:default-fn model)
             (first ((:default-fn model) partial-tree))
             partial-tree)))))

(defn comp-paths
  "Find all paths to all complements (both terminal and non-terminal) given a depth. Returned in 
   ascending length (shortest first)."
  ;; e.g., a tree of depth 2
  ;; will have the following paths:
  ;;   [:comp] [:head :comp]
  ;;   because it looks like:
  ;; 
  ;;   H
  ;;  / \
  ;; C   H
  ;;    / \
  ;;   H   C
  ;;
  [depth]
  (cond
    (= depth 0)
    []
    (= depth 1)
    (list [:comp])
    true
    (cons
     (concat (take (- depth 1)
                   (repeatedly (fn [] :head)))
             [:comp])
     (comp-paths (- depth 1)))))

(defn frontier
  "get the next path to which to adjoin within _tree_."
  [tree]
  (cond

    (= (get-in tree [::done]) true)
    []
    
    (and (= (get-in tree [:phrasal] true))
         (= ::none (get-in tree [:head] ::none)))
    []
    
    (and (= (u/get-in tree [:phrasal]) true)
         (not (u/get-in tree [::done]))
         (not (u/get-in tree [:head ::done])))
    (cons :head (frontier (u/get-in tree [:head])))

    (and (= (u/get-in tree [:phrasal]) true))
    (cons :comp (frontier (u/get-in tree [:comp])))
    
    true []))

(defn grow [trees model]
  (if (not (empty? trees))
    (let [tree (first trees)
          f (frontier tree)
          depth (count f)
          child-spec (u/get-in tree f)
          child-lexemes #(get-lexemes child-spec model)
          child-trees #(sprouts child-spec model)
          
          ;; the higher the constant below,
          ;; the more likely we'll first generate leaves
          ;; (terminal nodes) rather than trees.
          branching-factor #(+ % 3)]

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
                        (if (and (= :comp (last f))
                                 (= true (u/get-in child [::done])))
                          (u/assoc-in! tree-with-child (butlast f) {::done true})
                          tree-with-child)))))
          model)
         [tree])
       (grow (rest trees) model)))))

(defn gen [spec model]
  (first (take 1 (grow (sprouts spec model) model))))


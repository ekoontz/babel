(ns babel.generate
  (:refer-clojure :exclude [assoc-in get-in deref resolve find parents])
  (:require
   [babel.index :refer [intersection-with-identity]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [clojure.math.combinatorics :as combo]
   [clojure.string :as string]
   [dag_unify.core :refer [assoc-in assoc-in! copy create-path-in
                           dissoc-paths fail-path get-in fail? strip-refs unify unify!]]))
                                        
;; during generation, will not decend deeper than this when creating a tree:
;; TODO: should also be possible to override per-language.
(def ^:const max-total-depth 8)

;; use map or pmap.
(def ^:const mapfn map)

(def ^:const handle-unify-fail #(log/debug %))
(def ^:const throw-exception-on-unify-fail false)


(def ^:const shufflefn
  (fn [x]
    ;; deterministic generation:
;;    x
    ;; nondeterministic generation
    (lazy-seq (shuffle x))

    ))

;; whether to remove [:head] and [:comp] paths from generated trees after generation:
;; for performance.
(def ^:const truncate false)

(declare candidate-parents)
(declare get-lexemes)
(declare lightning-bolts)
(declare add-paths-to-bolt)
(declare paths-for-bolt)
(declare add-path-to-bolts)
(declare add-to-bolt-at-path)

(declare gen)

(defn generate
  "Return one expression matching spec _spec_ given the model _model_."
  [spec language-model
   & {:keys [max-total-depth truncate-children? lexicon take-n]
      :or {max-total-depth max-total-depth
           lexicon nil
           shuffle? nil
           truncate-children? true}}]
  (log/debug (str "(generate) with model named: " (:name language-model)))
  (first (gen spec language-model 0)))

(defn gen
  "spec => trees"
  [spec model depth & [from-bolts]]
;;  (println (str "trying depth:" depth "; spec=" (dag_unify.core/strip-refs spec)))
  (if (< depth 5)
    (lazy-cat
     (let [bolts (or from-bolts (lightning-bolts model spec 0 depth))]
       (if (not (empty? bolts))
         (lazy-cat
          (add-paths-to-bolt (first bolts) model
                             (paths-for-bolt depth [:head]))
          (gen spec model depth (rest bolts)))))
     (gen spec model (+ 1 depth)))))

(defn lightning-bolts
  [model spec depth max-depth & [use-candidate-parents]]
  ;; Generate 'lightning bolts':
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
  ;; Each bolt is a tree with only one child per parent: the head child.
  ;; Each head child may be a leaf or
  ;; otherwise has a child with the same two options (leaf or a head child),
  ;; up to the maximum depth.
  (if (< depth max-depth)
    (let [candidate-parents (or use-candidate-parents
                                (shufflefn (candidate-parents (:grammar model) spec)))]
      (if (not (empty? candidate-parents))
        (let [candidate-parent (first candidate-parents)]
          (lazy-cat
           (let [unified-candidate-parent (unify candidate-parent spec)]
             (->> (lightning-bolts model
                                   (get-in unified-candidate-parent [:head])
                                   (+ 1 depth)
                                   max-depth)
                  (map (fn [head]
                         (assoc-in unified-candidate-parent [:head] head)))))
           (lightning-bolts model spec depth max-depth (rest candidate-parents))))))
    (shuffle (get-lexemes model spec))))

(defn add-paths-to-bolt
  "bolt + paths => trees"
  [bolt model paths-for-bolt]
  (if (not (empty? paths-for-bolt))
    (add-path-to-bolts 
     (add-paths-to-bolt bolt model (rest paths-for-bolt))
     (first paths-for-bolt)
     model)
    [bolt]))

(defn add-path-to-bolts
  "bolts + path => partial trees"
  [bolts path model]
  (if (not (empty? bolts))
    (lazy-cat
     (add-to-bolt-at-path (first bolts) path model)
     (add-path-to-bolts (rest bolts) path model))))

(defn paths-for-bolt [depth prefix]
  (cond
    (= depth 0)
    []
    (= depth 1)
    [[:comp]]
    true
    (cons (concat prefix [:comp])
          (paths-for-bolt (- depth 1) (cons :head prefix)))))

(defn add-to-bolt-at-path
  "bolt + path => partial trees"
  [bolt path model]
  (if
      ;; if the path _path_ exists for _bolt_:
      (and (= true (get-in bolt [:phrasal]))
           (not (nil? (get-in bolt path)))
           (= true (get-in bolt (concat path [:phrasal]) true)))
    
    (->>
     ;; set of all complements at _path_ for _bolt_:
     (gen (get-in bolt path) model 0)
     
     ;; add each member _each_comp_ of this set to _bolt_:
     (map (fn [each-comp]
            (->
             bolt
             (dag_unify.core/assoc-in path
                                      each-comp)
             ((:default-fn model))))))))

(defn candidate-parents
  "find subset of _rules_ for which each member unifies successfully with _spec_"
  [rules spec]
  (filter #(not (= :fail %))
          (mapfn (fn [rule]
                   (log/trace (str "candidate-parents: testing rule: " (:rule rule)))
                   (let [unified (unify spec rule)]
                     (if (= :fail unified)
                       (log/debug (str "candidate parent: " (:rule rule) " failed at:" (fail-path spec rule)))
                       (log/debug (str "candidate parent: " (:rule rule) " unified successfully with spec:" (strip-refs spec))))
                     unified))
                 rules)))

(defn get-lexemes [model spec]
  "Get lexemes matching the spec. Use a model's index if available, where the index is a function that we call with _spec_ to get a set of indices. otherwise use the model's entire lexeme."
  (->>

   (if (= false (get-in spec [:phrasal] false))
     (if-let [index-fn (:index-fn model)]
       (lazy-seq (index-fn spec))
       (do
         (log/warn (str "get-lexemes: no index found: using entire lexicon."))
         (flatten (vals
                   (or (:lexicon (:generate model)) (:lexicon model)))))))

   (map #(unify % spec))
   (filter #(not (= :fail %)))))

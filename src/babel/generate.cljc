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

(declare add-comp-to-bolts)
(declare add-comps-to-bolt)
(declare add-to-bolt-at-path)
(declare candidate-parents)
(declare get-lexemes)
(declare lightning-bolts)
(declare paths-for-bolt)
(declare gen)
(declare show-spec)

(defn generate
  "Return one expression matching spec _spec_ given the model _model_."
  [spec language-model]
  (log/debug (str "(generate) with model named: " (:name language-model)))
  (first (gen spec language-model 0)))

(defn gen
  "spec => trees"
  [spec model depth & [from-bolts at-path]]
  (log/debug (str "gen@" depth "; spec=" (show-spec spec)))
  (println (str "gen@" depth "; spec=" (show-spec spec)))
  (if (< depth 5)
    (lazy-cat
     (let [throw-exception-if-bolt-fails false
           bolts (or from-bolts (lightning-bolts model spec 0 depth))]
       (if (not (empty? bolts))
         (do
           (if (get-in (first bolts) [:phrasal] false)
             (println (str "bolt@" depth ":'" ((:morph-ps model) (first bolts)) "' at: " at-path))
             (println (str "lexeme@" depth ":'" ((:morph model) (first bolts)) "' at: " at-path)))
           (lazy-cat
            (let [for-this-bolt
                  (add-comps-to-bolt (first bolts) model
                                     (paths-for-bolt depth))]
              (if (and (empty? for-this-bolt) throw-exception-if-bolt-fails)
                (throw (Exception. (str "entire bolt failed:"
                                        ((:morph-ps model) (first bolts))))))
              for-this-bolt)
            (gen spec model depth
                 (rest bolts)
                 at-path)))))
     (if (not (= false (get-in spec [:phrasal] true)))
       (gen spec model (+ 1 depth) nil at-path)))))

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
                                (shufflefn (candidate-parents (:grammar model) spec depth)))]
      (if (not (empty? candidate-parents))
        (let [candidate-parent (first candidate-parents)]
          (lazy-cat
           (if (not (= false (get-in spec [:phrasal] true)))
             (let [unified-candidate-parent (unify candidate-parent spec)]
               (->> (lightning-bolts model
                                     (get-in unified-candidate-parent [:head])
                                     (+ 1 depth)
                                     max-depth)
                    (map (fn [head]
                           (assoc-in unified-candidate-parent [:head] head))))))
           (lightning-bolts model spec depth max-depth (rest candidate-parents))))))
    (shuffle (get-lexemes model spec))))

(defn add-comps-to-bolt
  "bolt + paths => trees"
  [bolt model paths-for-bolt]
  (if (not (empty? paths-for-bolt))
    (add-comp-to-bolts 
     (add-comps-to-bolt bolt model (rest paths-for-bolt))
     (first paths-for-bolt)
     model)
    [bolt]))

(defn add-comp-to-bolts
  "bolts + path => partial trees"
  [bolts path model]
  (if (not (empty? bolts))
    (lazy-cat
     (let [result
           (add-to-bolt-at-path (first bolts) path model)]
       (if (empty? result)
         (println "could not add any comps at path:" path " to bolt: " ((:morph-ps model) (first bolts)))
         (println "found one or more comps at path:" path " to bolt: " ((:morph-ps model) (first bolts))))
       result)
     (add-comp-to-bolts (rest bolts) path model))))

(defn paths-for-bolt [depth]
  (cond
    (= depth 0)
    []
    (= depth 1)
    [[:comp]]
    (= depth 2)
    [[:head :comp][:comp]]
    (= depth 3)
    [[:head :head :comp][:head :comp][:comp]]
    true
    (cons
     (concat (take (- depth 1) (repeatedly (fn [] :head))) [:comp])
     (paths-for-bolt (- depth 1)))))

(defn add-to-bolt-at-path
  "bolt + path => partial trees"
  [bolt path model]
  (->>
   ;; set of all complements at _path_ for _bolt_:
   (gen (get-in bolt path) model 0 nil path)
     
   ;; add each member _each_comp_ of this set to _bolt_:
   (map (fn [each-comp]
          (->
           bolt
           (dag_unify.core/assoc-in path
                                    each-comp)
           ((fn [tree]
              (if (:default-fn model)
                ((:default-fn model) tree)
                tree))))))))

(defn candidate-parents
  "find subset of _rules_ for which each member unifies successfully with _spec_; _depth_ is only used for diagnostic logging."
  [rules spec depth]
  (filter #(not (= :fail %))
          (mapfn (fn [rule]
                   (log/trace (str "candidate-parents: testing rule: " (:rule rule) "; depth: " depth))
                   (let [unified (unify spec rule)]
                     (if (= :fail unified)
                       (log/trace (str "candidate parent: " (:rule rule) " failed at:" (fail-path spec rule)))
                       (log/debug (str "candidate parent: " (:rule rule) " spec:" (show-spec spec)
                                       "; depth: " depth)))
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

(defn show-spec [spec]
  (str "cat=" (get-in spec [:synsem :cat])
       (if (get-in spec [:rule])
         (str "; rule=" (strip-refs (get-in spec [:rule]))))
       (if (get-in spec [:synsem :subcat :1 :cat])
         (str "; subcat1=" (strip-refs (get-in spec [:synsem :subcat :1 :cat]))))
       (if (get-in spec [:synsem :subcat :2 :cat])
         (str "; subcat2=" (strip-refs (get-in spec [:synsem :subcat :2 :cat]))))
       (if (get-in spec [:synsem :subcat :3 :cat])
         (str "; subcat3=" (strip-refs (get-in spec [:synsem :subcat :3 :cat]))))
       (if (not (= (get-in spec [:phrasal] ::none) ::none))
         (str "; phrasal=" (strip-refs (get-in spec [:phrasal]))))))


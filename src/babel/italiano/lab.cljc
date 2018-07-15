(ns babel.italiano.lab
  (:require
   [babel.directory] ;; this is needed even though there are no references to directory in here.
   [babel.generate :as g :refer [bolt bolts mini-bolts]]
   [babel.italiano :as italiano :refer [model morph morph-ps parse]]
   #?(:cljs [babel.logjs :as log])
   #?(:clj [clojure.tools.logging :as log])
   #?(:clj [clojure.repl :refer [doc]])
   [dag_unify.core :as u :refer [pprint strip-refs unify]]))

;; [H C]
;;
;;   H
;;  / \
;; H   C
;;
(def tree-1
  {:phrasal true
   :head {:phrasal false}
   :comp {:phrasal false}})

;; [[H C] C]
;;
;;     H
;;    / \
;;   H   C
;;  / \
;; H   C
;;
(def tree-2
  {:phrasal true
   :head tree-1
   :comp {:phrasal false}})

;; [H [H C]]
;;
;;    H
;;   / \
;;  H   C
;;     / \
;;    H   C
;;
(def tree-3
  {:phrasal true
   :head {:phrasal false}
   :comp tree-1})


;; [[H C] [H C]]
;;
;;      H
;;    /   \
;;   H     C
;;  / \   / \
;; H   C H   C
;;
(def tree-4
  {:phrasal true
   :head tree-1
   :comp tree-1})

;; [[H C] C]
;;
;;     H
;;    / \
;;   H   C
;;  / \
;; H   C
(def tree-5
  {:phrasal true
   :head tree-1
   :comp {:phrasal false}})

;; [[H C] [H [H C]]]
;;
;;      H
;;    /    \
;;   H      C
;;  / \    / \
;; H   C  H   C
;;       / \
;;      H   C
;;
(def tree-6
  {:phrasal true
   :head tree-1
   :comp tree-5})

(def tree-7
  {:phrasal true
   :comp tree-1
   :head {:phrasal true
          :head {:phrasal false}
          :comp {:phrasal true
                 :head tree-1
                 :comp tree-1}}})

(defn spec-to-comp-paths [spec]
  (cond
    (= true (u/get-in spec [:phrasal] true))
    [[:comp]]

    true
    []
    
    ;; comp is lexical:
    (and (not (= ::none (u/get-in spec [:comp] ::none)))
         (= ::none (u/get-in spec [:comp :head] ::none)))
    (concat
     [[:comp]]
     (spec-to-comp-paths (dissoc spec :comp)))
    
    ;; head is lexical:
    (and (not (= ::none (u/get-in spec [:head] ::none)))
         (= ::none (u/get-in spec [:head :head] ::none)))
    (spec-to-comp-paths (dissoc spec :head))

    ;; head is phrasal:
    (and (not (= ::none (u/get-in spec [:head] ::none)))
         (not (= ::none (u/get-in spec [:head :head] ::none))))
    (concat
     (map (fn [path]
            (vec (cons :head path)))
          (spec-to-comp-paths (u/get-in spec [:head])))
     (spec-to-comp-paths (dissoc spec :head)))
     
    ;; comp is phrasal:
    (and (not (= ::none (u/get-in spec [:comp] ::none)))
         (not (= ::none (u/get-in spec [:comp :head] ::none))))
    (concat
     [[:comp]]
     (map (fn [path]
            (vec (cons :comp path)))
          (spec-to-comp-paths (u/get-in spec [:comp])))
     (spec-to-comp-paths (dissoc spec :comp)))

    true []))

(defn get-rules
  "get all the rules that match spec."
  [model spec]
  (filter (fn [rule]
            (not (= :fail (unify rule spec))))
          (:grammar model)))

(defn get-lexemes
  "get all the lexemes that match spec."
  [model spec]
  (babel.generate/get-lexemes model spec))

(defn gen-one [spec model]
  (let [each-bolt (bolt spec model)]
    (log/debug (str ((:morph-ps model) each-bolt) ": adding bolts to paths:"
                    (clojure.string/join "|" (spec-to-comp-paths each-bolt))))
    (reduce (fn [tree-accumulator path]
              (log/debug (str ((:morph-ps model) tree-accumulator) ": adding bolt at: " path))
              (let [b (if true (bolt (u/get-in tree-accumulator path) model))]
                (if (and false (nil? b))
                  (throw (Exception. (str "failed to add a bolt to: "
                                          ((:morph-ps model) each-bolt)
                                          " at path:" path "; attempted spec:"
                                          (strip-refs (u/get-in tree-accumulator path))
                                          "; comp semantics: "
                                          (strip-refs (u/get-in tree-accumulator [:comp :synsem :sem]))
                                          "; comp-paths:"
                                          (clojure.string/join "|" (spec-to-comp-paths each-bolt))))))
                (log/debug (str "gen: "
                                ((:morph-ps model) each-bolt) "@" path ":"
                                ((:morph-ps model) b)))
                (if true
                  (u/assoc-in! tree-accumulator path b)
                  tree-accumulator)))
            each-bolt
            (spec-to-comp-paths each-bolt))))

(def object-is-pronoun {:head {:comp {:synsem {:pronoun true}}}})

(def basic
  {:modified false
   :synsem {:cat :verb
            :subcat []}})

(def specs 
  [
;   (unify tree-1 basic)
;   (unify tree-2 basic)
;   (unify tree-3 basic)
;   (unify tree-4 basic)
;   (unify tree-4 basic object-is-pronoun)
;   (unify tree-5 basic)
;   (unify tree-5 basic object-is-pronoun)
   (unify tree-6 basic {:synsem {:sem {:tense :present
                                       :aspect :perfect}}})
;   (unify tree-6 basic object-is-pronoun)
;   (unify tree-7 basic)
   ])

(def vedere-specs
  (map #(unify % {:synsem {:essere false}
                  ;;                  :root {:italiano {:italiano "vedere"}}})
                  :root {:italiano {:italiano :top}}})
       specs))


(def h-specs (map (fn [tree]
                    (u/get-in tree [:head]))
                  (take 1 (repeatedly #(gen-one (first (take 1 (shuffle vedere-specs))) model)))))

(defn hs []
  (let [h (first (take 1 (shuffle h-specs)))]
    (gen-one (strip-refs h) model)))

(defn sentence-one []
  (gen-one (first (take 1 (shuffle vedere-specs))) model))

(def fts
  [{:phrasal true}
   {:phrasal true
    :head {:phrasal true}}
   {:phrasal true
    :head {:done true}
    :comp :top}])

(defn frontier
  "get the next path to which to adjoin within _tree_."
  [tree]
  (cond

    (= (get-in tree [:done]) true)
    []
    
    (and (= (get-in tree [:phrasal] true))
         (= ::none (get-in tree [:head] ::none)))
    []
    
    (and (= (u/get-in tree [:phrasal]) true)
         (not (u/get-in tree [:done]))
         (not (u/get-in tree [:head :done])))
    (concat [:head] (frontier (u/get-in tree [:head])))

    (and (= (u/get-in tree [:phrasal]) true))
    (concat [:comp] (frontier (u/get-in tree [:comp])))
    
    true []
    
    true
    [(str "(unhandled): " ((:morph-ps model) tree))]))

(defn add-children [tree]
  (let [path (frontier tree)
        depth (count path)
        child-spec (u/get-in tree path)

        ;; the higher the constant,
        ;; the more likely we'll generate leaves
        ;; (terminal nodes) rather than trees.
        pruning-factor #(+ % 50)]
    (cond
      (= true (u/get-in child-spec [:phrasal]))
      (mini-bolts child-spec model)
      
      (= false (u/get-in child-spec [:phrasal]))
      (babel.generate/get-lexemes model child-spec)
      
      ;; TODO: concat: trees and lexemes: order depends on rand-int.
      ;; 
      (= 0 (rand-int (pruning-factor depth)))
      (mini-bolts child-spec model)
      
      true
      (babel.generate/get-lexemes model child-spec))))

(defn add-child [tree]
  (first (add-children tree)))

(defn sprouts [spec]
  (->>
   (mini-bolts spec model)

;;   (filter #(not (= % :fail)))

   (mapcat (fn [g]
             (let [f (frontier g)]
               (map (fn [child]
                      (-> g
                          (u/assoc-in! f child)))
                    (add-children g)))))

   (map (fn [g]
          (if (= true (u/get-in g [:comp :done]))
            (-> g
                (u/assoc-in! {:done true}))
            g)))
   
   (filter #(not (= % :fail)))))

(defn onegoon [tree]
  (let [f (frontier tree)]
    (if (not (empty? f))
      (let [tree-with-child (u/assoc-in! tree f (first (add-children tree)))]
        (-> (if (= true (u/get-in tree-with-child (concat f [:done])))
              (u/assoc-in! tree-with-child (butlast f) {:done true})
              tree-with-child)
            (onegoon)))
      tree)))

(defn gen [spec]
  (first (take 1 (map (fn [sprout]
                        (onegoon sprout))
                      (sprouts spec)))))

(def spec
  {:modified false,
   :root {:italiano {:italiano "chiamarsi"}},
   :synsem {:cat :verb, :subcat []},
   :rule "s-present-phrasal"})

;;(repeatedly #(println (morph-ps (gen spec))))

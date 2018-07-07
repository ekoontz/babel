(ns babel.italiano.lab
  (:require
   [babel.directory] ;; this is needed even though there are no references to directory in here.
   [babel.generate :refer [bolt bolts]]
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

(declare get-mini-tree)

(defn gen-mini [spec model depth]
  (let [bolt
        (first (babel.generate/lightning-bolts model spec 0 1))]
    (if true
      (u/assoc-in! bolt [:comp]
                   (get-mini-tree (u/get-in bolt [:comp]) model (+ depth 1)))
      bolt)))

(defn get-mini-tree [spec model depth]
  (cond
    (= false (u/get-in spec [:phrasal] ::none))
    (first (shuffle (get-lexemes model spec)))

    (= true (u/get-in spec [:phrasal] true))
    (gen-mini spec model depth)
    
    (< depth (rand-int 4))
    (gen-mini spec model depth)

    true (first (shuffle (get-lexemes model spec)))))


;;(def v+b (u/assoc-in (unify tree-3 basic) [:head] (nth (get (:lexicon model) "vedere") 0)))

(defn foo []
  (repeatedly 
   #(println 
     (morph-ps
      (time
       (get-mini-tree {:synsem {:cat :noun}}
                      model 0))))))

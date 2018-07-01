(ns babel.italiano.lab
  (:require
   [babel.directory] ;; this is needed even though there are no references to directory in here.
   [babel.generate :refer [bolt]]
   [babel.italiano :as italiano :refer [model morph morph-ps]]
   #?(:cljs [babel.logjs :as log])
   [clojure.pprint :refer [pprint]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:clj [clojure.repl :refer [doc]])
   [dag_unify.core :as u :refer [strip-refs unify]]))

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

(defn spec-to-comp-paths [spec]
  (cond

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

(defn gen2 [spec model]
  (let [tree (bolt spec model)]
    (log/info (str "generating with top-level bolt: " ((:morph-ps model) tree)))
    (reduce (fn [tree-accumulator path]
              (u/assoc-in! tree-accumulator path
                           (bolt model (u/get-in tree-accumulator path))))
            tree
            (if true [[:comp]] (spec-to-comp-paths tree)))))

(defn gen-all [spec model]
  (mapcat (fn [tree]
            (reduce (fn [tree-accumulator path]
                      (u/assoc-in! tree-accumulator path
                                   (bolt model (u/get-in tree-accumulator path))))
                    tree
                    (spec-to-comp-paths tree)))
          (babel.generate/bolts spec model)))

(def object-is-pronoun {:head {:comp {:synsem {:pronoun true}}}})

(def basic
  {:modified false
   :synsem {:cat :verb
            :subcat []}})

(def specs 
  [
   (unify tree-1 basic)
   (unify tree-2 basic)
   (unify tree-3 basic)
   (unify tree-4 basic)
   (unify tree-4 basic object-is-pronoun)
   (unify tree-5 basic)
   (unify tree-5 basic object-is-pronoun)
   (unify tree-6 basic)
   (unify tree-6 basic object-is-pronoun)
   ])

(def vedere-specs
  (map #(unify % {:synsem {:essere false}
                  :root {:italiano {:italiano "vedere"}}})
       specs))

(def t1 (unify tree-1 basic {:root {:italiano {:italiano "vedere"}}}))
(def t2 (unify tree-2 basic {:synsem {:essere false}
                             :root {:italiano {:italiano "vedere"}}}))
(def t3 (unify tree-3 basic {:synsem {:essere false}
                             :root {:italiano {:italiano "vedere"}}}))

(defn s3 []
  (repeatedly #(println (time (let [s (gen2 t3 model)] (if (keyword? s) s (morph s)))))))

(defn sentence []
  (gen2 (first (take 1 (shuffle vedere-specs))) model))


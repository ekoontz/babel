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

(defn gen-one [spec model]
  (let [each-bolt (bolt spec model)]
    (reduce (fn [tree-accumulator path]
              (let [b (bolt (u/get-in tree-accumulator path) model)]
                (if (nil? b)
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
                (u/assoc-in! tree-accumulator path b)))
            each-bolt
            (spec-to-comp-paths each-bolt))))

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

(defn sentence-one []
  (gen-one (first (take 1 (shuffle vedere-specs))) model))




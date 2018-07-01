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
(def tree-1
  {:head {:phrasal false}
   :comp {:phrasal false}})

;; [[H C] C]
;;
;;     H
;;    / \
;;   H   C
;;  / \
;; H   C
(def tree-2
  {:head {:comp {:phrasal false}
          :head {:phrasal false}}
   :comp {:phrasal false}})

;; [H [H C]]
;;
;;    H
;;   / \
;;  H   C
;;     / \
;;    H   C
(def tree-3
  {:head {:phrasal false}
   :comp {:phrasal true
          :head {:phrasal false}
          :comp {:phrasal false}}})

;; [[H C] [H C]]
;;
;;      H
;;    /   \
;;   H     C
;;  / \   / \
;; H   C H   C
(def tree-4
  {:head {:phrasal true
          :comp {:phrasal false}
          :head {:phrasal false}}
   :comp {:phrasal true
          :comp {:phrasal false}
          :head {:phrasal false}}})

;; [[H C] [H [H C]]]
;;
;;      H
;;    /   \
;;   H     C
;;  / \   / \
;; H   C H   C
;;      / \
;;     H   C
(def tree-5
  {:head {:phrasal true
          :comp {:phrasal false}
          :head {:phrasal false}}
   :comp {:phrasal true
          :comp {:phrasal false}
          :head {:phrasal true
                 :comp {:phrasal false}
                 :head {:phrasal false}}}})

(defn spec-to-path [spec]
  (cond
    ;; tree-5: [[H C] [H [H C]]]
    (and (= false (u/get-in spec [:head :comp :phrasal]))
         (= false (u/get-in spec [:head :head :phrasal]))
         (= true (u/get-in spec [:comp :head :phrasal])))
    [[:head :comp][:comp][:comp :head :comp][:comp :comp]]

    ;; tree-4: [[H C] [H C]]
    (and (= false (u/get-in spec [:head :comp :phrasal]))
         (= false (u/get-in spec [:head :head :phrasal]))
         (= false (u/get-in spec [:comp :head :phrasal])))
    [[:head :comp][:comp][:comp :comp]]

    ;; tree-3: [H [H C]]
    (and (= false (u/get-in spec [:head :phrasal]))
         (= false (u/get-in spec [:comp :comp :phrasal]))
         (= false (u/get-in spec [:comp :head :phrasal])))
    [[:comp] [:comp :comp]]

    ;; tree-2: [[H C] C]
    (and (= false (u/get-in spec [:head :comp :phrasal]))
         (= false (u/get-in spec [:head :head :phrasal]))
         (= false (u/get-in spec [:comp :phrasal])))
    [[:head :comp] [:comp]]

    ; tree-1: [H C]
    (and (= false (u/get-in spec [:head :phrasal]))
         (= false (u/get-in spec [:comp :phrasal])))
    [[:comp]]

    ))
    
(defn gen [tree paths model]
  (if (not (empty? paths))
    (let [path (first paths)]
      (gen
       (u/assoc-in! tree path
                    (bolt model (u/get-in tree path)))
       (rest paths) model))
    tree))

(defn gen2 [spec model]
  (let [bolt (bolt model spec)]
    (log/debug (str "top bolt: " ((:morph-ps model) bolt)))
    (gen bolt
         (spec-to-path spec)
         model)))

(def object-is-pronoun {:head {:comp {:synsem {:pronoun true}}}})

(def basic
  {:modified false
   :synsem {:cat :verb
            :subcat []}})

(def specs 
  [(unify tree-1 basic)
   (unify tree-2 basic)
   (unify tree-3 basic)
   (unify tree-4 basic)
   (unify tree-4 basic object-is-pronoun)
   (unify tree-5 basic)
   (unify tree-5 basic object-is-pronoun)])

(defn sentence []
  (let [specs (map #(unify % {:root {:italiano {:italiano "vedere"}}})
                   specs)]
    (gen2 (first (take 1 (shuffle specs))) model)))





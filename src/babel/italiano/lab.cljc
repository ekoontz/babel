(ns babel.italiano.lab
  (:require
   [babel.directory] ;; this is needed even though there are no references to directory in here.
   [babel.generate :as g :refer [frontier gen get-lexemes minitrees]]
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
   (unify tree-6 basic {:synsem {:sem {:tense :present
                                       :aspect :perfect}}})
   (unify tree-6 basic object-is-pronoun)
;;   (unify tree-7 basic)
   ])

(def vedere-specs
  (map #(unify % {:synsem {:essere false}
                  :root {:italiano {:italiano "vedere"}}})
       specs))

(def fts
  [{:phrasal true}
   {:phrasal true
    :head {:phrasal true}}
   {:phrasal true
    :head {:done true}
    :comp :top}])

(defn downtown []
  (let [spec
        {:synsem {:cat :verb
                  :subcat []}
         :modified false}]
    (repeatedly #(println (morph (gen spec model))))))

(defn basecamp []
  (let [semantic-spec
        {:modified false,
         :synsem {:cat :verb, :subcat []
                  :sem {:aspect :simple
                        :pred :be-called
                        :tense :present}}}
        root-spec
        {:modified false,
         :root {:italiano {:italiano "chiamarsi"}}
         :synsem {:cat :verb, :subcat []
                  :sem {:aspect :simple
                        :tense :present}}}

        all-of-the-specs (concat specs [root-spec semantic-spec])]
        
    (repeatedly #(println (morph (gen
                                  (nth all-of-the-specs (rand-int (count all-of-the-specs)))
                                  model))))))
(defn nextcamp []
  (let [spec (unify tree-7 basic)]
    (gen spec model)))

(defn refresh []
  (babel.test.test/init-db)
  (babel.directory/refresh-models))

;;(map #(println (morph %)) (grow (sprouts spec model) model))

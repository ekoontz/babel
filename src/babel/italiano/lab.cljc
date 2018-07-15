(ns babel.italiano.lab
  (:require
   [babel.directory] ;; this is needed even though there are no references to directory in here.
   [babel.generate :as g :refer [bolt bolts get-lexemes sprouts]]
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

(defn grow [trees]
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
                                 (= true (u/get-in child [:done])))
                          (u/assoc-in! tree-with-child (butlast f) {:done true})
                          tree-with-child))))))
         [tree])
       (grow (rest trees))))))

(defn gen [spec model]
  (first (take 1 (grow (sprouts spec model)))))

(def semantic-spec
  {:modified false,
   :synsem {:cat :verb, :subcat []
            :sem {:aspect :simple
                  :pred :be-called
                  :tense :present}}})

(def root-spec
  {:modified false,
   :root {:italiano {:italiano "chiamarsi"}}
   :synsem {:cat :verb, :subcat []
            :sem {:aspect :simple
                  :tense :present}}})

(def spec root-spec)

;;(repeatedly #(println (morph-ps (gen spec model))))

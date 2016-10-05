(ns babel.over
  (:refer-clojure :exclude [get get-in resolve find parents])
  (:require
   [babel.exception :refer [exception]]
   [babel.lexiconfn :refer [get-fail-path]]
   [clojure.string :as string]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [dag_unify.core :refer [copy fail? fail-path fail-path-between get-in strip-refs unify unifyc
                           ;; temporary: until we move (truncate) from here to dag_unify.
                           deserialize dissoc-paths serialize
]]))

;; TODO: need better debugging throughout this file to diagnose generation failures.
;; using (get-fail-path) is one example.

;; use map or pmap.
(def ^:const mapfn pmap)
(def ^:dynamic *extra-diagnostics* false)

(defn overh
  "add given head as the head child of the phrase: parent."
  [parent head]
  ;; TODO: get rid of all this type-checking and use
  ;; whatever people use for Clojure argument type-checking.
  (cond
    (or (seq? head)
        (vector? head))
    (mapcat (fn [child]
              (overh parent child))
            head)
    true
    ;; TODO: 'true' here assumes that both parent and head are maps: make this assumption explicit,
    ;; and save 'true' for errors.
    (let [result (unify (copy parent)
                        {:head (copy head)})
          
          is-fail? (= :fail result)]
      (if (not is-fail?)
        (list result)))))

;; Haskell-looking signature:
;; (parent:map) X (child:{set,seq,fs}) => list:map
;; TODO: verify that the above comment about the signature
;; is still true.
(defn overc [parent comp]
  "add given child as the comp child of the phrase: parent."
  (cond
   (or (seq? parent)
       (vector? parent))
   (let [parents (lazy-seq parent)]
     (mapcat (fn [parent]
               (overc parent comp))
             parents))

   (vector? comp)
   (overc parent (lazy-seq comp))

   (seq? comp)
   (let [comp-children comp]
     (mapcat (fn [child]
               (overc parent child))
             comp-children))
   true
   (let [result (unifyc parent
                        {:comp comp})
         is-fail? (= :fail result)]
     (if (not is-fail?)
       (do
         (log/debug (str "overc: " (get-in parent [:rule]) " -> " (get-in comp [:rule]
                                                                          (get-in comp [:synsem :sem :pred]
                                                                                  "(no pred for comp)"))))
         ;; TODO: why are we returning a list here rather than just the result?
         (list result))))))

(defn overhc [parent head comp]
  (-> parent
      (overh head)
      (overc comp)))

;; TODO: distinguish between when:
;; 1) called with only a child1 (no child2),
;; 2) called with both a child1 and a child2, but child2's supplied value is nil:
;;    should be treated the same as empty list.
(defn over [parents child1 & [child2]]
  (cond (map? parents)
        (over (list parents) child1 child2)

        true
        (mapcat
         (fn [parent]
           (let [[head comp] (if (= (:first parent) :head)
                               [child1 child2]
                               [child2 child1])]
             (overhc parent head comp)))
         parents)))

(defn morph-with-recovery [morph-fn input]
  (if (nil? input)
    (exception (str "don't call morph-with-recovery with input=nil.")))
  (if (nil? morph-fn)
    (exception (str "don't call morph-with-recovery with morph-fn=nil.")))
  (let [result (morph-fn input)
        result (if (or (nil? result)
                       (= "" result))
                 (get-in input [:english :english] "")
                 result)
        result (if (or (nil? result)
                       (= "" result))
                 (get-in input [:english] "")
                 result)
        result (if (or (nil? result)
                       (= "" result))
                 (get-in input [:rule] "")
                 result)
        result (if (or (nil? result)
                       (= "" result))
                 (exception
                  (str "r5: " input "/" (nil? input)))
                 result)]
    result))

(defn show-bolt [bolt language-model]
  (if (nil? bolt)
    (exception (str "don't call show-bolt with bolt=null."))
    (let [morph (:morph language-model)]
      (if (nil? morph)
        (exception (str "don't call show-bolt with morph=null."))
        (str "[" (get-in bolt [:rule])
             " '" (morph-with-recovery morph bolt) "'"
             (let [head-bolt (get-in bolt [:head])]
               (if (not (nil? head-bolt))
                 (let [rest-str (show-bolt (get-in bolt [:head]) language-model)]
                   (if (not (nil? rest-str))
                     (str " -> " rest-str)))))
             "]")))))

(defn subpath? [path1 path2]
  "return true if path1 is subpath of path2."
  (if (empty? path1)
    true
    (if (= (first path1) (first path2))
      (subpath? (rest path1)
                (rest path2))
      false)))

(defn truncate [input truncate-paths language-model]
  (log/debug (str "truncating@" truncate-paths ":" (show-bolt input language-model)))
  (let [serialized (if (:dag_unify.core/serialized input)
                     (:dag_unify.core/serialized input)
                     (serialize input))
        paths-and-vals (rest serialized)
        path-sets (mapfn first paths-and-vals)
        path-vals (mapfn second paths-and-vals)
        truncated-path-sets (mapfn
                             (fn [path-set] 
                               (filter (fn [path] 
                                         (not (some (fn [truncate-path]
                                                      (subpath? truncate-path path))
                                                    truncate-paths)))
                                       path-set))
                             path-sets)
        skeleton (first serialized)
        truncated-skeleton (dissoc-paths skeleton truncate-paths)
        truncated-serialized
        (cons truncated-skeleton
              (zipmap truncated-path-sets
                      path-vals))]
    (deserialize truncated-serialized)))

(defn truncate-expressions [expressions truncate-paths language-model]
  (map #(truncate % truncate-paths language-model)
       expressions))

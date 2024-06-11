(ns babel.unify-compat
  (:require [clojure.string :refer [join]]
            [dag_unify.core :as u :refer [fail? ref? unify]]
            [dag_unify.diagnostics :as diag :refer [strip-refs]]
            [dag_unify.dissoc :as dissoc]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [babel.logjs :as log]) 
            [dag_unify.serialization :refer [create-path-in serialize]]))

;;(defn strip-refs [& args]
;;  (apply diag/strip-refs args))

(defn unifyc [& args]
  (->> args
       (reduce u/unify)))

(defn dissoc-paths
  "dissoc a path from a map; e.g.:
    (dissoc-paths
     {:a {:b 42
          :c 43}}
     [[:a :b]])
  
     =>
  
    {:a {:c 43}}."
  [fs & [paths]]
  (cond (empty? paths)
        fs

        (seq? fs)
        (map #(dissoc-paths % paths) fs)

        (ref? fs)
        (dissoc-paths @fs paths)

        (keyword? fs)
        fs

        (empty? fs)
        :top

        true
        (let [path (first paths)]
          (dissoc-paths
           (let [fs (dissoc fs ::serialized) ;; remove the existing serialized version of the serialized structure, since
                 ;; it will not be valid after we've altered the structure itself.
                 feature (first path)]
             (cond
               (and
                (empty? (rest path))
                (empty? (dissoc fs feature)))
               :top

               (empty? (rest path))
               (dissoc fs feature)

               (not (= :notfound (get-in fs (list feature) :notfound)))
               (conj
                {feature (dissoc-paths (get-in fs (list feature)) (list (rest path)))}
                (dissoc fs feature))

               true
               fs))
           (rest paths)))))

(defn exists? [the-map path]
  (not (= :does-not-exist
          (get-in the-map path :does-not-exist))))

(defn label-of [parent]
  (if (:rule parent) (:rule parent) (:comment parent)))

(defn recursive-dissoc
  "like dissoc, but works recursively. Only works on reference-free maps (contains no atoms)."
  [a-map pred]
  (if (not (empty? a-map))
    (let [k (first (first a-map))
          v (second (first a-map))]
      (if (pred k)
        (recursive-dissoc (dissoc a-map k)
                          pred)
        (conj
         {k (cond (map? v)
                  (recursive-dissoc v pred)
                  true v)}
         (recursive-dissoc (dissoc a-map k)
                           pred))))
    {}))

(defn deserialize-with-remove [serialized pred]
  (let [base (recursive-dissoc (second (first serialized)) pred)]
    (apply merge
           (let [all
                 (cons base
                       (flatten
                        (map (fn [paths-val]
                               (let [paths (first paths-val)
                                     val (atom
                                          (cond (map? atom)
                                                (recursive-dissoc
                                                 (second paths-val)
                                                 pred)
                                                true
                                                (second paths-val)))]
                                 (map (fn [path]
                                        (if (empty?
                                             (remove false?
                                                     (map (fn [key-in-path]
                                                            (pred key-in-path))
                                                          path)))
                                          (create-path-in path val)))
                                      paths)))
                             (rest serialized))))]
             all))))

(defn remove-matching-keys [fs pred]
  (let [serialized (serialize fs)]
    (deserialize-with-remove serialized pred)))

(defn- pathify
  "Transform a map into a map of paths/value pairs,
  where paths are lists of keywords, and values are atomic values.
  e.g.:
  {:foo {:bar 42, :baz 99}} =>  { { (:foo :bar) 42}, {(:foo :baz) 99} }
  The idea is to map the key :foo to the (recursive) result of pathify on :foo's value."
  [fs & [prefix]]
  (mapcat (fn [kv]
            (let [key (first kv)
                  val (second kv)]
              (if true
                (if (map? val)
                  (pathify val (concat prefix (list key)))
                  (if (and (ref? val)
                           (let [val @val]
                             (map? val)))
                    (pathify @val (concat prefix (list key)))
                    [{(concat prefix (list key))
                      (if (ref? val) @val ;; simply resolve references rather than trying to search for graph isomorphism.
                          val)}])))))
          fs))

;; TODO: use a reduce or recur here rather
;; than simply recursion
(defn find-fail-in [fs1 fs2 paths]
  (if (not (empty? paths))
    (let [path (first paths)
          val1 (get-in fs1 path :top)
          val2 (get-in fs2 path :top)]
      (log/info (str "looking at: " val1 " and " val2 " unify=> " (unify val1 val2) " and path: " (vec path)))
      (if (fail? (unify val1 val2))
        {:fail-path (str "/" (join "/" path))
         :val1 (strip-refs val1)
         :val2 (strip-refs val2)}
        (find-fail-in fs1 fs2 (rest paths))))))

(defn fail-path-between
  "If unifying fs1 and fs2 leads to a fail somewhere, show the path to the fail. Otherwise return nil. Not efficient: use only for diagnostics."
  [fs1 fs2]
  (let [paths-in-fs1 (map #(first (first %)) (pathify fs1))
        paths-in-fs2 (map #(first (first %)) (pathify fs2))]
    (find-fail-in fs1 fs2 (concat paths-in-fs1 paths-in-fs2))))

(ns babel.dagcompat
  (:require [dag_unify.core :refer [ref?]]
            [dag_unify.serialization :refer [create-path-in serialize]]))

(defn dissoc-paths [fs & [paths]]
  "dissoc a path from a map; e.g.:

    (dissoc-paths
     {:a {:b 42
          :c 43}}
     [[:a :b]])

     =>

    {:a {:c 43}}."

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
           (let [fs (dissoc fs :dag_unify.serialization/serialized) ;; remove the existing serialized version of the serialized structure, since
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



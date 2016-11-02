(ns babel.salad)

(def salad
  {:name "salad"
   :language "salad"
   :grammar [
             (let [surface-a (atom :top)
                   surface-b (atom :top)]
               {:rule "rule1"
                :surface {:a surface-a
                          :b surface-b}
                :head {:surface surface-a}
                :comp {:surface surface-b}})]
   :lexicon
   (into {}
         (map (fn [[k v]]
                [k (merge v {:surface k})])
              {"the" {:foo :a}
               "dog" {:foo :b}
               "cat" {:foo :c}
               "sleeps" {:foo :d}}))})

(defn morph [input]
  (cond
    (and (map? input)
         (not (nil? (dag_unify.core/get-in input [:surface]))))
    (morph (dag_unify.core/get-in input [:surface]))
    
    (map? input)
    (str (morph (dag_unify.core/get-in input [:a])) " "
         (morph (dag_unify.core/get-in input [:b])))
    true input))

  




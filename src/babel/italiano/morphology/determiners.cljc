(ns babel.italiano.morphology.determiners
  (:refer-clojure :exclude [get-in resolve])
  (:require
   [babel.dagcompat :refer [dissoc-paths]]
   [clojure.string :as string]
   [clojure.string :refer (trim)]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log])))

(def patterns
  (-> "babel/italiano/morphology/determiners.edn"
      clojure.java.io/resource
      slurp
      read-string))

(defn apply-determiner-rules [input]
  (let [results
        (remove nil?
                (map (fn [[from to]]
                       (if (re-find from input)
                         (clojure.string/replace 
                          input
                    from to)))
                     (map #(get % :g)
                          (filter (fn [pattern-map]
                                    (some #(= :g %) (keys pattern-map)))
                                  patterns))))]
    (if (not (empty? results))
      [true (first results)]
      [false input])))

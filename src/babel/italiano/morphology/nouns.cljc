(ns babel.italiano.morphology.nouns
  (:refer-clojure :exclude [get-in resolve])
  (:require
   [clojure.string :as string]
   [clojure.string :refer (trim)]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log])
   [dag_unify.core :refer (copy dissoc-paths fail? get-in ref? strip-refs unifyc)]))

(def patterns {})

(def exceptions-rules
  [{:path [:italiano :plur]
    :merge-fn
    (fn [val]
      {:synsem {:cat :noun}
       :italiano {:agr {:number :plur}}})}])


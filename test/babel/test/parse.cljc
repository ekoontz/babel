(ns ^{:doc "Parsing Testing Code"}
    babel.test.parse
  (:refer-clojure :exclude [get-in])
  (:require [babel.francais.lexicon :refer [lexicon]]
            [babel.francais.morphology :refer [analyze fo]]
            [babel.over :as over]
            [babel.parse :refer [parse tokenizer]]
            [clojure.string :as string]
            #?(:clj [clojure.test :refer [deftest is]])
            #?(:cljs [cljs.test :refer-macros [deftest is]])
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [babel.logjs :as log]) 
            [dag_unify.core :refer [fail? get-in]]
            [dag_unify.diagnostics :refer [fail-path strip-refs]]))

(deftest split
  (is (= 2 (count (string/split "je suis" tokenizer)))))

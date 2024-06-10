(ns babel.espanol.morphology.nouns
  (:refer-clojure :exclude [get-in resolve])
  (:require [babel.unify-compat :refer [dissoc-paths unifyc]]
            [clojure.string :as string]
            [clojure.string :refer (trim)]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [babel.logjs :as log])
            [dag_unify.core :refer (copy fail? get-in ref?)]
            [dag_unify.diagnostics :refer [strip-refs]]))



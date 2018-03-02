(ns babel.directory
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.english.grammar :as en]
   [babel.espanol.grammar :as es]
   [babel.francais.grammar :as fr]
   [babel.latin :as la]
   [babel.italiano.grammar :as it]
   [clojure.tools.logging :as log]
   [dag_unify.core :refer [get-in unify]]))

(def models
  {:en (delay (en/medium))
   :es (delay (es/small))
   :fr (delay (fr/medium))
   :la (delay (la/model))
   :it (delay (it/medium))})


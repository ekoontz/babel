(ns babel.test.en
  (:refer-clojure :exclude [get-in])
  (:require [babel.engine :as engine]
            [babel.english.writer :as en]
            [babel.english.morphology :refer [fo]]
            [babel.writer :as writer]
            [clojure.string :as string]
            [clojure.test :refer :all]
            [clojure.tools.logging :as log]
            [dag-unify.core :refer [get-in]]))




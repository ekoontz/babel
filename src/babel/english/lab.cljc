(ns babel.english.lab
  (:require
   [babel.directory :refer [models]] ;; this is needed even though there are no references to directory in here.
   [babel.generate :as g :refer [frontier generate get-lexemes]]
   [babel.english :as english :refer [model morph morph-ps parse]]
   #?(:cljs [babel.logjs :as log])
   #?(:clj [clojure.tools.logging :as log])
   #?(:clj [clojure.repl :refer [doc]])
   [dag_unify.core :as u :refer [pprint strip-refs unify]]))

(defn downtown []
  (let [spec
        {:synsem {:cat :verb
                  :subcat []
                  :sem {:aspect :simple
                        :pred :top
                        :tense :present}}}]
    (repeatedly #(println
                  (morph (generate
                          spec
                          model)
                         :show-notes false)))))
(defn refresh []
  (let [refresh-lexicon false]
    (babel.test.test/init-db)
    (if refresh-lexicon (babel.lexiconfn/write-lexicon "en" (babel.english.grammar/compile-lexicon)))
    (babel.directory/refresh-models)
    (load "../english")))




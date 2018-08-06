(ns babel.english.lab
  (:require
   [babel.directory :refer [models]] ;; this is needed even though there are no references to directory in here.
   [babel.generate :as g :refer [frontier generate get-lexemes]]
   [babel.english :as english :refer [model morph morph-ps]]
   #?(:cljs [babel.logjs :as log])
   #?(:clj [clojure.tools.logging :as log])
   #?(:clj [clojure.repl :refer [doc]])
   [dag_unify.core :as u :refer [pprint strip-refs unify]]))

(defn parse [surface-string]
  (english/parse surface-string model false))

(defn downtown []
  (let [spec
        {:synsem {:cat :verb
                  :subcat []
                  :sem {:aspect :simple
                        :tense :present}}}]
    (repeatedly #(println
                  (morph (generate
                          spec
                          model)
                         :show-notes false)))))

(defn basecamp []
  (let [spec
        {:synsem {:cat :verb
                  :sem {:pred :give-x-to-y
                        :obj {:pred :cat
                              :mod {:first {:pred :black}}}}
                  :subcat []}
         :head {:head {:comp {:phrasal true}}}}]
    (repeatedly #(println
                  (morph (time (generate spec model))
                         :show-notes false)))))

(defn nextcamp []
  (let [parse (-> "the small dogs you see" parse first)]
    (pprint (u/get-in parse [:synsem :sem]))))

(defn refresh []
  (let [refresh-lexicon false]
    (babel.test.test/init-db)
    (if refresh-lexicon (babel.lexiconfn/write-lexicon "en" (babel.english.grammar/compile-lexicon)))
    (babel.directory/refresh-models)
    (load "../english")))




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
  (let [semantic-spec
        {:synsem {:cat :verb
                  :subcat []
                  :sem {:aspect :simple
                        :pred :top
                        :tense :present}}}
        all-of-the-specs [semantic-spec]]
    (repeatedly #(println
                  (morph (generate
                          (nth all-of-the-specs
                               (rand-int (count all-of-the-specs)))
                          model)
                         :show-notes false)))))

(defn basecamp []
  (let [spec
        {:rule "noun-phrase"
         :synsem {:cat :noun
                  :mod {:first {:pred :red}}
                  :sem {:pred :top
                        :number :sing
                        :spec {:def :def
                               :pred :definite}}}}]
    (generate spec model)))

(defn nextcamp []
  (let [spec
        {:rule "noun-phrase"
         :synsem {:cat :noun
                  :mod {:first {:pred :top}}
                  :sem {:pred :top
                        :number :sing}}}]
    (if false (println (morph (generate spec model)))
        (generate spec model))))

(defn refresh []
  (let [refresh-lexicon false]
    (babel.test.test/init-db)
    (if refresh-lexicon (babel.lexiconfn/write-lexicon "en" (babel.english.grammar/compile-lexicon)))
    (babel.directory/refresh-models)
    (load "../english")))





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
        {:modified false,
         :synsem {:cat :verb, :subcat []
                  :sem {:aspect :simple
                        :pred :sleep
                        :tense :present}}}
        all-of-the-specs [semantic-spec]]
    (repeatedly #(println
                  (morph (generate
                          (nth all-of-the-specs
                               (rand-int (count all-of-the-specs)))
                          model))))))

(defn basecamp []
  (let [semantic-spec
        {:modified false,
         :synsem {:cat :verb, :subcat []
                  :sem {:aspect :simple
                        :pred :sleep
                        :tense :present}}}
        all-of-the-specs [semantic-spec]]
    (repeatedly #(println
                  (morph-ps (time
                             (first
                              ((:default-fn model)
                               (generate
                                (nth all-of-the-specs
                                     (rand-int (count all-of-the-specs)))
                                model)))))))))

(defn nextcamp []
  (let [spec {:rule "noun-phrase3" :synsem {:cat :noun :sem {:spec {:pred :definite}} :subcat []}}]
      (println (morph (generate spec model)))))

(defn refresh []
  (babel.test.test/init-db)
  (babel.lexiconfn/write-lexicon "en" (babel.english.grammar/compile-lexicon))
  (babel.directory/refresh-models)
  (load "../english"))



  

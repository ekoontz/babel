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

;; (def spec {:synsem {:cat :noun, :subcat []}})
;; (let [foo (generate spec model)] (println (morph-ps foo)))

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




(ns babel.english.demo
  (:require
   [babel.english :refer [generate model]]
   [babel.english.morphology :refer [fo]]
   #?(:cljs [babel.logjs :as log])
   [clojure.string :as string]
   #?(:clj [clojure.tools.logging :as log])
   [dag_unify.core :refer [unify]]))

(declare run-demo-with)

;; during generation, will not decend deeper than this when creating a tree:
;; should also be possible to override per-language.
(def ^:const max-total-depth 25)

;; run each (generate) as (time (generate)):
(def ^:const timings? false)

(defn demo [ & [n spec]]
  (let [demo-specs
        [{:demo "Dog noun phrases"
          :synsem {:cat :noun
                   :sem {:pred :dog}}}

         {:demo "Sentences about dogs eating"
          :synsem {:cat :verb
                   :sem {:subj {:pred :dog}
                         :pred :eat}}}

         {:demo "The adventures of Luisa's yellow cat"
          :synsem {:cat :verb
                   :sem {:subj {:pred :cat
                                :mod {:first {:pred :yellow}}
                                :spec {:def :possessive
                                       :of {:pred :luisa}}}
                         :obj {:pred :top}}}}

         {:demo "Women who read books"
          :synsem {:cat :verb
                   :sem {:subj {:pred :woman}
                         :pred :read
                         :obj {:pred :book}}}}

         {:demo "Thinking"
          :synsem {:cat :verb
                   :sem {:pred :think
                         :obj {:pred :top}}}}

         ]]
    
    (doall (map (fn [spec]
                  (let [log-message 
                        (str "running demo: " (:demo spec) "..")]
                    (do (if timings? (log/info log-message))
                        (println)
                        (println log-message)
                        (println)
                        (let [language-model model
                              expressions (run-demo-with n (dissoc spec :lm) language-model)]
                          (count (pmap (fn [expression]
                                         (let [formatted (fo expression :show-notes false)]
                                           (println
                                            (str (string/capitalize (subs formatted 0 1))
                                                 (subs formatted 1 (count formatted))
                                                 "."))))
                                       expressions))))))
                (if (and spec (not (empty? (string/trim spec))))
                  (filter #(= (:demo %)
                              spec)
                          demo-specs)
                  demo-specs)))
    (println (str "babel demo is finished."))))

(defn run-demo-with [n spec model]
  "print out _n_ generated sentences to stdout."
  (let [n (if n (Integer. n)
              10)
        spec (if spec (unify spec
                             {:synsem {:subcat '()}}
                             {:modified false})
                 {:modified false})]
    (filter #(not (nil? %))
            (if timings?
              (take n (repeatedly
                       #(time
                         (generate spec model))))
              (take n (repeatedly
                       #(generate spec model)))))))




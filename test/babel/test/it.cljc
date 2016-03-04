(ns babel.test.it
  (:refer-clojure :exclude [get-in])
  (:require [babel.italiano.grammar :refer [small medium np-grammar]]
            [babel.italiano.lexicon :refer [lexicon]]
            [babel.italiano.morphology :refer [fo new-style replace-patterns]]
            [babel.italiano.workbook :refer [analyze generate generate-all parse]]
            [babel.parse :as parse]
            #?(:clj [clojure.test :refer [deftest is]])
            #?(:cljs [cljs.test :refer-macros [deftest is]])
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [babel.logjs :as log]) 
            [dag_unify.core :refer [get-in strip-refs]]))

(deftest analyze-1
  (let [singular (analyze "compito" (:lookup medium))
        plural  (analyze "compiti" (:lookup medium))]
    (is (not (nil? singular)))
    (is (not (nil? plural)))))

(deftest analyze-2
  (let [singular (analyze "difficile" (:lookup medium))
        plural  (analyze "difficili" (:lookup medium))]
    (is (not (nil? singular)))
    (is (not (nil? plural)))))

(deftest present-irregular
  (let [result (first (take 1 (generate {:synsem {:subcat '()
                                                  :sem {:pred :be
                                                        :subj {:pred :I}
                                                        :tense :present}}}
                                        small)))]
    (is (= "io sono" (fo result)))))

(deftest passato-prossimo
  (let [result (first (take 1 (generate {:root {:italiano {:italiano "bere"}}
                                         :synsem {:subcat ()
                                                  :sem {:subj {:pred :I}
                                                        :tense :past
                                                        :aspect :perfect}}}
                                        small)))]
    (is (not (nil? result)))
    (is (= "io ho bevuto" (fo result)))))

(deftest passato-prossimo-reflexive
  (let [result (first (take 1 (generate {:head {:synsem {:agr {:gender :fem}}}
                                         :synsem {:subcat '()
                                                  :infl :present
                                                  :sem {:pred :get-up
                                                        :subj {:pred :I}
                                                        :tense :past
                                                        :aspect :perfect}}}
                                        small)))]
    (is (not (nil? result)))
    (is (= "io mi sono alzata" (fo result)))))

(deftest parse-io-parlo
  (let [result (parse "io parlo")]
    (is (not (empty? result)))
    (is (= "io parlo") (fo (first result)))))

        
(deftest round-trip-1
  (let [expr (first (take 1 (generate {:synsem {:subcat '()
                                                :sem {:spec {:def :def} 
                                                      :mod {:pred :difficile}
                                                      :number :sing
                                                      :pred :donna}}} 
                                      np-grammar)))]
    (is (= (fo expr) "la donna difficile"))
    (is (not (empty? (parse (fo expr) np-grammar))))))

(deftest forbid-mispelling
 (is (empty? (parse (fo "la donna difficila") np-grammar))))

;; useful for adding more round-trip tests.
(def foo
  (take 1 (repeatedly
           #(let [expr (generate {:synsem {:sem
                                           {:spec {:def :top}
                                            :mod {:pred :top}
                                            :number :top
                                            :pred :top}}}
                                 np-grammar)]
              (if (empty? (parse (fo expr) np-grammar))
                {:fo (fo expr)
                 :expr (get-in expr [:synsem :sem])
                 :sem (get-in (first (parse (fo expr) np-grammar))
                              [:synsem :sem])})))))
(deftest roundtrip-np-grammar
  (let [do-this-many 100]
    (is (empty?
         (filter #(not (nil? %))
                 (let [expressions
                       (generate-all {:synsem {:sem {:spec {:def :top}
                                                     :mod {:pred :top}
                                                     :number :top
                                                     :pred :top}}}
                                     np-grammar)]
                   (pmap (fn [expr] 
                           (if (empty? (parse (fo expr) np-grammar))
                             (do
                               (log/error (str "failed to parse: " (fo expr)))
                             {:fo (fo expr)
                              :expr (get-in expr [:synsem :sem])
                              :sem (get-in (first (parse (fo expr) np-grammar))
                                           [:synsem :sem])})
                             (log/info (str "parse OK:" (get-in expr [:rule]) " " (fo expr)))))
                         (if (= do-this-many :all)
                           expressions
                           (take do-this-many expressions)))))))))

(deftest roundtrip-small-grammar
  (let [do-this-many 200]
    (is (empty?
         (filter #(not (nil? %))
                 (let [expressions
                       (generate-all {:synsem {:cat :verb
                                               :sem {:tense :present}
                                               :subcat '()}}
                                     small)]
                   (pmap (fn [expr] 
                           (if (empty? (parse (fo expr) small))
                             (do
                               (log/error (str "failed to parse: " (fo expr)))
                             {:fo (fo expr)
                              :expr (get-in expr [:synsem :sem])
                              :sem (get-in (first (parse (fo expr) small))
                                           [:synsem :sem])})
                             (log/info (str "parse OK:" (fo expr)))))
                         (if (= do-this-many :all)
                           expressions
                           (take do-this-many expressions)))))))))

(ns babel.francais.writer
  (:refer-clojure :exclude [get-in]))

(require '[babel.cache :refer [create-index]])
(require '[babel.engine :refer [generate]])
(require '[babel.english.grammar :as en])
(require '[babel.forest :as forest])
(require '[babel.francais.grammar :refer [small medium]])
(require '[babel.francais.lexicon :refer [lexicon]])
(require '[babel.francais.morphology :refer [analyze exception-generator
                                             fo get-string phonize]])
(require '[babel.francais.pos :refer [intransitivize transitivize]])
(require '[babel.lexiconfn :refer (compile-lex map-function-on-map-vals unify)])
(require '[babel.parse :as parse])
(require '[babel.ug :refer [head-principle]])
(require '[babel.writer :refer [delete-from-expressions process write-lexicon]])
(require '[clojure.string :as string])
(require '[clojure.tools.logging :as log])
(require '[dag-unify.core :refer (fail? get-in strip-refs)])

(defn tout [ & [count]]
  (let [count (if count (Integer. count) 10)]

    ;; A place to erase old mistakes (before attempting again).
    ;; TODO: promote to the (process) command set.
    ;; 
    (if false
      (delete-from-expressions "fr" {:synsem {:sem {:aspect :perfect, :tense :past}}, :root {:français {:français "avoir"}}}))
    (if false
      (delete-from-expressions "fr" {:synsem {:sem {:aspect :perfect, :tense :past}}, :root {:français {:français "être"}}}))

    (if false
      (delete-from-expressions "fr" {:synsem {:sem {:aspect :perfect, :tense :past}}, :root {:français {:français "abandoner"}}}))

    (if false
      (delete-from-expressions "fr" {:synsem {:sem {:aspect :perfect, :tense :past}}}))
    (if true
      (delete-from-expressions "fr" {:synsem {:sem {:subj {:pred :io}}}}))

    (write-lexicon "fr" @lexicon)
    (let [
          ;; subset of the lexicon: only verbs which are infinitives and that can be roots:
          ;; (i.e. those that have a specific (non- :top) value for [:synsem :sem :pred])
          root-verbs 
          (zipmap
           (keys @lexicon)
           (map (fn [lexeme-set]
                  (filter (fn [lexeme]
                            (and
                             (= (get-in lexeme [:synsem :cat]) :verb)
                             (= (get-in lexeme [:synsem :infl]) :top)
                             (not (= :top (get-in lexeme [:synsem :sem :pred] :top)))))
                          lexeme-set))
                (vals @lexicon)))]
      (.size (map (fn [verb]
                    (let [root-form (get-in verb [:français :français])]
                      (log/info (str "generate with:" root-form))
                      (.size (map (fn [tense]
                                    (let [spec (unify {:root {:français {:français root-form}}}
                                                      tense)]
                                      (log/debug (str "generating from: " spec))
                                      (process [{:fill
                                                 {:spec spec
                                                  :source-model en/small
                                                  :target-model small}
                                                 :count count}] "fr")))
                                  [{:synsem {:sem {:tense :conditional}}}
                                   {:synsem {:sem {:tense :future}}}
                                   {:synsem {:sem {:tense :present}}}
                                   {:synsem {:sem {:tense :past
                                                   :aspect :perfect}}}]
                                  ))))
                  (reduce concat
                          (map (fn [key]
                                 (get root-verbs key))
                               (sort (keys root-verbs)))))))))

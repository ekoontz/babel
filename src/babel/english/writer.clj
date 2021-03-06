(ns babel.english.writer
  (:refer-clojure :exclude [get-in]))

(require '[babel.config :refer [language-to-root-spec]])
(require '[babel.directory :refer [models]])
(require '[babel.english.morphology :refer [fo]])
(require '[babel.korma :refer [init-db]])
(require '[babel.reader :refer [read-all read-one]])
(require '[babel.writer :as writer
           :refer [delete-from-expressions
                   fill-language-by-spec
                   process write-lexicon reload]])
(require '[clojure.repl :refer (doc)])
(require '[clojure.tools.logging :as log])
(require '[dag_unify.core :refer (fail? get-in strip-refs unify)])

;; false: throw error and stop further generation; true: tolerate errors: log/error and continue generation.
(def ^:const allow-generation-error true)

(def lexicon (-> models :en deref :lexicon))

(def fast-lexicon (try-to-read-lexemes))

(defn rewrite-lexicon []
  (write-lexicon "en" lexicon))

;; TODO: database becomes a bottleneck due to multiple concurrent queries that return a single row.
;; These queries should be combined into fewer queries where we iterate over the rows in that
;; this smaller set of queries.
(defn translate [source-language-short-name & [root no-older-than]]
  "Generate translations from source language (e.g. 'it' for Italian) into English.
   Optionally takes a root form of a verb in the source language."
  ;; Example usage: (translate \"es\" \"abrazar\"
  (log/info (str "calling (rewrite-lexicon)"))
  (init-db)
  (rewrite-lexicon)
  (log/info (str "finished calling (rewrite-lexicon)"))
  (let [spec :top
        ;; {:synsem {:sem {:pred :arrive}}}

        spec (if (and root (not (= root "all")))
               (unify spec
                      (language-to-root-spec source-language-short-name root))
               spec)

        debug (log/debug (str "using root:" root))
        debug (log/debug (str "using spec:" (strip-refs spec)))
        
;;        use-map-fn map ;; <- development
        use-map-fn pmap ;; <- production

        model (-> ((-> models :en)) deref)
        type-of-model (type model)
        ;; TODO: use :no-older-than here.
        source-expressions (read-all spec source-language-short-name)]
    (doall
     (use-map-fn
      ;; source-expression in the language that we want to translate into English.
      (fn [source-expression]
        (do (log/info
             (str "translating from:" source-language-short-name ": "
                  (:surface source-expression)))
            (log/debug (str source-language-short-name ": "
                            (get-in source-expression [:structure :synsem :sem])))
            (let [source-cat (get-in source-expression [:structure :synsem :cat] :top)
                  spec {:synsem {:cat source-cat
                                 :subcat (get-in source-expression [:structure :synsem :subcat])
                                 :sem
                                 (strip-refs (get-in
                                              (:structure source-expression)
                                              [:synsem :sem]))}}]
              (let [existing-english-expression (read-one spec "en" {:no-older-than (or no-older-than nil)})]
                (if existing-english-expression
                  ;; found existing expression: return that.
                  (do
                    (log/info (str (:surface source-expression)
                                   " -> " (:surface existing-english-expression)))
                    existing-english-expression)
                  ;; else, no existing expression: generate a new one.
                  (do
                    (log/debug (str "calling (process) with spec:" spec "; using source expression: '"
                                    (:surface source-expression) "'"))
                    (try
                      (let [result
                            (process [{:fill-one-language
                                       {:count 1
                                        :spec spec
                                        :model model
                                        }}]
                                     source-language-short-name)]
                        ;; TODO: 'result' is currently
                        ;; returning nil: should return something more indicative
                        ;; of what the (process) command did.
                        (log/debug (str "process result:" result)))
                      (catch Exception e
                        (do
                          (log/error (str "Could not translate source expression: "
                                          "'" (get source-expression :surface) "'"
                                          " from language: '" source-language-short-name 
                                          "' with predicate: '"
                                          (strip-refs (get-in source-expression
                                                              [:structure :synsem :sem :pred]))
                                          "' into English; subj:"
                                          "'" (get-in source-expression
                                                      [:structure :synsem :sem :subj :pred])
                                          "'; source semantics:'"
                                          (strip-refs (get-in source-expression
                                                              [:structure :synsem :sem]))
                                          "'" " ; exception: " e))
                          (cond
                            allow-generation-error
                            (log/info (str "ignoring above error and continuing."))
                            true
                            (throw e)))))))))))
      source-expressions))))

(defn write-one [spec]
  (log/debug (str "generating from spec: " spec))
  (let [model (-> ((-> models :en)) deref)]
    (try
      (process [{:fill-one-language
                 {:count 1
                  :spec spec
                  :model model}}]
               "en")
      (catch Exception e
        (cond
          
          ;; TODO: make this conditional on
          ;; there being a legitimate reason for the exception -
          ;; e.g. the verb is "works (nonhuman)" (which takes a non-human
          ;; subject), but we're trying to generate with
          ;; {:agr {:person :1st or :2nd}}, for which the only lexemes
          ;; are human.
          true
          
          (log/warn (str "ignoring exception: " e))
          false
          (throw e))))))

(defn all [ & [count]]
  (let [count (if count (Integer. count) 10)
        ;; subset of the lexicon: only verbs which are infinitives and that can be roots:
        ;; (i.e. those that have a specific (non- :top) value for [:synsem :sem :pred])
        root-verbs 
        (zipmap
         (keys lexicon)
         (map (fn [lexeme-set]
                (filter (fn [lexeme]
                          (and
                           ;; for development, restrict :pred to a single value.
                           (or true (= (get-in lexeme [:synsem :sem :pred]) :talk))
                           (= (get-in lexeme [:synsem :cat]) :verb)
                           (= (get-in lexeme [:synsem :infl]) :top)
                           (not (= :top (get-in lexeme [:synsem :sem :pred] :top)))))
                        lexeme-set))
              (vals lexicon)))]
    (init-db)
    (write-lexicon "en" lexicon)
    (log/info (str "done writing lexicon."))
    (log/info (str "generating with this many verbs: " (.size (reduce concat (vals root-verbs)))))
    ;; TODO: rewrite with threading macro (->>)
    (.size (pmap (fn [verb]
                   (let [root-form (get-in verb [:english :english])]
                     (log/debug (str "generating from root-form:" root-form))
                     (.size (map (fn [tense]
                                   (let [spec (unify {:root {:english {:english root-form}}}
                                                     tense)]
                                     (.size
                                      (map (fn [gender]
                                             (let [spec (unify spec
                                                               {:comp {:synsem {:agr gender}}})]
                                               (log/trace (str "generating from gender: " gender))
                                               (.size
                                                (map (fn [person]
                                                       (let [spec (unify spec
                                                                         {:comp {:synsem {:agr {:person person}}}})]
                                                         (log/trace (str "generating from person: " person))
                                                         (.size
                                                          (map (fn [number]
                                                                 (let [spec (unify spec
                                                                                   {:comp {:synsem {:agr {:number number}}}})]
                                                                   (write-one spec)))
                                                               [:sing :plur]))))
                                                     [:1st :2nd :3rd]))))
                                           (cond (= tense ;; TODO: what is this conditional accomplishing?
                                                    ;; look at `git blame` to try to understand.
                                                    {:synsem {:sem {:aspect :simple
                                                                    :tense :past}}})
                                                 [{:gender :masc}
                                                  {:gender :fem}]
                                                 true
                                                 [:top])))))
                                 (list {:synsem {:sem {:tense :conditional}}}
                                       {:synsem {:sem {:tense :future}}}

                                       {:synsem {:sem {:tense :present
                                                       :aspect :simple}}}

                                       {:synsem {:sem {:tense :present
                                                       :aspect :progressive}}}

                                       {:synsem {:sem {:aspect :progressive
                                                       :tense :past}}}

                                       {:synsem {:sem {:aspect :perfect
                                                      :tense :present}}})))))
                 (reduce concat
                         (map (fn [key]
                                (get root-verbs key))
                              (sort (keys root-verbs))))))))


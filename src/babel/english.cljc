(ns babel.english
  (:require
   [babel.directory :as directory]
   [babel.english.grammar :as grammar]
   [babel.english.lexicon :as lex]
   [babel.english.morphology :as morph]
   [babel.parse :as parse]
   [babel.test.test :refer [init-db]]
   [clojure.string :as string]))

(def model
  (do
    (init-db)
    @@(get babel.directory/models :en)))

;; can't decide between 'morph' or 'fo' or something other better name.
(defn morph [expr & {:keys [from-language model show-notes]
                     :or {from-language nil
                          model model
                          show-notes true}}]
  (morph/fo expr
            :from-language from-language :show-notes show-notes
            :lexicon (:lexicon model)))

(defn preprocess [input]
  "arbitrary regexp replacements to convert English orthography into a parsable whitespace-delimited expression"
  ;; e.g.
  ;; (preprocess "the womens' hats and the cats' pyjamas")
  ;;  => "the women 's  hats and the cats 's pyjamas"
  ;;
  (string/replace (string/replace input #"(men)s'(\s*)" "$1 's $2") #"(\S)s'" "$1s 's"))

(defn parse
  "parse a string in English into zero or more (hopefully more) phrase structure trees"
  ([input model truncate?]
   (parse/parse (preprocess input) model :parse-with-truncate truncate?)))

(defn morph-ps
  ([expr]
   (morph-ps expr model))

  ([expr model & {:keys [from-language show-notes]
                  :or {from-language nil
                       show-notes true}}]
   ;; modeled after babel.english/morph:
   ;; most arguments are simply discarded for italian.
   (parse/fo-ps expr (:morph model))))



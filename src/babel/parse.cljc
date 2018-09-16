(ns babel.parse
 (:refer-clojure :exclude [get-in resolve find])
 (:require
  [babel.over :as over :refer [truncate]]
  [clojure.core.cache :as cache]
  [clojure.set :refer [union]]
  [clojure.string :as string]
  #?(:clj [clojure.tools.logging :as log])
  #?(:cljs [babel.logjs :as log])
  [dag_unify.core :refer (get-in strip-refs)]))

(def ^:const parse-with-lexical-caching true)

;; for now, using a language-independent tokenizer.
(def tokenizer #"[ ']")
(def map-fn #?(:clj pmap) #?(:cljs map))

;; thanks to http://tobiasbayer.com/post/2014-11-12-using-clojures-core-dot-cache/
(defn deal-with-cache [k if-miss-do lexical-cache]
  (if (cache/has? @lexical-cache k)
    (do
      (log/trace (str "cache hit for " k))
      (swap! lexical-cache #(cache/hit % k)))
    (swap! lexical-cache #(cache/miss % k (if-miss-do k)))))

(defn lookup [{lookup :lookup
               lexical-cache :lexical-cache
               default-fn :default-fn} k]
  (log/debug (str "lookup: k=" k))
  (let [pre-default
        (if (or (= false parse-with-lexical-caching)
                (nil? lexical-cache))
          (lookup k)
          (let [lookup-fn (fn [k]
                            (do (log/debug (str "cache miss for " k))
                                (let [looked-up (lookup k)]
                                  (if (not (empty? looked-up)) looked-up :none))))]
            (deal-with-cache k lookup-fn lexical-cache)
            (let [cache-value (get @lexical-cache k)]
              (if (= cache-value :none) nil
                  cache-value))))]
    (log/trace (str "default-fn: nil? " (nil? default-fn)))
    (log/trace (str "pre-default: count: " (count pre-default)))
    (cond (nil? default-fn)
          pre-default
          true
          (mapcat default-fn pre-default))))

;; TODO: use dag_unify/assoc-in rather than over/over, so that we can remove babel.over.
(defn over [grammar left right morph default-fn]
  "opportunity for additional logging before calling the real (over)"
  (if default-fn
    (->>
     (over/over grammar left right)
     (mapcat default-fn))
    (over/over grammar left right)))

(defn square-cross-product [x]
  (mapcat (fn [each-x]
            (filter #(not (nil? %))
                    (map-fn (fn [each-y]
                              (if (= (second each-x) (first each-y))
                                [each-x each-y]))
                            x)))
          x))

(defn span-map [max]
  "return a map of size -> _spans_, where _size_ is an integer, and _spans_ are all
   the [left,right] pairs whose combined size is equal to _size_., and _size_ ranges from 1 to _max_."
  ;; for example:
  ;; (span-map 5) =>
  ;; 
  ;; {1 ([0 1]         [1 2] ...) ;; all of these are of size 1
  ;;  2 ([[0 1] [1 2]] [[1 2] [2 3]] ... ) ;; each pair has a combined size of 2
  ;;  3 ([[0 2] [2 3]] [[1 2] [2 3]] ... ) ;; each pair has a combined size of 3
  ;;  4 ([[0 3] [3 4]] [[1 2] [2 4]] ..
  ;;  5 ([[0 4] [4 5]] [[1 3] [3 5]] ... ) } ;; each pair has a combined size of 5.
  (cond
    (< max 2)
    nil

    true
    (let [
          ;; TODO: rather than this function, make a static lookup table, at least for n < (e.g.) 5.
          ;; e.g. (spanpairs 5) =>
          ;; ([0 1] [0 2].. [0 5] [1 2] ..[1 4] [1 5] .... [4 5])
          spanpairs (fn [n]
                      (mapcat (fn [x]
                                (map-fn (fn [y]
                                          [x y])
                                        (range (+ x 1) (+ n 1))))
                              (range 0 n)))
          spans (square-cross-product (spanpairs max))]
      (merge
       {1 (map-fn
           (fn [i]
             [i (+ 1 i)])
           (range 0 max))}
       (reduce (fn [resultant-map this-submap]
                 (merge-with union ;; TODO: this could get expensive - consider alternatives.
                             resultant-map this-submap))
               (map-fn (fn [span-pair]
                         (let [left-span (first span-pair)
                               left-boundary (first left-span)
                               right-span (second span-pair)
                               right-boundary (second right-span)]
                           {(- right-boundary left-boundary)
                            (list span-pair)}))
                       spans))))))
(declare leaves)

(defn summarize [sign model]
  "useful for providing an abbreviated form of a sign's comps and heads. called as part of truncation to improve parsing performance by avoiding copying unnecessary material."
  (let [language-keyword (:language-keyword model)
        root-form (or
                   (get-in sign [language-keyword :infinitive])
                   (get-in sign [language-keyword :root])
                   (get-in sign [language-keyword language-keyword])
                   (if (string? (get-in sign [language-keyword]))
                     (get-in sign [language-keyword])))
        stringified ((:morph model) sign)]
    (conj (if (and root-form
                   (= :none (get-in sign [:comp] :none))
                   (= :none (get-in sign [:head] :none))
                   (not (= root-form stringified)))
            {language-keyword {language-keyword stringified
                               :root root-form}}
            {language-keyword stringified})
          (if-let [rule (get-in sign [:rule])]
            {:rule rule})
          (if-let [cat (or (get-in sign [:cat])
                           (get-in sign [:synsem :cat]))]
            {:cat cat})
          (if-let [head (get-in sign [:head])]
            {:head (summarize head model)})
          (if-let [comp (get-in sign [:comp])]
            {:comp (summarize comp model)}))))
  
(defn parses [input n model span-map parse-with-truncate]
  (cond
    (= n 1) input
    (> n 1)
    (let [minus-1 (parses input (- n 1) model span-map parse-with-truncate)]
      (log/debug (str "parses: input=" input))
      (merge minus-1
             (reduce (fn [x y]
                       (merge-with concat x y))
                     (map-fn (fn [span-pair]
                               
                               ;; create a new key/value pair: [i,j] => parses,
                               ;; where each parse in parses matches the tokens from [i,j] in the input.
                               {

                                ;; <key: [i,j]>
                                [(first (first span-pair))
                                 (second (second span-pair))]
                                ;; </key>

                                ;; <value: parses for strings indexed at [i,j]>
                                (let [left (get minus-1 (first span-pair))
                                      right (get minus-1 (second span-pair))
                                      left-strings (filter string? left)
                                      right-strings (filter string? right)
                                      left-lexemes (mapcat (fn [string]
                                                             (lookup model string))
                                                           left-strings)
                                      right-lexemes (mapcat (fn [string]
                                                              (lookup model string))
                                                            right-strings)
                                      left-signs (lazy-cat left-lexemes (filter map? left))
                                      right-signs (lazy-cat right-lexemes (filter map? right))
                                      ]
                                  (lazy-cat
                                   (if (and (not (empty? left-signs))
                                            (not (empty? right-signs)))
                                     (let [morph-ps (if (:morph-ps model)
                                                      (:morph-ps model)
                                                      (:morph model))
                                           ;; fallback to (:morph model) if
                                           ;; (:morph-ps is not available)
                                           trees (over (:grammar model) left-signs right-signs morph-ps
                                                       (:default-fn model))
                                           truncated-trees
                                           (if parse-with-truncate
                                             (map-fn (fn [parent]
                                                       (conj
                                                        {:head (summarize (get-in parent [:head]) model)
                                                         :comp (summarize (get-in parent [:comp]) model)}
                                                        (truncate parent [[:comp]
                                                                          [:head]]
                                                                  model)))
                                                     trees))]
                                       (if (not (empty? trees))
                                         (log/trace (str "parse/parses: trees: "
                                                         (string/join ","
                                                                      (map #(morph-ps %) trees))))
                                         (log/trace (str "no trees found for left/right:")))
                                       (log/trace (str " left: "
                                                       (string/join ","
                                                                    (map #(morph-ps %) left-signs))))
                                       (log/trace (str " right: "
                                                       (string/join ","
                                                                    (map #(morph-ps %) right-signs))))
                                       (if (and false parse-with-truncate) truncated-trees trees)))
                                   ;; TODO: explain why we can use (first) here for the left- and right-strings.
                                   ;; Throw an exception if (> 1 (count left-strings)) or (> 1 (count right-strings))
                                   [(string/join " " [(first left-strings) (first right-strings)])]))
                                ;; </value>
                                })
                             (get span-map n)))))))

(declare parse)

(defn parse-from-file [reader]
  "parse linefeed-separated file line-by-line."
  (map parse
       (line-seq reader)))

(defn parse
  "Return a list of all possible parse trees for a string or a sequence of tokens.
   If the input is a string, then use a language-independent tokenizer
  to turn the string into a sequence of tokens.  In the latter case,
  the tokens are assumed to be produced by splitting a string in some
  language-dependent way. If parse-with-truncate is false, then at
  each step of parsing the input, the entire tree will be preserved
  rather than just the immediate parents of the root node. Preserving
  the entire tree costs about 20% more time than truncating it."
  ([input model & {:keys [parse-with-truncate original-input]}]
   (let [parse-with-truncate
         (cond (= parse-with-truncate false)
               false
               true true)
         original-input (if original-input original-input input)
         model (if (future? model) @model model)]
     (cond (= (type input) java.io.BufferedReader)
           (parse-from-file input model)
           (string? input)
           (do
             (log/debug (str "parsing input: '" input "'"))
             ;; tokenize input (more than one tokenization is possible), and parse each tokenization.
             (let [tokenizations (filter #(not (empty? %)) (string/split input tokenizer))
                   result (parse tokenizations model
                                 :parse-with-truncate parse-with-truncate
                                 :original-input original-input)]
               (if (empty? result)
                 (log/warn (str "could not parse: \"" input "\". Tokenizations attempted: "
                                (string/join ";" tokenizations)))
                 (log/info (str "parsed input:    \"" input "\"")))
               result))
         
           (or (seq? input) (vector? input))
           ;; assume input is a list of tokens.
           (let [tokens input
                 token-count (count tokens)
                 token-count-range (range 0 token-count)
                 input-map (zipmap (map (fn [i] [i (+ i 1)])
                                        token-count-range)
                                   (map (fn [i] [(nth tokens i)])
                                        token-count-range))]
             (log/debug (str "input map:" input-map))
             (let [all-parses
                   (parses input-map token-count model
                           (span-map token-count) parse-with-truncate)
                   result
                   {:token-count token-count
                    :complete-parses
                    (filter map? (get all-parses
                                      [0 token-count]))
                    :all-parses all-parses}]
               (:complete-parses result)))

           true
           (throw (Exception. "Don't know how to parse input of type: " (type input)))))))
  
(defn leaves [parse-tree]
  "return terminal nodes (leaves) for this tree."
  (let [head (get-in parse-tree [:head] :none)
        comp (get-in parse-tree [:comp] :none)]
    (cond
      (and (= :none head)
           (= :none comp))
      [parse-tree]

      (= :none head)
      (leaves comp)

      (= :none comp)
      (leaves head)
      
      true
      (concat
       (leaves head)
       (leaves comp)))))

;; TODO: make a fo-ps, but instead of
;; returning a string, it returns a map,
;; e.g., instead of returning:
;;   [noun-phrase C:'the' H:[nbar-s-obj H:[nbar C:'tall' H:'man'] C:[s/obj C:'you' H:'see']]]
;; instead, return:
;;
;; {:rule "noun-phrase"
;;  :c "the"
;;  :h {:rule "nbar-s-obj"
;;      :h {:rule "nbar"
;;          :c "tall"
;;         :h "man"))
;;     :c {:rule "s/obj"
;;          :c "you"
;;         :h "see")))))
;;
;; 
;; in other words, transform input tree into a much
;; smaller map.

(defn fo-ps [tree morph]
  "return a human-readable string of a phrase structure tree. leaves of the tree are printed
   by calling the supplied _morph_ function"
  (let [head-first? (get-in tree [:first] :none)
        cat (str (get-in tree [:synsem :cat]) "")]
    (if (= true (get-in tree [:phrasal]))
      (do (log/trace (str "fo-ps: rule at top of tree: " (get-in tree [:rule])))
          (log/trace (str "fo-ps: " (strip-refs tree))))
      (log/trace (str "fo-ps: leaf: " (strip-refs tree))))
    (cond
      (= :fail tree) :fail

      (and (= head-first? :none)
           (= false (get-in tree [:phrasal])))
      (str "" (morph tree) "")

      (= head-first? :comp)
      (str "[" (get-in tree [:rule]) " "
           "C:" (fo-ps (get-in tree [:comp]) morph) " "
           "H:" (fo-ps (get-in tree [:head]) morph)
           "]")

      (= head-first? :head)
      (str "[" (get-in tree [:rule]) " "
           "H:" (fo-ps (get-in tree [:head]) morph) " "
           "C:" (fo-ps (get-in tree [:comp]) morph)
           "]")

      (nil? tree)
      (str "(nil)")

      true "_"

      ;; other possibilities for fallthrough besides "_".
      true (strip-refs tree)
      
      true (str "?? " (if (nil? tree) "(nil)" (type tree))))))

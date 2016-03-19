(ns babel.parse
 (:refer-clojure :exclude [get-in resolve find])
 (:require
  [babel.over :as over]
  [clojure.string :as string]
  #?(:clj [clojure.tools.logging :as log])
  #?(:cljs [babel.logjs :as log])
  [dag_unify.core :refer (get-in strip-refs)]))

;; for now, using a language-independent tokenizer.
(def tokenizer #"[ ']")
(declare over)
(declare toks2)

(defn toks [s lookup]
  (let [tokens (string/split s tokenizer)
        tokens2 (toks2 tokens (count tokens))]
    (pmap (fn [token-vector]
            (pmap lookup token-vector))
         tokens2)))

(defn toks2 [tokens n]
  "group tokens together into every possible sequence of n or less tokens."
  (cond
    (seq? tokens)
    (toks2 (vec tokens) n)
    (< n 1) nil
    (= n 1) (vec (list tokens))
    (> n (count tokens)) (toks2 tokens (count tokens))

    (= (count tokens) n) ;; ([a b]; n = 2) => [["a b"] ["a" "b"]]
    (cons [(string/join " " tokens)]
          (toks2 tokens (- n 1)))
    
    (> (count tokens) n) ;; ([a b c]; n = 2) => [["a b" "c"] ["a" "b c"]]
    (concat (let [first-token (string/join " " (subvec tokens 0 n))]
              (pmap #(vec (cons first-token %))
                    (toks2 (subvec tokens n (count tokens))
                           n)))
            (let [last-token (string/join " " (subvec tokens (- (count tokens) n) (count tokens)))]
              (pmap #(vec (concat % (list last-token)))
                    (toks2 (subvec tokens 0 (- (count tokens) n))
                           n)))
            (toks2 tokens (- n 1)))
    true
    tokens))

(defn over [grammar left right]
  "opportunity for additional logging before calling the real (over)"
  (log/trace (str "parse/over: grammar size: " (count grammar)))
  (over/over grammar left right))

(defn create-ngram-map [args left ngrams grammar morph split-at x]
  (log/debug (str "create-ngram-map: left:" left ";split-at:" split-at))
  (log/trace (str "create-ngram-map: left:" left ";split-at:" split-at "; size:" (count args) "; x:" x))

  (log/debug
   (str "create-ngram-map: args: "
        (string/join ";"
                     (map (fn [arg]
                            (string/join ","
                                         (map (fn [tree-node]
                                                (cond (and (map? tree-node)
                                                           (:rule tree-node))
                                                      (str "[" (:rule tree-node) ":"
                                                           (morph tree-node)
                                                           "]")
                                                      true
                                                      (str "'" (morph tree-node) "'")))
                                              arg)))
                          args))))

  (if (< (+ left (- split-at 2))
         (/ (count args) 2))
    (lazy-cat
     (let [left-parses (get ngrams [left (+ left (- split-at 0))] '())
           right-parses (get ngrams [(+ left split-at 0) (- (count args) 0)] '())]
       (if (and (not (empty? left-parses))
                (not (empty? right-parses)))
         (do
           (log/debug (str "left: " (if (not (nil? left-parses))
                                       (string/join ";"
                                                    (if (not (nil? left-parses))
                                                      (map (fn [parses]
                                                             (str "'" (morph parses) "'"))
                                                           (filter #(not (empty? %))
                                                                   left-parses)))))))
           (log/debug (str "right: " (if (not (nil? right-parses))
                                       (string/join ";"
                                                    (if (not (nil? right-parses))
                                                      (map (fn [parses]
                                                             (str "'" (morph parses) "'"))
                                                           (filter #(not (empty? %))
                                                                   right-parses)))))))
           (let [result (over grammar left-parses right-parses)]
             (if (not (empty? result))
               (log/debug (str "create-ngram-map: "
                               (string/join ";"
                                            (map (fn [res]
                                                   (str "[" (:rule res) " -> "
                                                        (morph res)
                                                        "]"))
                                                 result)))))
             result))))
     (create-ngram-map args left ngrams grammar morph (+ 1 split-at) x))))

(defn create-xgram-map [args from to grammar morph]
  (log/debug
   (str "create-xgram-map: from: " from ";to: " to ";args: "
        args))
  (cond (= to 0) {}

        ;; create a vector of: [ {[0 1] tok0}, {[1 2] tok1}, .. ]
        (= to 1)
        (reduce merge
                (map (fn [from]
                       {[from (+ 1 from)]
                        (subvec args from (+ 1 from))})
                     (range 0 (count args))))


        (< (+ to from) (+ 1 (count args)))
        (let [debug
              (log/debug (str "COND 3: from=" from "; to=" to ";count(args)=" (count args)))]
          (merge 
           ;; 1. the span from:..
           {[from (+ to from)]
            (create-ngram-map args from
                              (create-xgram-map args 0 (- to 1) grammar morph)
                              grammar morph 1 to)}
           (create-xgram-map args (+ from 1) to grammar morph)))

        true
        (let [debug (str "COND 4:  to=" to "; from=" from "; count(args)=" (count args))]
          (create-xgram-map args 0 (- to 1) grammar morph))))

;; TODO: move tokenization to within lexicon.
(defn parse [arg lookup grammar]
  "return a list of all possible parse trees for a string or a list of lists of maps (a result of looking up in a dictionary a list of tokens from the input string)"
  (cond (string? arg)
        (let [tokens (toks arg lookup)]
          (parse tokens lookup grammar))
        (and (vector? arg)
             (empty? (rest arg)))
        (first arg)

        (vector? arg)
        ;; returns the parse of the whole expression (from [0..l] where l=length(arg).
        ;; TODO: if a parse for the whole expression is not found,
        ;; return the largest subparse(s).
        (let [grammar-input grammar
              grammar (cond (map? grammar-input)
                            (:grammar grammar-input)
                            true
                            grammar-input)
              morph (cond (map? grammar-input)
                          (:morph grammar-input)
                          true
                          (fn [x] (str (type grammar-input) "(morph goes here)")))]
          (get (create-xgram-map arg 0 (count arg) grammar morph)
               [0 (count arg)]))

        (seq? arg)
        (mapcat #(parse (vec %) lookup grammar)
                arg)
        
        ;; TODO: throw exception here.
        true
        (str "unexpected input: type: " (type arg))))

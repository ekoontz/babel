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
(declare toks3)

(defn segmentations [tokens]
  (vec (set (toks3 tokens))))

(defn toks [s lookup morph]
  (let [tokens (string/split s tokenizer)
        ;; TODO: workaround for the fact that toks3 generates duplicate segmentations
        tokens2 (segmentations tokens)]
    (pmap (fn [token-vector]
            (let [segmentations (pmap lookup token-vector)
                  filtered-segmentations
                  (filter

                   (fn [segmentation]
                     (let [empty-count
                           (count (filter #(= true %)
                                          (map (fn [segmentation]
                                                 (empty? segmentation))
                                               segmentations)))]
                       (= 0 empty-count)))
                   segmentations)]
                  
              (log/trace (str "segmentations (pre-empty?-filtering) found:"
                              (string/join ";"
                                           (map (fn [segmentation]
                                                  (string/join ","
                                                               (map
                                                                (fn [segment]
                                                                  (morph segment))
                                                                segmentation)))
                                                segmentations))))
              (if (not (empty? filtered-segmentations))
                (log/debug (str "segmentations found:"
                                (string/join ";"
                                             (map (fn [segmentation]
                                                    (string/join ","
                                                                 (map
                                                                  (fn [segment]
                                                                    (morph segment))
                                                                  segmentation)))
                                                  filtered-segmentations)))))


              filtered-segmentations))
         tokens2)))

(defn toks3 [tokens]
  "group tokens together into every possible grouping"
  (cond
    (empty? tokens) tokens
    (= (count tokens) 1)
    []

    (= (count tokens) 2)
    [[(string/join " " tokens)]
     tokens]

    true
    (concat
     (map (fn [each]
            (vec (cons (first tokens)
                       each)))
          (toks3 (subvec tokens 1 (count tokens))))
     (map (fn [each]
            (vec (concat each (list (last tokens)))))
          (toks3 (subvec tokens 0 (- (count tokens) 1)))))))

(defn over [grammar left right]
  "opportunity for additional logging before calling the real (over)"
  (log/trace (str "parse/over: grammar size: " (count grammar)))
  (over/over grammar left right))

(defn create-trees [args left ngrams grammar morph split-at]
  (log/debug (str "create-trees: left:" left ";split-at:" split-at))
  (if (< (+ left (- split-at 2))
         (/ (count args) 2))
    (lazy-cat
     (let [left-trees (get ngrams [left (+ left (- split-at 0))] '())
           right-trees (get ngrams [(+ left split-at 0) (- (count args) 0)] '())]
       (if (and (not (empty? left-trees))
                (not (empty? right-trees)))
         (let [result (over grammar left-trees right-trees)]
             result)))
     (if (> (count args) (+ 1 split-at))
       (create-trees args left ngrams grammar morph (+ 1 split-at))))))

(defn create-tree-map [args from extent grammar morph]
  (cond (= extent 0) {}
        (= extent 1)
        (reduce merge
                (map (fn [from]
                       {[from (+ 1 from)]
                        (subvec args from (+ 1 from))})
                     (range 0 (count args))))
        (< (+ from extent) (+ (count args) 1))
        (merge 
         {[from (+ extent from)]
          (create-trees args from
                        (create-tree-map args 0 (- extent 1) grammar morph)
                        grammar morph 1)}
         (create-tree-map args (+ from 1) extent grammar morph))

        true
        (create-tree-map args 0 (- extent 1) grammar morph)))

;; TODO: move tokenization to within lexicon.
(defn parse [arg lookup grammar]
  "return a list of all possible parse trees for a string or a list of lists of maps (a result of looking up in a dictionary a list of tokens from the input string)"
  (cond (string? arg)
        (let [grammar-input grammar
              grammar (cond (map? grammar-input)
                            (:grammar grammar-input)
                            true
                            grammar-input)
              morph (cond (map? grammar-input)
                          (:morph grammar-input)
                          true
                          (fn [x] (str (type grammar-input) "(morph goes here)")))
              tokens (toks arg lookup morph)]
          (parse tokens lookup grammar-input))
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
          (get (create-tree-map arg 0 (count arg) grammar morph)
               [0 (count arg)]))

        (seq? arg)
        (mapcat #(parse (vec %) lookup grammar)
                arg)
        
        ;; TODO: throw exception here.
        true
        (str "unexpected input: type: " (type arg))))

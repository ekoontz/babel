(ns babel.parse
 (:refer-clojure :exclude [get-in resolve find])
 (:require
  [babel.over :as over]
  [clojure.set :refer [union]]
  [clojure.string :as string]
  #?(:clj [clojure.tools.logging :as log])
  #?(:cljs [babel.logjs :as log])
  [dag_unify.core :refer (get-in strip-refs)]))

;; for now, using a language-independent tokenizer.
(def tokenizer #"[ ']")
(declare over)
(declare toks3)

;; A segmentation is a vector of segments. Each
;; segment is a lexeme that consists of 0 or more tokens.
(defn segmentations [tokens]
  (vec (set (toks3 tokens))))

(defn toks [s lookup morph]
  (let [tokens (string/split s tokenizer)
        ;; TODO: workaround for the fact that toks3 generates duplicate segmentations
        tokens2 (segmentations tokens)]

    (pmap (fn [token-vector]
            (let [segmentations (pmap lookup token-vector)

                  ;; we filter out any segmentation that has no matches
                  ;; for a given segment in its segmentation. For example, in
                  ;; "la sua ragazza", there are two possible segmentations:
                  ;; 1. ["la sua"] ["ragazza"]
                  ;; 2. ["la"] [] ["ragazza"]
                  ;;
                  ;; In segmentation 2., there is no lexeme to be found when we look up "sua",
                  ;; so the whole segmentation is removed, and we return a list with only 
                  ;; one member - segmentation 1.
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
                (log/debug (str "segmentation found:"
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

(defn tree-map-entries [args from extent]
  (if (< (+ from extent)
         (+ extent (count args)))
    (merge
     {[from (+ extent from)]
      (subvec args from (+ extent from))}
     (tree-map-entries args (+ extent from) extent))
    {}))

(defn create-trees [left right parse-map grammar morph split-at]
  (lazy-cat
   (let [left-trees (get parse-map [left (+ left split-at)])
         right-trees (get parse-map [(+ left split-at) right])]
     (if (and (not (empty? left-trees))
              (not (empty? right-trees)))
       (let [result (over grammar left-trees right-trees)]
         result)))
   (if (> right (+ 1 split-at))
     (create-trees left right parse-map grammar morph (+ 1 split-at)))))

(defn terminal-spans [segmentations]
  "Generate a map of pair:[i,i+1] => tokens; used as an input to n-spans."
  (reduce merge
          (map (fn [tokens]
                 (map (fn [from]
                        {[from (+ 1 from)]
                         (subvec (vec tokens) from (+ 1 from))})
                    (range 0 (count tokens))))
               segmentations)))

(defn cross-product [x y]
  (mapcat (fn [each-x]
            (filter #(not (nil? %))
                    (map (fn [each-y]
                           (if (= (second each-x) (first each-y))
                             [each-x each-y]))
                         y)))
          x))

(defn spanpairs [n]
  (mapcat (fn [x]
            (map (fn [y]
                   [x y])
                 (range (+ x 1) (+ n 1))))
          (range 0 n)))

(defn square [x]
  (let [pairs (spanpairs x)]
    (cross-product pairs pairs)))

(defn span-map [n]
  "take a 'square span array' and reorganizes it into a map of size -> _spans_,
   where _size_ is an integer, and _spans_ are all the [left,right] pairs whose combined
   size is equal to _size_."
  (let [spans
        (square n)]
    (reduce (fn [resultant-map this-submap]
              (merge-with union
                          resultant-map this-submap))
            (map (fn [span-pair]
                   (let [left-span (first span-pair)
                         left-boundary (first left-span)
                         right-span (second span-pair)
                         right-boundary (second right-span)]
                     {(- right-boundary left-boundary)
                      (list span-pair)}))
                 spans))))
(def span-maps
  {2 (get (span-map 2) 2)
   3 (get (span-map 3) 3)
   4 (get (span-map 4) 4)
   5 (get (span-map 5) 5)
   6 (get (span-map 6) 6)
   7 (get (span-map 7) 7)})

(defn create-tree-map [args from extent grammar morph]
  (log/debug (str "create-tree-map (#args=" (count args)
                  ",from=" from ",extent=" extent ") = "))
  (log/debug (str "condition holding: "
                  (cond (= extent 0) "1st"
                        (= extent 1) "2nd"
                        (< (+ from extent)
                           (+ (count args) 1)) "3rd"
                        true "4th")))
  (cond (= extent 0) {}

        (= extent 1) (tree-map-entries args from extent)

        (<= (+ from extent) (count args))
        (merge
         (let [trees (create-trees from (- (count args) 1)
                                   (create-tree-map args 0 (- extent 1) grammar morph)
                                   grammar morph 1)]
           (if (not (empty? trees))
             {[from (+ extent from)] trees}))
         (create-tree-map args (+ from 1) extent grammar morph))

        true
        (create-tree-map args 0 (- extent 1) grammar morph)))

;; TODO: move tokenization to within lexicon.
(defn parse [input lookup grammar]
  "return a list of all possible parse trees for a string or a list of lists of maps
   (a result of looking up in a dictionary a list of tokens from the input string)"
  (cond (string? input)
        (let [grammar-input grammar
              grammar (cond (map? grammar-input)
                            (:grammar grammar-input)
                            true
                            grammar-input)
              morph (cond (map? grammar-input)
                          (:morph grammar-input)
                          true
                          (fn [x] (str (type grammar-input) "(morph goes here)")))
              tokens (filter #(not (empty? %)) (toks input lookup morph))]
          (parse tokens lookup grammar-input))
        (and (vector? input)
             (empty? (rest input)))
        (first input)

        (vector? input)
        ;; returns the parse of the whole expression (from [0..l] where l=length(input).
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
          (get (create-tree-map input 0 (count input) grammar morph)
               [0 (count input)]))

        (seq? input)
        (mapcat #(parse (vec %) lookup grammar)
                input)
        
        ;; TODO: throw exception here.
        true
        (str "unexpected input: type: " (type input))))

(defn lookup-tokens [input-string grammar]
  (let [morph (:morph grammar)
        lookup (:lookup grammar)
        grammar (:grammar grammar)
        tokens (toks input-string lookup morph)]

    (filter #(not (empty? %))
            (toks input-string lookup morph))))

(defn get-parse-map [input grammar]
  (let [morph (:morph grammar)
        lookup (:lookup grammar)
        grammar (:grammar grammar)
        input (lookup-tokens input grammar)]
    (create-tree-map input 0 (count input) grammar morph)))

(defn parses [spans2subtrees i n]
  (if (<= i n)
    (let [i-spans (get span-maps i)]
      (parses
       (merge
        (let []
          {i 42})
        spans2subtrees)
       (+ 1 i) n))))

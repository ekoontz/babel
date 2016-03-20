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

(defn create-trees [args left parse-map grammar morph split-at]
  (lazy-cat
   (let [left-trees (get parse-map [left (+ left split-at)])
         right-trees (get parse-map [(+ left split-at) (count args)])]
     (if (and (not (empty? left-trees))
              (not (empty? right-trees)))
       (let [result (over grammar left-trees right-trees)]
         result)))
   (if (> (count args) (+ 1 split-at))
     (create-trees args left parse-map grammar morph (+ 1 split-at)))))

(defn create-tree-map [args from extent grammar morph]
  (log/debug (str "create-tree-map (#args=" (count args)
                  ",from=" from ",extent=" extent ") = "))
;  (if (and (< (+ from extent 0) (count args)))
;    (log/debug (str " .. = " (morph (subvec args from (+ 1 extent))))))
  (cond (= extent 0) {}

        (= extent 1) (tree-map-entries args from extent)

        (< (+ from extent) (+ (count args) 1))
        (merge 
         {[from (+ extent from)]
          (create-trees args from
                        (create-tree-map args 0 (- extent 1) grammar morph)
                        grammar morph 1)}
         (create-tree-map args (+ from 1) extent grammar morph))

        true
        (create-tree-map args 0 (- extent 1) grammar morph)))
;        (merge
;         {[from (- extent 1)]
;          (create-trees args from 
;                        (create-tree-map args 0 (- extent 1) grammar morph)
;                        grammar morph 1)}
;         (create-tree-map args (+ from 1) (- extent 1) grammar morph))))

;; TODO: move tokenization to within lexicon.
(defn parse [input lookup grammar]
  "return a list of all possible parse trees for a string or a list of lists of maps (a result of looking up in a dictionary a list of tokens from the input string)"
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

(defn get-parse-map [input grammar]
  (let [morph (:morph grammar)
        lookup (:lookup grammar)
        grammar (:grammar grammar)
        input (vec (first (filter #(not (empty? %))
                                  (toks input lookup morph))))]
    (let [pm (create-tree-map input 0 (count input) grammar morph)]
      (zipmap (filter #(not (empty? (get pm %)))
                      (keys pm))
              (filter #(not (empty? %))
                      (vals pm))))))


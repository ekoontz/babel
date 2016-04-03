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
                                (string/join " ; "
                                             (map (fn [segmentation]
                                                    (string/join ","
                                                                 (set (map
                                                                       (fn [segment]
                                                                         (morph segment))
                                                                       segmentation))))
                                                  filtered-segmentations)))))


              filtered-segmentations))
         tokens2)))

(defn toks3 [tokens]
  "group tokens together into every possible grouping"
  (cond
    (empty? tokens) tokens

    (= (count tokens) 1) []

    (= (count tokens) 2) [[(string/join " " tokens)]
                          tokens]

;    (= (count tokens) 3) [[(string/join " " tokens)]
;                          tokens]

    true
    (concat
     (map (fn [each]
            (vec (cons (first tokens)
                       each)))
          (toks3 (subvec tokens 1 (count tokens))))
     (map (fn [each]
            (vec (concat each (list (last tokens)))))
          (toks3 (subvec tokens 0 (- (count tokens) 1)))))))

(defn sliding-tokenizer-n [tokens n i]
  (cond (= 0 (count tokens)) nil
        (>= (count tokens) n)
        (cons [i n (string/join " " (subvec tokens 0 n))]
              (sliding-tokenizer-n (subvec tokens 1)
                                   n
                                   (+ i 1)))))

(defn sliding-tokenizer [tokens size]
  (if (>= (count tokens) size)
    (concat (sliding-tokenizer-n tokens size 0)
            (sliding-tokenizer tokens (+ 1 size)))))

(defn over [grammar left right]
  "opportunity for additional logging before calling the real (over)"
  (log/trace (str "parse/over: grammar size: " (count grammar)))
  (over/over grammar left right))

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
    (merge
     {1 (map
         (fn [i]
           [i (+ 1 i)])
         (range 0 n))}
     (reduce (fn [resultant-map this-submap]
               (merge-with union ;; TODO: this could get expensive - consider alternatives.
                           resultant-map this-submap))
             (map (fn [span-pair]
                    (let [left-span (first span-pair)
                          left-boundary (first left-span)
                          right-span (second span-pair)
                          right-boundary (second right-span)]
                      {(- right-boundary left-boundary)
                       (list span-pair)}))
                  spans)))))

(defn lookup-tokens [input-string grammar]
  (let [morph (:morph grammar)
        lookup (:lookup grammar)
        grammar (:grammar grammar)
        tokens (toks input-string lookup morph)]

    (filter #(not (empty? %))
            (toks input-string lookup morph))))

(defn parse-with-segmentation [input n model span-map]
  (log/trace (str "calling p-w-s " n "; span-maps: " (get span-map n)))
  (cond
    (= n 1) input
    (> n 1)
    (let [minus-1 (parse-with-segmentation input (- n 1) model span-map)]
      (merge minus-1
             (reduce (fn [x y]
                       (do
                         (log/trace (str "merge x: " (keys x)))
                         (log/trace (str "merge y: " (keys y)))
                         (merge-with concat x y)))
                     (map (fn [span-pair]
                            {[(first (first span-pair))
                              (second (second span-pair))]
                             (do
                               (let [result
                                     (if (and (not (empty? (get minus-1 (first span-pair))))
                                              (not (empty? (get minus-1 (second span-pair)))))
                                       (do
                                         (log/debug (str "span-pair: " span-pair))
                                         (log/info (str "left: " ((:morph model)
                                                                  (get minus-1 (first span-pair)))))
                                         (log/info (str "right: " ((:morph model)
                                                                   (get minus-1 (second span-pair)))))
                                         (concat
;                                          (lookup-tokens (string/join " " [(first span-pair) (second span-pair)] model))
                                          (over (:grammar model)
                                                (get minus-1 (first span-pair))
                                                (get minus-1 (second span-pair))))))]
                                 (if (not (empty? result))
                                   (log/info
                                    (str "result: "
                                         [(first (first span-pair))
                                          (second (second span-pair))] " "
                                         (string/join "; "
                                                      (map (fn [each-parse]
                                                             (str
                                                              (get each-parse :rule) ":'"
                                                              ((:morph model) each-parse)
                                                              "'"))
                                                           result)))))
                                 result))})
                              (get span-map n)))))))
(defn parses [input model]
  (filter #(not (empty? %))
          (map (fn [segments]
                 (let [segment-count (count segments)
                       token-count-range (range 0 segment-count)
                       input-map (zipmap (map (fn [i] [i (+ i 1)])
                                              token-count-range)
                                         (map (fn [i] (nth segments i))
                                              token-count-range))]
                   (let [all-parses
                         (parse-with-segmentation input-map segment-count model
                                                  (span-map segment-count))]
                     {:segment-count segment-count
                      :complete-parses
                      (get all-parses
                           [0 segment-count])
                      :all-parses all-parses})))
               ;; TODO: move tokenization to within lexicon.
               (lookup-tokens input model))))

(defn parse-with-segmentation-as-strings [input n model span-map]
  (log/trace (str "calling p-w-s " n "; span-maps: " (get span-map n)))
  (cond
    (= n 1) input
    (> n 1)
    (let [minus-1 (parse-with-segmentation-as-strings input (- n 1) model span-map)]
      (merge minus-1
             (reduce (fn [x y]
                       (do
                         (log/trace (str "merge x: " (keys x)))
                         (log/trace (str "merge y: " (keys y)))
                         (merge-with concat x y)))
                     (map (fn [span-pair]
                            {[(first (first span-pair))
                              (second (second span-pair))]
                             (let [left (get minus-1 (first span-pair))
                                   right (get minus-1 (second span-pair))]
                               (log/debug (str "span-pair: " span-pair))
                               (log/info (str "left: " ((:morph model)
                                                         left)))
                               (log/info (str "right: " ((:morph model)
                                                         right)))
                               (let [left-strings (filter string? left)
                                     right-strings (filter string? right)
                                     left-lexemes (mapcat (:lookup model)
                                                          left-strings)
                                     right-lexemes (mapcat (:lookup model)
                                                          right-strings)
                                     left-signs (concat left-lexemes (filter map? left))
                                     right-signs (concat right-lexemes (filter map? right))
                                     debug (do (log/info (str "left-signs: " ((:morph model)
                                                                              left-signs)))
                                               (log/info (str "right-signs: " ((:morph model)
                                                                               right-signs)))
                                               (log/info (str "left-strings: " (string/join "," left-strings)))
                                               (log/info (str "right-strings: " (string/join "," right-strings)))
                                               (log/info (str "looked-up left-strings:" (string/join ","  (map (:morph model)
                                                                                                               (mapcat (:lookup model)
                                                                                                                       left-strings)))))
                                               (log/info (str "looked-up right-strings:" (string/join "," (map (:morph model)
                                                                                                               (mapcat (:lookup model)
                                                                                                                       right-strings))))))
                                     result
                                     (concat
                                      (if (and (not (empty? left-signs))
                                               (not (empty? right-signs)))
                                        (do
                                          (log/debug (str "span-pair: " span-pair))
                                          (over (:grammar model) left-signs right-signs)))
                                      [(string/join " " [(first left-strings) (first right-strings)])])]
                                 (if (not (empty? result))
                                   (log/debug
                                    (str "result: "
                                         [(first (first span-pair))
                                          (second (second span-pair))] " "
                                         (string/join "; "
                                                      (map (fn [each-parse]
                                                             (str
                                                              (get each-parse :rule) ":'"
                                                              ((:morph model) each-parse)
                                                              "'"))
                                                           result)))))
                                 result))})
                              (get span-map n)))))))

(defn parses-with-strings [input model]
  (let [segments (string/split input tokenizer)
        segment-count (count segments)
        token-count-range (range 0 segment-count)
        input-map (zipmap (map (fn [i] [i (+ i 1)])
                               token-count-range)
                          (map (fn [i] [(nth segments i)])
                               token-count-range))]
    (log/debug (str "input map: " input-map))
    (let [all-parses
          (parse-with-segmentation-as-strings input-map segment-count model
                                              (span-map segment-count))]
      {:segment-count segment-count
       :complete-parses
       (filter map? (get all-parses
                         [0 segment-count]))
       :all-parses all-parses})))

(defn parse [input model]
  "return a list of all possible parse trees for a string or a list of lists of maps
   (a result of looking up in a dictionary a list of tokens from the input string)"
  (:complete-parses (parses-with-strings input model)))



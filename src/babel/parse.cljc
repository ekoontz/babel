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

(defn parse [input model]
  "return a list of all possible parse trees for a string or a list of lists of maps
   (a result of looking up in a dictionary a list of tokens from the input string)"
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
                                              (span-map segment-count))
          result
          {:segment-count segment-count
           :complete-parses
           (filter map? (get all-parses
                             [0 segment-count]))
           :all-parses all-parses}]
      (:complete-parses result))))




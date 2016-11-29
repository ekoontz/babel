(ns babel.generate
  (:refer-clojure :exclude [assoc-in get-in deref resolve find parents])
  (:require
   [babel.index :refer [intersection-with-identity]]
   [babel.over :as over :refer [show-bolt spec-info truncate truncate-expressions]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log]) 
   [clojure.string :as string]
   [dag_unify.core :refer [assoc-in copy fail-path get-in fail? strip-refs unify unify!]]))
                                        
;; during generation, will not decend deeper than this when creating a tree:
;; TODO: should also be possible to override per-language.
(def ^:const max-total-depth 6)

;; TODO support setting max-generated-complements to :unlimited
(def ^:const max-generated-complements 20000)

;; use map or pmap.
(def ^:const mapfn map)

(def ^:const randomize-lexemes-before-phrases true)
(def ^:const error-if-no-complements false)

(declare add-all-comps)
(declare add-all-comps-with-paths)
(declare add-complement-to-bolt)
(declare any-possible-complement?)
(declare bolt-depth)
(declare candidate-parents)
(declare do-defaults)
(declare exception)
(declare find-comp-paths-in)
(declare lazy-mapcat)
(declare lexemes-before-phrases)
(declare lightning-bolts)
(declare generate-all)
(declare not-fail?)

;; FIXME: truncate-children=false (the non-default option) is not propagated through all calls,
;; causing trees to be unexpectedly truncated.
(defn generate
  "Return a single expression generated by the given language model, constrained by the given spec."
  [spec language-model
   & {:keys [max-total-depth truncate-children lexicon]
      :or {max-total-depth max-total-depth
           lexicon nil
           truncate-children true}}]
  (if (:generate-fn language-model)
    ((:generate-fn language-model) spec)
    (let [spec (if (or (fail? (unify spec {:synsem {:subcat '()}}))
                       (not (= ::none (get-in spec [:synsem :subcat] ::none))))
                 spec
                 
                 ;; else:
                 (unify spec
                        {:synsem {:subcat '()}}))

          ;; remove metadata (if any)
          ;; that's not relevant to generation:
          spec (dissoc spec
                       :dag_unify.core/serialized)
          
          lexicon (if lexicon lexicon (:lexicon language-model))
          morph (:morph language-model)
          morph-ps (:morph-ps language-model)]
      (log/debug (str "generate: generating from spec: "
                      (strip-refs spec) " with max-total-depth: " max-total-depth ";truncate: " truncate-children))
      (let [expression (first (take 1 (generate-all spec language-model 0
                                                    :max-total-depth max-total-depth
                                                    :truncate-children truncate-children)))]
        (if expression
          (log/debug (str "generate: generated "
                          (morph-ps expression)
                          " for spec:" (strip-refs spec)))
          (log/warn (str "generate: no expression could be generated for spec:" (strip-refs spec))))
        expression))))

(defn generate-all
  "Returns all possible expressions generated by the given language model, constrained by the given spec.
   Depending on the grammar in the language model, could be an infinite number of expressions."
  [spec language-model total-depth
   & {:keys [max-total-depth truncate-children]
      :or {max-total-depth max-total-depth
           truncate-children true}}]
  (let [total-depth (if total-depth total-depth 0)]
    (if truncate-children
      (->
       (lightning-bolts language-model spec 0 total-depth :max-total-depth max-total-depth)
       (add-all-comps language-model total-depth true max-total-depth spec)
       (truncate-expressions [[:head]] language-model))
      (->
       (lightning-bolts language-model spec 0 total-depth :max-total-depth max-total-depth)
       (add-all-comps language-model total-depth false max-total-depth spec)))))

(declare find-comp-paths)

;; TODO: lightning-bolts should use this.
(defn get-lexemes [model spec]
  (if (= false (get-in spec [:phrasal] false))
    (filter #(not-fail? (unify % spec))
            (if-let [index-fn (:index-fn model)]
              (index-fn spec)
              (flatten (vals
                        (or (:lexicon (:generate model)) (:lexicon model))))))))

(defn comp-paths-to-bolts-map
  "return a map between the set of all complements in the given _bolt_,and the lazy sequence of bolts for that spec."
  [bolt model depth max-depth]
  (if (and (not (nil? bolt)))
    (let [comp-paths (find-comp-paths bolt)]
      (if (not (empty? comp-paths))
        (zipmap
         comp-paths
         (map #(let [spec (get-in bolt %)
                     lexemes (get-lexemes model spec)
                     bolts-at (if (< depth max-depth)
                                (lightning-bolts
                                 model
                                 (get-in bolt %)
                                 depth max-depth))
                     lexemes-before-phrases
                     (lexemes-before-phrases depth max-depth)]
                 (if lexemes-before-phrases
                   (concat lexemes bolts-at)
                   (concat bolts-at lexemes)))
              comp-paths))))))

(defn add-comps
  "given a bolt, return the lazy sequence of all bolts derived from this bolt after adding,
   at each supplied path in comp-paths, the bolts for that path."
  [bolt model comp-paths bolts-at-paths depth max-depth & [top-bolt path-from-top]]
  (let [top-bolt (or top-bolt bolt)]
    (if (empty? comp-paths)
      [bolt] ;; done: we've added all the comps to the bolt, so just return the bolt as a singleton vector.
      ;; else, more comp-paths to go.
      (flatten
       (mapfn #(add-comps % model
                          (rest comp-paths)
                          (rest bolts-at-paths)
                          depth max-depth top-bolt path-from-top)
              (let [path (first comp-paths)
                    bolts-at (first bolts-at-paths)]
                (flatten
                 (mapfn
                  (fn [bolt-at]
                    (if (= false (get-in bolt-at [:phrasal]))
                      [(assoc-in bolt path bolt-at)]
                      (let [comps-map (comp-paths-to-bolts-map bolt-at model (+ 1 depth) max-depth)]
                        (when (not (some empty? (vals comps-map)))
                          (let [comp-paths (keys comps-map)
                                comp-bolts (mapfn #(get comps-map %)
                                                  comp-paths)]
                            (mapfn #(do-defaults (assoc-in bolt path %) model)
                                   (add-comps bolt-at
                                              model
                                              comp-paths
                                              comp-bolts
                                              (+ 1 depth)
                                              max-depth
                                              top-bolt)))))))
                  bolts-at))))))))
(defn generate2
  "Return all expressions matching spec _spec_ given the model _model_."
  [spec model]
  (log/debug (str "generate2: spec: " spec))
  (flatten
   (mapfn (fn [bolt]
            (let [comps-map (comp-paths-to-bolts-map bolt model 0 max-total-depth)
                  comp-paths (keys comps-map)
                  comp-bolts (mapfn #(get comps-map %)
                                    comp-paths)]
              ;; filter by the following constraint:
              ;; that for every path that points to a complement of a bolt,
              ;; there is a non-empty set of bolts that satisfies
              ;; that complement's path.
              (when (not (some empty? (vals comps-map)))
                (mapfn #(do-defaults % model)
                       (add-comps bolt
                                  model
                                  comp-paths
                                  comp-bolts
                                  0 max-total-depth
                                  bolt)))))
          (lightning-bolts model spec 0 max-total-depth))))

(defn generate-n
  "Returns all possible expressions generated by the given language model, constrained by the given spec.
   Depending on the grammar in the language model, could be an infinite number of expressions."
  [n spec language-model total-depth
   & {:keys [max-total-depth truncate-children]
      :or {max-total-depth max-total-depth
           truncate-children true}}]
  (let [total-depth (if total-depth total-depth 0)]
    (if truncate-children
      (take n
            (-> 
             (lightning-bolts language-model spec 0 total-depth :max-total-depth max-total-depth)
             (add-all-comps language-model total-depth true max-total-depth spec)
             (truncate-expressions [[:head]] language-model)))
      (take n
            (-> 
             (lightning-bolts language-model spec 0 total-depth :max-total-depth max-total-depth)
             (add-all-comps language-model total-depth true max-total-depth spec))))))

(defn lightning-bolts
  "Returns a lazy-sequence of all possible bolts given a spec, where a bolt is a tree
  such that only the head children are generated. This sequence is used by (generate (above))
  to generate expressions by adding complements using (add-all-comps)."
  [language-model spec depth total-depth
                       & {:keys [max-total-depth]
                          :or {max-total-depth max-total-depth}}]
  (if (nil? spec)
    (exception (str "given a null spec for lightning-bolts.")))
  (log/trace (str "lightning-bolts: depth: (" depth "/" max-total-depth ") and spec-info:"
                 (spec-info spec)))
  (let [grammar (:grammar language-model)
        depth (if depth depth 0)
        ;; this is the relative depth; that is, the depth from the top of the current lightning bolt.
        ;; total-depth, on the other hand, is the depth all the way to the top of the entire
        ;; expression, which might involve several parent lightning bolts.
        parents
        (let [parents (shuffle (candidate-parents grammar spec))]
          (log/trace (str "lightning-bolts: candidate-parents:" (string/join "," (map :rule parents))))
          parents)]
    ;; TODO: use (defn get-lexemes) rather than this code which duplicates (get-lexemes)'s functionality.
    (let [lexical ;; 1. generate list of all phrases where the head child of each parent is a lexeme.
          (when (= false (get-in spec [:head :phrasal] false))
            (lazy-mapcat
             (fn [parent]
               (log/trace (str "lightning-bolts: parent: " (:rule parent) " over lexical heads."))
               (let [lexicon (or (:lexicon (:generate language-model)) (:lexicon language-model))
                     subset (if-let [index-fn (:index-fn language-model)]
                              (do
                                (log/trace (str "using index to find lexical heads for parent:"
                                                (:rule parent)))
                                (index-fn (get-in parent [:head] :top)))
                              (flatten (vals lexicon)))]
                 (let [shuffled-subset (shuffle subset)
                       log (log/debug (str "lexical head candidates:"
                                           (string/join "," (sort (map #((:morph language-model) %)
                                                                       subset)))))
                       result
                       (mapcat #(do
                                  (log/trace (str "trying parent: " (:rule parent) " with lexical head:"
                                                  ((:morph language-model) %)))
                                  (over/overh parent %))
                               shuffled-subset)]
                   (if (and (not (empty? subset)) (empty? result)
                            (> (count subset)
                               50))
                     ;; log/warn because it's very expensive to run
                     ;; over/overh: for every candidate, both parent
                     ;; and candidate head must be copied.
                     (log/warn (str "tried: " (count subset) " lexical candidates with spec:"
                                    (strip-refs spec) " and all of them failed as heads of parent:" (get-in parent [:rule]))))
                   result)))
             (filter #(= false
                         (get-in % [:head :phrasal] false))
                     parents)))
          phrasal ;; 2. generate list of all phrases where the head child of each parent is itself a phrase.
          (if (and (< depth max-total-depth)
                   (= true (get-in spec [:head :phrasal] true)))
            (lazy-mapcat (fn [parent]
                           (log/trace (str "lightning-bolts: parent: " (:rule parent) " over phrasal heads."))
                           (mapcat #(over/overh parent %)
                                   (lightning-bolts language-model (get-in parent [:head])
                                                    (+ 1 depth) (+ 1 total-depth)
                                                    :max-total-depth max-total-depth)))
                           (filter #(= true
                                       (get-in % [:head :phrasal] true))
                                   parents)))]
      (log/debug (str "lexical-heads for parents:" (string/join "," (map :rule parents)) ":"
                      (string/join ","
                                   (map #((:morph language-model) %)
                                        lexical))))
      (if (lexemes-before-phrases total-depth max-total-depth)
        (lazy-cat lexical phrasal)
        (lazy-cat phrasal lexical)))))

(defn add-all-comps
  "At each point in each bolt in the list of list of bolts,
  _bolt-groups_, add all possible complements at all open nodes in the
  bolt, from deepest and working upward to the top. Return a lazy
  sequence of having added all possible complements at each node in
  the bolt."
  [bolts language-model total-depth truncate-children max-total-depth top-level-spec]
  (log/trace (str "add-all-comps: bolt count: " (count bolts)))
  (lazy-mapcat
   (fn [bolt]
     (log/trace (str "adding all comps to bolt: " (show-bolt bolt language-model)))
     (add-all-comps-with-paths [bolt] language-model total-depth
                               (find-comp-paths-in (bolt-depth bolt))
                               truncate-children max-total-depth
                               top-level-spec))
   bolts))

(defn add-all-comps-with-paths [bolts language-model total-depth comp-paths
                                truncate-children max-total-depth top-level-spec]
  (log/debug (str "add-all-comps to bolts with paths to comps: "
                  (string/join "," comp-paths)))
                  
  (if (empty? comp-paths)
    bolts
    (add-all-comps-with-paths
     (lazy-mapcat
      (fn [bolt]
        (let [path (first comp-paths)]
          (add-complement-to-bolt bolt path
                                  language-model (+ total-depth (count path))
                                  top-level-spec
                                  :max-total-depth max-total-depth
                                  :truncate-children truncate-children
                                  :top-level-spec top-level-spec)))
      bolts)
     language-model total-depth (rest comp-paths)
     truncate-children max-total-depth top-level-spec)))

(defn do-defaults [tree language-model]
  (log/debug (str "calling do-defaults on tree:" ((:morph language-model) tree)))
  (if-let [default-fn (:default-fn language-model)]
    (let [result
          (default-fn tree)]
      (log/debug (str "result of calling do-defaults on tree:" ((:morph language-model) result)))
      result)
    ;;
    (do
      (log/trace (str "language-model has no default function."))
      tree)))

(defn add-complement-to-bolt [bolt path language-model total-depth top-level-spec
                              & {:keys [max-total-depth truncate-children]
                                 :or {max-total-depth max-total-depth
                                      truncate-children true}}]
  (log/debug (str "add-complement-to-bolt: " (show-bolt bolt language-model)
                  "@[" (string/join " " path) "]" "^" total-depth
                  "; top-level spec: " (strip-refs top-level-spec)))
  (log/debug (str "add-complement-to-bolt: complement-spec: (1) "
                  (strip-refs (get-in bolt path))))

  (let [lexicon (or (:lexicon (:generate language-model))
                    (:lexicon language-model))
        from-bolt bolt ;; so we can show what (add-complement-to-bolt) did to the input bolt, for logging.
        spec (strip-refs (get-in bolt path))
        debug (log/debug (str "spec to find complement lexemes: " spec))
        complement-candidate-lexemes
        (if (not (= true (get-in bolt (concat path [:phrasal]))))
          (if-let [index-fn
                   (:index-fn language-model)]
            (do (log/debug (str "add-complement-to-bolt with bolt: " (show-bolt bolt language-model)
                                " calling index-fn with spec: " spec ))
                (let [result
                      (index-fn spec)]
                    result))
            (do (log/warn (str "add-complement-to-bolt: no index-fn for model:" (:name language-model) ": using entire lexicon."))
                (flatten (vals lexicon)))))
        debug 
        (log/trace (str "lexical-complements (pre-over):"
                        (string/join ","
                                     (map #((:morph language-model) %)
                                          complement-candidate-lexemes))))
               
        bolt-child-synsem (get-in bolt (concat path [:synsem]))
        lexical-complements (shuffle
                             (filter (fn [lexeme]
                                       (and (not-fail? (unify (get-in lexeme [:synsem] :top)
                                                              bolt-child-synsem))))
                                     complement-candidate-lexemes))]
    (log/debug (str "lexical-complements (post-over):"
                    (string/join ","
                                 (map #((:morph language-model) %)
                                      lexical-complements))))
    (->>
     (let [phrasal-complements (if (and (> max-total-depth total-depth)
                                        (= true (get-in spec [:phrasal] true)))
                                 (do
                                   (log/debug (str "calling (generate-all) from add-complement-to-bolt with bolt:"
                                                   (show-bolt bolt language-model)))
                                   (generate-all spec language-model (+ (count path) total-depth)
                                                 :max-total-depth max-total-depth)))
           lexemes-before-phrases (lexemes-before-phrases total-depth max-total-depth)]
       (cond (and lexemes-before-phrases
                  (empty? lexical-complements)
                  (= false (get-in spec [:phrasal] true)))
             (log/warn (str "failed to generate any lexical complements at path:" path " with spec: "
                            (strip-refs spec)))
             
             (and lexemes-before-phrases
                  (= true (get-in spec [:phrasal] false))
                  (empty? phrasal-complements))
             (log/warn (str "failed to generate any phrasal complements with spec: "
                            (strip-refs spec)))
             
             (and (empty? lexical-complements)
                  (empty? phrasal-complements))
             
             (let [message (str "add-complement-to-bolt: could generate neither phrasal "
                                "nor lexical complements for "
                                "bolt:" (show-bolt bolt language-model) "; immediate parent: "
                                (get-in bolt (concat (butlast path) [:rule]) :norule) " "
                                "while trying to create a complement: "
                                (spec-info spec)
                                )]
               (log/debug message)
               (if error-if-no-complements (exception message)))
             
             lexemes-before-phrases
             (take max-generated-complements
                   (lazy-cat lexical-complements phrasal-complements))
             true
             (take max-generated-complements
                   (lazy-cat phrasal-complements lexical-complements))))

     (mapfn (fn [complement]
              (let [unified
                    (unify! (copy bolt)
                            (assoc-in {} path 
                                      (copy complement)))]
                (if (and (not-fail? unified)
                         truncate-children)
                  (truncate unified [path] language-model)
                  unified))))

     (filter #(not-fail? %))

     (mapfn (fn [tree]
              (do-defaults tree language-model))))))
  
(defn bolt-depth [bolt]
  (if-let [head (get-in bolt [:head] nil)]
    (+ 1 (bolt-depth head))
    0))

(defn find-end-of-bolt [bolt]
  (cond (= true (get-in bolt [:phrasal]))
        (cons :head
              (find-end-of-bolt (get-in bolt [:head])))
        true
        []))

(defn find-comp-paths [bolt & [path]]
  (if (not (= false (get-in bolt [:phrasal])))
    (let [path (if (nil? path)
                 (rest (find-end-of-bolt bolt))
                 path)]
      (if (not (empty? path))
        (concat 
         [(vec (concat path [:comp]))]
         (find-comp-paths bolt (rest path)))
        [[:comp]]))))

(defn find-comp-paths-in [depth]
  (cond
    ;; most-frequent cases done statically:
    (= 0 depth) nil
    (= 1 depth) [[:comp]]
    (= 2 depth) [[:head :comp][:comp]]
    (= 3 depth) [[:head :head :comp][:head :comp][:comp]]
    (= 4 depth) [[:head :head :head :comp][:head :head :comp][:head :comp][:comp]]

    ;; 
    true
    (cons (vec (concat (take (- depth 1) (repeatedly (fn [] :head))) [:comp]))
          (find-comp-paths-in (- depth 1)))))

(defn candidate-parents
  "find subset of _rules_ for which each member unifies successfully with _spec_"
  [rules spec]
  (log/trace (str "candidate-parents: spec: " (strip-refs spec)))
  (let [result
        (filter not-fail?
                (mapfn (fn [rule]
                         (log/trace (str "candidate-parents: testing rule: " (:rule rule)))
                         (if (and (not-fail? (unify (get-in rule [:synsem :cat] :top)
                                                    (get-in spec [:synsem :cat] :top))))
                           ;; TODO: add checks for [:synsem :subcat] valence as well as [:synsem :cat].
                           (do
                             (log/trace (str "candidate-parents: " (:rule rule) " is a head candidate for spec:"
                                             (strip-refs spec)))
                             (unify spec rule))
                           (do
                             (log/trace (str "candidate-parents: " (:rule rule) " is *not* a head candidate for spec:"
                                             (strip-refs spec)))
                             :fail)))
                       rules))]
    (log/trace (str "candidate-parents: " 
                    (string/join "," (map :rule result))
                    " for spec: " (strip-refs spec)))
    (if (empty? result)
      (log/trace (str "candidate-parents: " 
                      "no parents found for spec: " (spec-info spec)))
      (log/trace (str "candidate-parents: " 
                    (string/join "," (map :rule result))
                    " for: " (spec-info spec))))
    result))

(defn exception [error-string]
  #?(:clj
     (throw (Exception. (str ": " error-string))))
  #?(:cljs
     (throw (js/Error. error-string))))

;; Thanks to http://clojurian.blogspot.com.br/2012/11/beware-of-mapcat.html
(defn lazy-mapcat  [f coll]
  (lazy-seq
   (if (not-empty coll)
     (concat
      (f (first coll))
      (lazy-mapcat f (rest coll))))))

(defn lexemes-before-phrases
  "returns true or false: true means generate by adding lexemes first;
  otherwise, by adding phrases first. Takes depth as an argument,
  which makes returning true (i.e. lexemes first) increasingly likely
  as depth increases."
  [depth max-total-depth]
  (if (not randomize-lexemes-before-phrases)
    false
    (if (> max-total-depth 0)
      (let [prob (- 1.0 (/ (- max-total-depth depth)
                           max-total-depth))]
        (log/trace (str "P(c," depth ") = " prob " (c: probability of choosing lexemes rather than phrases given depth " depth ")"))
        (> (* 10 prob) (rand-int 10)))
      false)))

(defn not-fail? [arg]
  (not (= :fail arg)))

(ns babel.over
  (:refer-clojure :exclude [get get-in merge resolve find parents])
  (:require
   [clojure.core :as core] ;; This allows us to use core's get-in by doing "(core/get-in ..)"
   [clojure.set :refer :all]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [dag-unify.core :refer [fail? fail-path get-in merge strip-refs unifyc]]
   [babel.lexiconfn :refer [sem-impl]]))

;; TODO: need better debugging throughout this file to diagnose generation failures.
;; this (get-fail-path) is one example.
(declare get-fail-path)

;; tree-building functions: useful for developing grammars.
(defn into-list-of-maps [arg]
  (cond

   (seq? arg)
   arg

   (set? arg)
   (seq arg)

   (map? arg)
   (list arg)

   (nil? arg)
   (list :top)

   (keyword? arg)
   (list arg)

   true (throw (Exception. (str "into-map: don't know what to do with a " (type arg) ".")))))

(declare overh)
(declare overc)

(defn over-each-parent-head [parents head]
  (if (not (empty? parents))
    (let [each-parent (first parents)]
      (log/debug (str "over-each-parent-head: each-parent type:" (type (first parents))))
      (log/debug (str "over-each-parent-head: head type:" (type head)))
      (lazy-cat
       (overh each-parent head)
       (over-each-parent-head (rest parents) head)))
    (do
      (log/debug (str "over-each-parent-head: done. returning nil"))
      nil)))

(defn over-each-parent-comp [parents comp]
  (log/trace (str "over-each-parent-comp: parents type: " (type parents)))
  (log/trace (str "over-each-parent-comp: comp type: " (type comp)))
  (if (not (empty? parents))
    (let [each-parent (first parents)]
      (log/trace (str "over-each-parent-comp: each-parent type:" (type (first parents))))
      (log/trace (str "over-each-parent-comp: comp type:" (type comp)))
      (lazy-cat
       (overc each-parent comp)
       (over-each-parent-comp (rest parents) comp)))
    (do
      (log/trace (str "over-each-parent-comp: done. returning nil"))
      nil)))

(defn over-each-head-child [parent children morph]
  (log/trace (str "over-each-head-child: parent type: " (type parent)))
  (log/trace (str "over-each-head-child: head children type: " (type children)))
  (if (not (empty? children))
    (let [each-child (first children)]
      (lazy-cat
       (overh parent each-child morph)
       (over-each-head-child parent (rest children) morph)))
    (do
      (log/trace (str "over-each-head-child: done. returning nil."))
      nil)))

(defn over-each-comp-child [parent children]
  (log/trace (str "over-each-comp-child: parent type: " (type parent)))
  (log/trace (str "over-each-comp-child: comp children type: " (type children)))
  (if (not (empty? children))
    (let [each-child (first children)]
      (lazy-cat
       (overc parent each-child)
       (over-each-comp-child parent (rest children))))
    (do
      (log/trace (str "over-each-comp-child: done. returning nil."))
      nil)))

(defn moreover-head [parent child lexfn-sem-impl morph]
  (do
    (log/debug (str "moreover-head (candidate) parent sem:    " (strip-refs (get-in parent '(:synsem :sem) :no-semantics))))
    (log/debug (str "moreover-head (candidate) head child sem:" (strip-refs (get-in child '(:synsem :sem) :top))))
    (let [result
          (unifyc parent
                  (unifyc {:head child}
                          {:head {:synsem {:sem (lexfn-sem-impl (get-in child '(:synsem :sem) :top))}}}))]
      (if (not (fail? result))
        (let [debug (log/debug (str "moreover-head " (get-in parent '(:rule)) " succeeded: " (:rule result)
                                    ":'" (morph result) "'"))
              debug (log/trace (str " resulting sem: " (strip-refs (get-in result '(:synsem :sem)))))]
          (merge {:head-filled true}
                 result))

        ;; attempt to put head under parent failed: provide diagnostics through log/debug messages.
        ;; TODO: make (fail-path) call part of each log/debug message to avoid computing it if log/debug is not enabled.
        (let [fail-path (get-fail-path (get-in parent [:head]) child)
              debut (log/debug (str "moreover-head: failed to add child to parent: " (:rule parent)))
              debug (log/trace (str "parent " (:rule parent)
                                    " wanted head with: "
                                    (strip-refs (get-in parent [:head]))))
              debug (log/debug (str "parent " (:rule parent)
                                    " wanted head with: "
                                    (strip-refs (get-in parent [:head]))))
              debug (log/trace (str "candidate child has synsem: "
                                   (strip-refs
                                    (get-in
                                     (unifyc child
                                             {:synsem {:sem (lexfn-sem-impl (get-in child '(:synsem :sem) :top))}})
                                     [:synsem]))))
              debug (log/warn (str "fail-path: " (get-fail-path (get-in parent [:head])
                                                                child)))
              debug (log/warn (str "  parent@" fail-path "="
                                    (get-in parent (concat [:head] fail-path))))
              debug (log/warn (str "    head@" fail-path "="
                                   (get-in child fail-path)))]
          :fail)))))

;; Might be useful to set the following variable to true,
;; if doing grammar development and it would be unexpected
;; to have a failing result from calling (moreover-comp)
;; with certain arguments.
(def ^:dynamic *throw-exception-if-failed-to-add-complement* false)

(defn moreover-comp [parent child lexfn-sem-impl]
  (log/debug (str "moreover-comp type parent: " (type parent)))
  (log/debug (str "moreover-comp type comp:" (type child)))

  (let [result
        (unifyc parent
                (unifyc {:comp child}
                        {:comp {:synsem {:sem (lexfn-sem-impl (get-in child '(:synsem :sem) :top))}}}))]

    (if (not (fail? result))
      (let [debug (log/debug (str "moreover-comp added parent to child: " (:rule parent)))]
        (let [result
              (merge {:comp-filled true}
                     result)]
          result))
      (do
        (log/debug "moreover-comp: fail at: " (fail-path result))
        (if (and
             *throw-exception-if-failed-to-add-complement*
             (get-in child '(:head)))
          (throw (Exception. (str "failed to add complement: " child "  to: phrase: " parent
                                  ". Failed path was: " (fail-path result)
                                  ". Value of parent at path is: "
                                  (get-in parent (fail-path result))
                                  "; Synsem of child is: "
                                  (get-in child '(:synsem) :top)))))
        (log/debug "moreover-comp: complement synsem: " (get-in child '(:synsem) :top))
        (log/debug "moreover-comp:  parent value: " (get-in parent (fail-path result)))
        :fail))))

(defn overh [parent head morph]
  "add given head as the head child of the phrase: parent."
  (log/trace (str "overh parent type: " (type parent)))
  (log/trace (str "overh head  type: " (type head)))

  (log/trace (str "set? parent:" (set? parent)))
  (log/trace (str "seq? parent:" (seq? parent)))
  (log/trace (str "seq? head:" (seq? head)))
  (log/trace (str "vector? head:" (vector? head)))

  (when (map? parent)
    (do
      (if (get-in parent '(:aliases))
        (log/debug (str "overh: parent aliases:" (get-in parent '(:aliases)))))
      (if (get-in parent '(:comment))
        (log/trace (str "overh: parent comment:" (get-in parent '(:comment)))))
      (if (get-in parent '(:rule))
        (log/debug (str "overh: parent rule:" (get-in parent '(:rule)))))))

  (cond

   (nil? head)
   nil

   (or
    (seq? parent)
    (set? parent)
    (vector? parent))
   (let [parents (lazy-seq parent)]
     (filter (fn [result]
               (not (fail? result)))
             (over-each-parent-head parents head)))

   (or (set? head)
       (vector? head))
   (do (log/trace "head is a set: converting to a seq.")
       (overh parent (lazy-seq head) morph))

   (seq? head)
   (let [head-children head]
     (log/trace (str "head is a seq - actual type is " (type head)))
     (filter (fn [result]
               (not (fail? result)))
             (over-each-head-child parent head-children morph)))

   true
   ;; TODO: 'true' here assumes that both parent and head are maps: make this assumption explicit,
   ;; and save 'true' for errors.
   (let [result (moreover-head parent head sem-impl morph)
         is-fail? (fail? result)
         label (if (:rule parent) (:rule parent) (:comment parent))]
     (log/trace (str "overh result keys: " (if (map? result) (keys result) "(not a map)")))
     (log/trace (str "overh italian value: " (if (map? result) (get-in result '(:italiano)) "(not a map)")))
     (log/trace (str "overh italian :a value: " (if (map? result) (get-in result '(:italiano :a)) "(not a map)")))
     (log/trace (str "overh italian :b value: " (if (map? result) (get-in result '(:italiano :b)) "(not a map)")))
     (if (not is-fail?)
       (list result)))))

;; Haskell-looking signature:
;; (parent:map) X (child:{set,seq,fs}) => list:map
;; TODO: verify that the above commentn about the signature
;; is still true.
(defn overc [parent comp]
  "add given child as the comp child of the phrase: parent."

  (log/debug (str "set? parent:" (set? parent)))
  (log/debug (str "seq? parent:" (seq? parent)))
  (log/debug (str "seq? comp:" (seq? comp)))

  (log/debug (str "type of parent: " (type parent)))
  (log/debug (str "type of comp  : " (type comp)))
  (log/debug (str "nil? comp  : " (nil? comp)))

  (cond
   (nil? comp) nil

   (or
    (seq? parent)
    (set? parent)
    (vector? parent))
   (let [parents (lazy-seq parent)]
     (filter (fn [result]
               (not (fail? result)))
             (over-each-parent-comp parents comp)))

   (future? comp)
   (overc parent (deref comp))

   (or (set? comp)
       (vector? comp))
   (do (log/debug "comp is a set: converting to a seq.")
       (overc parent (lazy-seq comp)))

   (seq? comp)
   (let [comp-children comp]
     (log/debug (str "comp is a seq - actual type is " (type comp)))
     (filter (fn [result]
               (not (fail? result)))
             (over-each-comp-child parent comp-children)))

   true
   (let [result (moreover-comp parent comp sem-impl)
         is-fail? (fail? result)]
     (if (not is-fail?)
       (list result)))))

(defn overhc [parent head comp morph]
  (overc (overh parent head morph) comp))

;; TODO: distinguish between when:
;; 1) called with only a child1 (no child2),
;; 2) called with both a child1 and a child2, but child2's supplied value is nil:
;;    should be treated the same as empty list.
(defn over [parents child1 & [child2]]
  (cond (vector? child1)
        (over parents (seq child1) child2)
        (vector? child2)
        (over parents child1 (seq child2))
        true
  (if (nil? child2) (over parents child1 :top)
      (if (map? parents)
        (over (list parents) child1 child2)
        (if (not (empty? parents))
          (let [parent (first parents)]
            (log/debug (str "over: parent: " (first (:aliases parent))))
            (concat
             (cond (and (map? parent)
                        (not (nil? (:serialized parent))))
                   ;; In this case, supposed 'parent' is really a lexical item: for now, definition of 'lexical item' is,
                   ;; it has a non-nil value for :serialized - just return nil, nothing else to do.

                   (throw (Exception. (str "Don't know what to do with this parent: " parent)))

                   ;; if parent is a symbol, evaluate it; should evaluate to a list of expansions (which might also be symbols, etc).
                   (symbol? parent)
                   (over (eval parent) child1 child2)

                   ;; if parent is map, do introspection: figure out the schema from the :schema-symbol attribute,
                   ;; and figure out head-comp ordering from :first attribute.
                   (and (map? parent)
                        (not (nil? (:schema-symbol parent))))
                   (filter (fn [each]
                             (not (fail? each)))
                           (overhc parent
                                   (if (= (:first parent) :head)
                                     child1 child2)
                                   (if (= (:first parent) :head)
                                     child2 child1)))
                   true
                   (throw (Exception. (str "Don't know what to do with parent: " parent))))
             (over (rest parents) child1 child2))))))))

(defn get-fail-path [map1 map2]
  (if (and (map? map1)
           (map? map2))
    (let [keys1 (keys map1)
          keys2 (keys map2)
          fail-keys (mapcat (fn [key]
                              (if (fail?
                                   (unifyc (get-in map1 [key] :top)
                                           (get-in map2 [key] :top)))
                                (list key)))
                            (set (concat keys1 keys2)))]
      (let [first-fail-key (first fail-keys)]
        (if (not (empty? fail-keys))
          (cons
           first-fail-key (get-fail-path (get-in map1 [first-fail-key])
                                         (get-in map2 [first-fail-key])))
          (if (not (nil? first-fail-key))
            [first-fail-key]))))))


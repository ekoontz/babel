[![Clojars Project](https://img.shields.io/clojars/v/babel.svg)](https://clojars.org/babel)
[![Build Status](https://secure.travis-ci.org/ekoontz/babel.png?branch=master)](http://travis-ci.org/ekoontz/babel)

# babel

Important note: I've moved on to the next iteration of these ideas in a [new project called Babylon.](https://github.com/ekoontz/babylon)

A Clojure library for generation and parsing of natural language expressions.

```clojure
user> (require 'babel.italiano)
nil
user> (in-ns 'babel.italiano)
babel.italiano> (repeatedly #(-> {:root {:italiano {:italiano "sapere"}}
                                  :synsem {:cat :verb
				           :subcat []
                                           :sem {:subj {:pred :mario}
                                                 :tense :future
                                                 :pred :know-s}}
                                  :phrasal true
                                  :head {:phrasal false}
                                  :comp {:phrasal false}}
                                 (generate (grammar/medium))
                                 (morph)
                                 pprint
                                 time))
"Mario saprà"
"Elapsed time: 507.604262 msecs"
"Mario saprà"
"Elapsed time: 479.080943 msecs"
"Mario saprà"
"Elapsed time: 513.452862 msecs"
"Mario saprà"
"Elapsed time: 529.560233 msecs"
..
```

```clojure
user> (require 'babel.english)
nil

;; generate a random expression in English
user> (babel.english/morph (babel.english/generate))
"your womens' new cities will lose me"

;; generate an infinite number of random expressions in English
user> (repeatedly #(println (babel.english/morph (babel.english/generate))))
Antonia and Luisa's first sigh
Gianluca's first pizza
you all turn a car down
Juan's short car was knowing the short bicycle

.. hit (control-c to stop) ..

;; generate the first 5 of all the expressions possible
user=> (map #(println (babel.english/morph %)) (take 5 (babel.english/generate-all)))
my first books
some first book
your first book
his first books
that first book
(nil nil nil nil nil)

```

The function `babel.english/generate` generates an English expression
with no specified constraints: that is, the expression will only be
constrained by the English grammar and lexicon defined by Babel.

The generated expression is a Clojure map representing the phrase
structure of the expression. We pass this to the function
`babel.english/morph` (short for "morphology") to convert this into a
human readable string.

```clojure
;; generate an English sentence about dogs eating something:
user> (def spec {:synsem {:sem {:pred :eat :subj {:pred :dog}}}})
user> (-> spec babel.english/generate babel.english/morph)
"your first student's new dogs used to eat a small music's pizza"
```

Above, we now provide some constraints to the generation
process. Specifically, we require that the expression must be about
eating, and that the subject of the eating must be a dog or dogs (the
value `:dog` is the semantics for the English word "dog").

```clojure
;; generate a random expression in Italian
user> (require 'babel.italiano)
(-> :top babel.italiano/generate babel.italiano/morph)
"qualche neonato cittadino bene non bene non la sua"

;; generate an Italian sentence about dogs drinking
(-> {:synsem {:sem {:pred :drink :subj {:pred :dog}}}} babel.italiano/generate babel.italiano/morph)
"in delle brutto isole uno corto cane berrà la tua ensalata"
user> 
```

```shell
demo.sh
```

runs
[babel.english.demo/demo](https://github.com/ekoontz/babel/blob/master/src/babel/english/demo.cljc)
and will demonstrate some of the library's abilities to generate
English expressions. [Here is the output of a sample run of
demo.sh](https://gist.github.com/ekoontz/57c332d85ccf47503666c72fe241cb14)

```shell
lein run -m babel.test.en/sentences 100
```

runs [babel.english/sentences](https://github.com/ekoontz/babel/blob/master/src/babel/english.cljc#L76) and will generate 100 random English sentences. [Here is the output of a sample run](https://gist.github.com/ekoontz/999b640014578b408437b97d5fdc7221).


```shell
lein test
```

will show a more in-depth verification of the library's behavior.

# HPSG (Head Driven Phrase Structure Grammar)

Babel is based on a linguistic theory called HPSG. For more details please see:

- http://hpsg.stanford.edu/
- https://en.wikipedia.org/wiki/Head-driven_phrase_structure_grammar

# Future Plans

Learn more about:

- https://en.wikipedia.org/wiki/Minimal_recursion_semantics
- https://en.wikipedia.org/wiki/Rhetorical_Structure_Theory

# License

Copyright © 2016 Eugene Koontz

Distributed under the Eclipse Public License, the same as Clojure.
Please see the `epl-v10.html` file at the top level of this repo.

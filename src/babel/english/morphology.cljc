(ns babel.english.morphology
  (:refer-clojure :exclude [assoc-in get-in resolve replace reverse])
  (:require [babel.pos :refer (noun)]
            [clojure.string :as string :refer [join replace trim]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [babel.logjs :as log]) 
            [dag_unify.core :as u :refer [assoc-in copy fail? get-in ref? unify]]
            [dag_unify.diagnostics :refer [strip-refs]]
            [babel.unify-compat :refer [dissoc-paths]]))

(declare get-string)
(declare plural-en)

(def ^:const present-participle-note "right now")

(defn fo [input & {:keys [from-language lexicon show-notes]
                   :or {from-language nil
                        lexicon nil
                        show-notes true}}]
  (let [input (if (and (or (= "it" from-language)
                           (= "fr" from-language))
                       (not (and (= :past (get-in input [:synsem :sem :tense]))
                                 (= :simple (get-in input [:synsem :sem :aspect]))))
                       (not (= :lui (get-in input [:synsem :sem :subj :pred])))
                       (not (= :lei (get-in input [:synsem :sem :subj :pred]))))
                ;; remove :notes in these circumstances.
                (dissoc-paths input [[:english :a :note] ;; this handles "you" in "you wash yourself"
                                     [:english :b :b :note]]) ;; this handles "yourself" in the same sentence.
                input)

        input (cond (and (some #(= from-language %) ["es" "fr"])
                         (some #(= (get-in input [:english :a :english]) %) ["they"]))
                    (cond (= :fem (get-in input [:english :a :agr :gender]))
                          (assoc-in input [:english :a :note] "♀")
                          (= :masc (get-in input [:english :a :agr :gender]))
                          (assoc-in input [:english :a :note] "♂")
                          true input)

                    (and (some #(= from-language %) ["es" "fr" "it"])
                         (some #(= (get-in input [:english :a :english]) %) ["it"]))
                    (cond (= :fem (get-in input [:english :a :agr :gender]))
                          (assoc-in input [:english :a :note] "♀")
                          (= :masc (get-in input [:english :a :agr :gender]))
                          (assoc-in input [:english :a :note] "♂")
                          true input)

                    true input)]
    (cond ;; fo
      (= input :fail)
      (str input)

      (string? input)
      input

      (string? (get-in input [:english]))
      (get-in input [:english])

      (and (map? input)
           (map? (get-in input [:english])))
      (get-string (get-in input [:english])
                  :from-language from-language
                  :lexicon lexicon
                  :show-notes show-notes)
      true
      "")))

(defn exception [error-string]
  #?(:clj
     (throw (Exception. (str ": " error-string))))
  #?(:cljs
     (throw (js/Error. error-string))))

(defn present-participle-of [stem]
  (cond
    (re-find #"ie$" stem)
    (str (replace stem #"..$" "y") "ing")
          
    (re-find #"..e$" stem) ;; "..": avoid matching two-letter word "be"
    (str (replace stem #"e$" "") "ing")
          
    (re-find #"[eu]m$" stem)
    (str (replace stem #"m$" "mm") "ing")
          
    (re-find #"[ou]p$" stem)
    (str (replace stem #"p$" "pp") "ing")
          
    (re-find #"[^e][a]n$" stem)
    (str (replace stem #"n$" "nn") "ing")
          
    (re-find #"[ou]b$" stem)
    (str (replace stem #"b$" "bb") "ing")
          
    (re-find #"[^aeiou][eou]t$" stem)
    (str (replace stem #"t$" "tt") "ing")
          
    true
    (str stem "ing")))

;; TODO: (defn get-string) is a big unmaintainable mess!
(defn get-string
  "transform _word_ as a map into a string"
  [word & {:keys [from-language lexicon show-notes]
           :or {from-language nil
                lexicon nil
                show-notes true}}]
  (log/debug (if (map? word) (str "get-string: " (strip-refs word) "; from-language: " from-language "; show-notes:" show-notes)))
  (cond ;; (get-string)
    
    (ref? word)
    (get-string @word :show-notes show-notes :lexicon lexicon :from-language from-language)

    (= word :top)
    ".."
    
    (and (map? word)
         (nil? (:a word))
         (nil? (:b word))
         (nil? (:english word))
         (nil? (:english word)))
    ".."

    (and (= true (get-in word [:exception] false))
         ;; for exceptions, lexical compilation sets [:exception] to true
         ;; and the :english is the inflected irregular form which does not need further conjugation/inflection.
         (string? (get-in word [:english])))
    (get-in word [:english])

    (= (get-in word '(:infl)) :infinitive)
    (if (string? (get-in word '(:english)))
      (str "to " (get-in word '(:english)))
      (exception (str "don't know how to find infinitive for word: " word)))

    ;; "to do [past]" + "well" => "did well"
    (and (= (get-in word '(:cat)) :verb)
         (= (get-in word '(:infl)) :past)
         (string? (get-in word '(:a :past))))
    (str (get-in word '(:a :past)) " "
         (get-string (get-in word '(:b))
                     :from-language from-language
                     :lexicon lexicon
                     :show-notes show-notes))

    (and (string? (get-in word [:english]))
         (= :masc (get-in word [:gender]))
         (= true (get-in word [:pronoun]))
         (some #(= from-language %) ["fr" "it"])
         ;; If a pronoun is 'gendered', it means that the gender
         ;; is implicit in the word, e.g. "he" and "she".
         ;; An example of a non-gendered pronoun in English is "they",
         ;; or "loro" in Italian, but in contrast, in French, "ils"
         ;; and "elles" *are* gendered.
         (= false (unify false (get-in word [:agr :gendered] false))))
    (trim (str (get-in word [:english]) " (♂)"))
    
    (and (string? (get-in word [:english]))
         (= :fem (get-in word [:gender]))
         (= true (get-in word [:pronoun]))
         (some #(= from-language %) ["fr" "it"])
         (= false (unify false (get-in word [:agr :gendered] false))))
    (trim (str (get-in word [:english]) " (♀)"))
    
    ;; :note is used for little annotations that are significant in italian but not in english
    ;; e.g. gender signs (♂,♀) on nouns like "professore" and "professoressa".
    (and (string? (get-in word '(:english)))
         (string? (get-in word '(:note))))
    (str (trim (get-string (dissoc word :note)
                           :from-language from-language
                           :lexicon lexicon
                           :show-notes show-notes))
         (if (not (= false show-notes))
           (str " (" (trim (get-in word '(:note))) ")")))

    (= (get-in word '(:a)) :top)
    (str
     ".." " " (get-string (get-in word '(:b))
                          :from-language from-language
                          :lexicon lexicon
                          :show-notes show-notes))

    ;; show elipsis (..) if :b is not specified.
    (and
     (= (get-in word '(:b)) :top)
     (string? (get-string (get-in word '(:a))
                          :from-language from-language
                          :lexicon lexicon
                          :show-notes show-notes)))
    (str
     (get-string (get-in word '(:a))
                 :from-language from-language
                 :lexicon lexicon
                 :show-notes show-notes)
     " " "..")

    ;; show elipsis (..) if :a is not specified.
    (and
     (= (get-in word '(:b)) :top)
     (string? (get-in word '(:a :english))))
    (str
     (get-string (get-in word '(:a :english))
                 :from-language from-language
                 :lexicon lexicon
                 :show-notes show-notes)
     " " "..")

    (string? word)
    (trim word)

    ;; (could have) + (to go) => "could have gone"
    (and
     (get-in word '(:a))
     (get-in word '(:b))
     (string? (get-in word '(:a :past)))
     (= (get-in word '(:a :past)) "could have")
     (string? (get-in word '(:b :past-participle)))
     (= (get-in word '(:a :infl)) :past))
    (join " " (list (get-in word '(:a :past))
                    (get-in word '(:b :past-participle))))

    ;; (could have) + (to sleep) => "could have slept"
    (and
     (get-in word '(:a))
     (get-in word '(:b))
     (string? (get-in word '(:a :past)))
     (= (get-in word '(:a :past)) "could have")
     (string? (get-in word '(:b :past)))
     (= (get-in word '(:a :infl)) :past))
    (join " " (list (get-in word '(:a :past))
                    (get-in word '(:b :past))))

    ;; (could have) + (do X) => "could have done X"
    (and
     (get-in word '(:a))
     (get-in word '(:b))
     (string? (get-in word '(:a :past)))
     (= (get-in word '(:a :past)) "could have")
     (string? (get-in word '(:b :a :past-participle)))
     (= (get-in word '(:a :infl)) :past))
    ;; recursive call after inflecting '(:b :a) to past.
    (get-string {:a (get-in word '(:a))
                 :b {:a (get-in word '(:b :a :past-participle))
                     :b (get-in word '(:b :b))}}
                :from-language from-language
                :lexicon lexicon
                :show-notes show-notes)

    ;; (could have) + (make X) => "could have made X"
    (and
     (get-in word '(:a))
     (get-in word '(:b))
     (string? (get-in word '(:a :past)))
     (= (get-in word '(:a :past)) "could have")
     (string? (get-in word '(:b :a :past)))
     (= (get-in word '(:a :infl)) :past))
    ;; recursive call after inflecting '(:b :a) to past.
    (get-string {:a (get-in word '(:a))
                 :b {:a (get-in word '(:b :a :past))
                     :b (get-in word '(:b :b))}}
                :from-language from-language
                :lexicon lexicon
                :show-notes show-notes)
    (and
     (get-in word [:a])
     (get-in word [:b]))
    (let [string-a (get-string (get-in word [:a])
                               :show-notes show-notes :lexicon lexicon :from-language from-language)
          string-b (get-string (get-in word [:b])
                               :show-notes show-notes :lexicon lexicon :from-language from-language)]
      (log/debug (str "A AGR1: " (strip-refs (get-in word [:a]))))
      (log/debug (str "A AGR2: " (strip-refs (get-in word [:a :agr]))))
      (log/debug (str "A AGR3: " (strip-refs (get-in word [:a :agr :number]))))
      (log/debug (str "A STRING-A: " string-a))
      (log/debug (str "B STRING-B: " string-b))
      (cond
        (and (= string-b "s")
             (nil? (re-find #"s$" string-a)) ;; plural noun phrase does *not* end with 's' (e.g. "the women"
             (= :plur (get-in word [:a :agr :number]))
             (= false (get-in word [:a :propernoun] false)))
        (str string-a "s'") ;; "the women" + "s" => "the womens'"

        (and (= string-b "s")  ;; plural noun phrase *does* end with 's' (e.g. "the women"
             (= :plur (get-in word [:a :agr :number]))
             (= false (get-in word [:a :propernoun] false)))
        (str string-a "'") ;; "the dogs" + "s" => "the dogs'"

        (= string-b "s")
        (str string-a "'" string-b)  ;; "the dog" + "s" => "the dog's"

        true
        (join " "
              [(str (get-string (get-in word '(:a)) :show-notes show-notes :lexicon lexicon :from-language from-language)
                    (get-in word [:punctuation :middle]))
               (get-string (get-in word '(:b)) :show-notes show-notes :lexicon lexicon :from-language from-language)])))

    ;; TODO: this seems wrong: how could :infl == :english?
    (and (= :english (get-in word '(:infl)))
         (string? (get-in word '(:english))))
    (get-in word '(:english))
    
    (= true (get-in word '(:hidden)))
    ;;   "Ø"
    ""

    (and
     (= true (get-in word '(:a :hidden)))
     (= true (get-in word '(:b :hidden))))
    ;;   "Ø"
    ""

    (= true (get-in word '(:a :hidden)))
    (get-string (get-in word '(:b))
                :from-language from-language
                :show-notes show-notes :lexicon lexicon)

    (= true (get-in word '(:b :hidden)))
    (get-string (get-in word '(:a))
                :from-language from-language
                :show-notes show-notes :lexicon lexicon)

    (and (= (get-in word '(:infl)) :conditional)
         (get-in word '(:english))
         (not (nil? (get-in word '(:agr :number))))
         (not (nil? (get-in word '(:agr :person))))
         (string? (get-in word [:conditional])))
    (str "would " (get-in word [:conditional]))

    (and (= (get-in word '(:infl)) :conditional)
         (get-in word '(:english))
         (not (nil? (get-in word '(:agr :number))))
         (not (nil? (get-in word '(:agr :person)))))
    (let [infinitive (get-in word '(:english))
          stem (string/replace infinitive #"^to " "")]
      (str "would " stem))

    (and (= (get-in word '(:infl)) :future)
         (get-in word '(:english))
         (not (nil? (get-in word '(:agr :number))))
         (not (nil? (get-in word '(:agr :person))))
         (string? (get-in word [:future])))
    (str "will " (get-in word [:future]))

    (and (= (get-in word '(:infl)) :future)
         (get-in word '(:english))
         (not (nil? (get-in word '(:agr :number))))
         (not (nil? (get-in word '(:agr :person)))))
    (let [infinitive (get-in word '(:english))
          stem (replace infinitive #"^to " "")]
      (str "will " stem))

    ;; <irregular imperfect>
    (let [number (get-in word '(:agr :number))
          person (get-in word '(:agr :person))]
      (and (= (get-in word [:infl]) :imperfect)
           (string? (get-in word [:imperfect :1sing]))
           (and (= person :1st) (= number :sing))))
    (get-in word [:imperfect :1sing])

    (let [number (get-in word '(:agr :number))
          person (get-in word '(:agr :person))]
      (and (= (get-in word [:infl]) :imperfect)
           (string? (get-in word [:imperfect :2sing]))
           (and (= person :2nd) (= number :sing))))
    (get-in word [:imperfect :2sing])

    (let [number (get-in word '(:agr :number))
          person (get-in word '(:agr :person))]
      (and (= (get-in word [:infl]) :imperfect)
           (string? (get-in word [:imperfect :3sing]))
           (and (= person :3rd) (= number :sing))))
    (get-in word [:imperfect :3sing])

    (let [number (get-in word '(:agr :number))
          person (get-in word '(:agr :person))]
      (and (= (get-in word [:infl]) :imperfect)
           (string? (get-in word [:imperfect :1plur]))
           (and (= person :1st) (= number :plur))))
    (get-in word [:imperfect :1plur])

    (let [number (get-in word '(:agr :number))
          person (get-in word '(:agr :person))]
      (and (= (get-in word [:infl]) :imperfect)
           (string? (get-in word [:imperfect :2plur]))
           (and (= person :2nd) (= number :plur))))
    (get-in word [:imperfect :2plur])

    (let [number (get-in word '(:agr :number))
          person (get-in word '(:agr :person))]
      (and (= (get-in word [:infl]) :imperfect)
           (string? (get-in word [:imperfect :3plur]))
           (and (= person :3rd) (= number :plur))))
    (get-in word [:imperfect :3plur])
    ;; </irregular imperfect>
    
    ;; regular imperfect
    (and (= (get-in word '(:infl)) :imperfect)
         (get-in word '(:english)))
    (let [infinitive (get-in word '(:english))
          stem (replace infinitive #"^to " "")
          to-final (re-find #" to$" stem) ;; occurs in e.g. "have to": in imperfect becomes "was having to"
          stem (replace stem #" to$" "")
          stem-minus-one (nth (re-find #"(.*).$" stem) 1)
          penultimate-stem-char (nth (re-find #"(.).$" stem) 1)
          penultimate-stem-char-is-vowel (or (= penultimate-stem-char "a")
                                             (= penultimate-stem-char "e")
                                             (= penultimate-stem-char "i")
                                             (= penultimate-stem-char "o")
                                             (= penultimate-stem-char "u"))
          last-stem-char (re-find #".$" stem)
          last-stem-char-is-e (re-find #"e$" stem)]
      ;; remove final "e", if any, before adding "e": e.g. "write" => "writing"
      (let [root stem ;; save this since we don't remove final 'e' for "used to" form.

            ;; unless overridden by :participle or :participle-suffix below,
            ;; ing-form or used-to-form (chosen randomly) will be used: see the (rand-int) below
            ing-form (present-participle-of stem)

            used-to-form
            (str "used to " root)]
        (cond (= 0 (rand-int 2))
              used-to-form

              true
              (cond ;; imperfect exception-handling: check for :participle
                
                ;; TODO: add support for per-agreement (by number or person) irregular participle;
                ;; for now, only support for a single participle irregular form for all agreements.
                ;; (might not be needed for english)
                
                ;; 2) use irregular that is the same for all number and person if there is one.
                (and (string? (get-in word '(:participle)))
                     (= :sing (get-in word '(:agr :number)))
                     (or (= :1st (get-in word '(:agr :person)))
                         (= :3rd (get-in word '(:agr :person))))
                     (string? (get-in word '(:participle))))
                (str "was " (get-in word '(:participle)))
                
                (string? (get-in word '(:participle)))
                (str "were " (get-in word '(:participle)))
                
                (and (= :sing (get-in word '(:agr :number)))
                     (or (= :1st (get-in word '(:agr :person)))
                         (= :3rd (get-in word '(:agr :person))))
                     (string? (get-in word '(:participle-suffix))))
                (str "was " (get-in word '(:participle-suffix)))
                
                (string? (get-in word '(:participle-suffix)))
                (str "were " (get-in word '(:participle-suffix)))
                
                (and (= :sing (get-in word '(:agr :number)))
                     (or (= :1st (get-in word '(:agr :person)))
                         (= :3rd (get-in word '(:agr :person)))))
                (str "was " ing-form (if to-final to-final ""))
                
                true
                (str "were " ing-form (if to-final to-final ""))))))
    
    ;; irregular past (1): a single inflection for all persons/numbers.
    (and (= :past (get-in word '(:infl)))
         (string? (get-in word '(:past))))
    (get-in word '(:past))
    
    ;; irregular pluperfect
    (and (= :pluperfect (get-in word '(:infl)))
         (string? (get-in word '(:past-participle))))
    (str "had " (get-in word '(:past-participle)))

    ;; regular pluperfect
    (and (= :pluperfect (get-in word '(:infl)))
         (nil? (get-in word '(:past-participle))))
    (str "had " (get-string (merge word
                                   {:infl :past})
                            :from-language from-language
                            :lexicon lexicon
                            :show-notes show-notes))

    (and (= :past (get-in word '(:infl)))
         (= :top (get-in word '(:agr :number)))
         (string? (get-in word '(:past :2sing))))
    ;; use the 2nd singular form if there's not enough inflection info to go on.
    (str "[" (get-in word '(:past :2sing)) "]")

    (= :top (get-in word '(:infl)))
    (get-in word '(:english))

    ;; irregular past (2): a different inflection for each persons/numbers.
    (and (= :past (get-in word '(:infl)))
         (map? (get-in word '(:past))))
    (let [number (get-in word '(:agr :number))
          person (get-in word '(:agr :person))]
      (cond (and (= person :1st) (= number :sing)
                 (string? (get-in word '(:past :1sing))))
            (get-in word '[:past :1sing])

            (and (= person :2nd) (= number :sing)
                 (string? (get-in word [:past :2sing])))
            (get-in word '[:past :2sing])
            
            (and (= person :3rd) (= number :sing)
                 (string? (get-in word [:past :3sing])))
            (get-in word '(:past :3sing))

            (and (= person :1st) (= number :plur)
                 (string? (get-in word [:past :1plur])))
            (get-in word '(:past :1plur))

            (and (= person :2nd) (= number :plur)
                 (string? (get-in word [:past :2plur])))
            (get-in word '(:past :2plur))

            (and (= person :3rd) (= number :plur)
                 (string? (get-in word [:past :3plur])))
            (get-in word '(:past :3plur))

            ;; default
            (string? (get-in word [:past :english]))
            (str (get-in word [:past :english])
                 (if (and (not (= false show-notes))
                          (string? (get-in word [:past :note])))
                   (str " (" (trim (get-in word '(:past :note))) ")")))
            
            true word)) ;; not enough agreement specified to conjugate.

    ;; regular past
    (and (= :past (get-in word '(:infl)))
         (string? (get-in word '(:english))))
    (let [infinitive (get-in word '(:english))
          stem (replace infinitive #"^to " "")
          stem-minus-one (nth (re-find #"(.*).$" stem) 1)
          penultimate-stem-char (nth (re-find #"(.).$" stem) 1)
          last-stem-char (re-find #".$" stem)
          last-stem-char-is-e (re-find #"e$" stem)
          last-stem-char-is-y (re-find #"y$" stem)]
      (cond last-stem-char-is-e
            (str stem-minus-one "ed")   ;; "bake"->"baked"

            (and last-stem-char-is-y
                 (or (= penultimate-stem-char "l")
                     (= penultimate-stem-char "n")
                     (= penultimate-stem-char "r")))
            (str stem-minus-one "ied")  ;; "try"->"tried"

            true
            (str stem "ed"))) ;; "play"->"played"   

    ;; simple present
    (and
     (= :present (get-in word '(:infl)))
     (string? (get-in word '(:english))))
    (let [root (get-in word '(:english))
          ;; TODO: throw exception rather than encoding error "(no root)" as part
          ;; of the english string.
          root (if (nil? root) "(no root)" root)
          root (if (not (string? root))
                 (get-in word '(:english :english))
                 root)
          person (get-in word '(:agr :person))
          number (get-in word '(:agr :number))
          stem (replace root #"^to " "")
          last-stem-char-is-e (re-find #"e$" stem)
          penultimate-stem-char-is-vowel (re-find #"[aeiou].$" stem)
          last-stem-char-is-vowel (re-find #"[aeiou]$" stem)]
      (log/debug "+else")
      (log/debug (str "(english):word: " word))
      (cond
        (and (= person :1st) (= number :sing)
             (string? (get-in word '(:present :1sing))))
        (get-in word '(:present :1sing))

        (and (= person :2nd) (= number :sing)
             (string? (get-in word '(:present :2sing))))
        (get-in word '(:present :2sing))

        (and (= person :3rd) (= number :sing)
             (string? (get-in word '(:present :3sing))))
        (get-in word '(:present :3sing))

        (and (= person :1st) (= number :plur)
             (string? (get-in word '(:present :1plur))))
        (get-in word '(:present :1plur))
        (and (= person :2nd) (= number :plur)
             (string? (get-in word '(:present :2plur))))
        (get-in word '(:present :2plur))
        (and (= person :3rd) (= number :plur)
             (string? (get-in word '(:present :3plur))))
        (get-in word '(:present :3plur))

        (and (= person :1st) (= number :sing))
        (str stem "")

        (and (= person :2nd) (= number :sing))
        (str stem "")

        (and (= person :3rd) (= number :sing)
             (re-find #"[dlnr]y$" stem))
        (let [stem-minus-final-y (replace root #"y$" "")]
          (str stem-minus-final-y "ies"))

        (and (= person :3rd) (= number :sing)
             (re-find #"[cs][sh]$" stem))
        (str stem "es")

        (and (= person :3rd) (= number :sing)
             (re-find #"o$" stem))
        (str stem "es")

        ;; default 3rd sing inflection: just add s.
        (and (= person :3rd) (= number :sing))
        (str stem "s")

        (and (= person :1st) (= number :plur))
        (str stem "")

        (and (= person :2nd) (= number :plur))
        (str stem "")

        (and (= person :3rd) (= number :plur))
        (str stem "")

        (string? (get-in word '(:english)))
        (get-in word '(:english))

        (string? (get-in word '(:english)))
        (get-in word '(:english))

        :else (str root)))

    ;; conjugate irregular present progressive e.g. "I am getting dressed"
    (and
     (= :verb (get-in word [:cat]))
     (= :present-progressive (get-in word [:infl]))
     (not (nil? lexicon))
     (not (nil? (get-in word [:participle]))))
    (let [to-be
          (-> lexicon (get "be")
              (nth 0) ;; there should be at least one, so take the first one.
              (get-in [:english]))
          to-be-present-tense-with-agreement
          (-> to-be
              (unify {:cat :verb
                       :infl :present}
                      {:agr (get-in word [:agr])}))]
      (log/debug (str "lexical entry for be:" (strip-refs to-be)))
      (log/debug (str "to-be-present-tense-with-agreement:" (strip-refs to-be-present-tense-with-agreement)))
      (str
       (get-string to-be-present-tense-with-agreement
                   :from-language from-language)
       " " (get-in word [:participle])
       (if show-notes (str " (" present-participle-note ")"))))

    ;; conjugate regular present progressive ("be + Ving") e.g. "I am eating"
    (and
     (= :verb (get-in word [:cat]))
     (= :present-progressive (get-in word [:infl]))
     (not (nil? lexicon)))
    ;; take present tense of verb "be" and add the present participle of this word.
    (let [to-be
          (-> lexicon (get "be")
              (nth 0) ;; there should be at least one, so take the first one.
              (get-in [:english]))
          to-be-present-tense-with-agreement
          (-> to-be
              (unify {:cat :verb
                      :infl :present}
                     {:agr (get-in word [:agr])}))]
      (log/debug (str "lexical entry for be:" (strip-refs to-be)))
      (log/debug (str "to-be-present-tense-with-agreement:" (strip-refs to-be-present-tense-with-agreement)))
      (str
       (get-string to-be-present-tense-with-agreement
                   :from-language from-language)
       " " (present-participle-of (get-in word [:english]))
       (if show-notes (str " (" present-participle-note ")"))))
    
    (= :verb (get-in word [:cat]))
    (let [result (get-in word [:english])]
      (log/warn (str "catch-all for :cat=:verb : nothing more specific matches the input:"
                     (strip-refs word) " : returning: " result))
      result)
    
    (and
     (get-in word '(:plur))
     (= (get-in word '(:agr :number)) :plur)
     (= (get-in word '(:cat) :noun)))
    (get-in word '(:plur))

    ;; TODO: remove support for deprecated :root - use :plur instead (as immediately above).
    (and
     (get-in word '(:root :plur))
     (= (get-in word '(:agr :number)) :plur)
     (= (get-in word '(:cat) :noun)))
    (get-in word '(:root :plur))

    ;; TODO: remove support for deprecated :root - use :sing instead.
    (and
     (= (get-in word '(:agr :number)) :sing)
     (= (get-in word '(:cat) :noun))
     (string? (get-in word '(:root))))
    (get-in word '(:root))
    
    (and
     (= (get-in word '(:agr :number)) :sing)
     (= (get-in word '(:cat) :noun))
     (string? (get-in word '(:english))))
    (trim (str (get-in word '(:english)) " "
               (if (and (get-in word '(:note))
                        (not (= false show-notes)))
                 (get-in word '(:note)))))
    
    ;; TODO: remove support for deprecated :root - use :sing instead.
    (and
     (= (get-in word '(:agr :number)) :sing)
     (= (get-in word '(:cat) :noun))
     (string? (get-in word '(:root :english))))
    (get-in word '(:root :english))
    
    (and
     (= (get-in word '(:agr :number)) :sing)
     (= (get-in word '(:cat) :noun))
     (string? (get-in word '(:english :english))))
    (str
     (trim (get-in word '(:english :english))) " "
     (if (get-in word '(:note))
       (trim (get-in word '(:note)))))

    ;; TODO: remove support for deprecated :root - use :plur instead.
    (and (= (get-in word '(:agr :number)) :plur)
         (= (get-in word '(:cat)) :noun)
         (string? (get-in word '(:root))))
    (str (get-in word '(:root)) "s")

    (and (= (get-in word '(:agr :number)) :plur)
         (= (get-in word '(:cat)) :noun)
         (string? (get-in word '(:english)))
         (not (= (get-in word [:pronoun]) true))
         (not (= (get-in word [:propernoun]) true)))
    (str (plural-en (get-in word '(:english)) word)
         (if (get-in word '(:note))
           (str (get-in word '(:note)))))

    (and (= (get-in word '(:agr :number)) :plur)
         (= (get-in word '(:cat)) :noun)
         (string? (get-in word '(:english :english)))
         (not (= (get-in word [:pronoun]) true))
         (not (= (get-in word [:propernoun]) true)))
    (str (plural-en (get-in word '(:english :english) word))
         (if (get-in word '(:english :note))
           (str (get-in word '(:english :note)))))

    (and (= (get-in word '(:cat)) :adjective)
         (string? (get-in word '(:english))))
    (get-in word '(:english))

    (string? (get-in word '(:english)))
    (get-in word '(:english))

    ;; TODO: not sure if this code is alive or not: is there ever
    ;; a case of a sign with '(:english :english :english)?
    (and (string? (get-in word '(:english :english)))
         (= (.size (keys word)) 1))
    (get-in word '(:english :english))

    (string? (get-in word '(:english)))
    (get-in word '(:english))

    :else
    word))

(defn remove-to [english-verb-phrase]
  (let [english (get english-verb-phrase :english)]
    (let [regex #"^to[ ]+(.*)"]
      (let [string
            (replace english regex (fn [[_ rest]] (str rest)))]
        (merge
         {:remove-to string}
         english-verb-phrase)))))

(defn plural-en [english & [word]]
  (log/debug (str "plural-en: english: " english "; word: " (strip-refs word)))
  (cond
    ;; handle commas in english word, e.g.: "agent,officer" => "agents,officers"
    (re-find #"," english)
    
    (let [divided-by-commas (filter #(not (empty? %))
                                    (string/split english #"[\s+,\s+]"))]
      (string/join ", " (map #(plural-en % word) divided-by-commas)))

    ;; wolf => wolves; life => lives
    (re-find #"fe?$" english) 
    (replace english #"fe?$" "ves")
    
    ;; city => cities
    (re-find #"[^aeiou]y$" english) 
    (replace english #"([^aeiou])y$" "$1ies")

    ;; crisis => crises
    (re-find #"i[s]$" english)
    (replace english #"is$" "es")
    
    (re-find #"[cs][hsx]$" english) ;; brush => brushes; beach => beaches
    (str english "es")

    ;; found in lexical entries derived from vocab-items where
    ;; the vocab-cat is "nounplurf" or "nounplurm".
    (and word (= true (get-in word [:inherently-plural])))
    (str english)
    
    ;; default case.
    true
    (str english "s")))

(def gender-symbol-female
  {#" \(♀\)$"
   {:replace-with ""
    :unify-with {:synsem {:cat :noun
                          :agr {:gender :fem}}}}})

(def gender-symbol-male
  {#" \(♂\)$"
   {:replace-with ""
    :unify-with {:synsem {:cat :noun
                          :agr {:gender :fem}}}}})

;; TODO: do other unicode entities/emojis like soccer ball and music staff.

(def plural-to-singular-noun
  {#"s$"
   {:replace-with ""
    :unify-with {:synsem {:cat :noun
                          :agr {:number :plur}}}}})

(def infinitive-to-infinitive
  {:identity1
   {:unify-with {:synsem {:cat :verb
                          :infl :infinitive}}}})

(def infinitive-to-1sing-present ;; infinitive "read" -> "i read"
  {:identity2
   {:unify-with {:synsem {:cat :verb
                          :infl :present
                          :subcat {:1 {:agr {:number :sing
                                             :person :1st}}}}}}})

(def infinitive-to-2sing-present ;; infinitive "read" -> "you read"
  {:identity3
   {:unify-with {:synsem {:cat :verb
                          :infl :present
                          :subcat {:1 {:agr {:number :sing
                                             :person :2nd}}}}}}})

(def infinitive-to-3sing-present ;; infinitive "read" -> "she reads"
  {#"s$"
   {:replace-with ""
    :unify-with {:synsem {:cat :verb
                          :infl :present
                          :subcat {:1 {:agr {:number :sing
                                             :person :3rd}}}}}}})
(def infinitive-to-future
  {#"^will "
   {:replace-with ""
    :unify-with {:synsem {:cat :verb
                          :infl :future}}}})

(def infinitive-to-conditional
  {#"^would "
   {:replace-with ""
    :unify-with {:synsem {:cat :verb
                          :infl :conditional}}}})

(def infinitive-to-imperfect1
  {#"^used to "
   {:replace-with ""
    :unify-with {:synsem {:cat :verb
                          :infl :imperfect}}}})

(def infinitive-to-imperfect2
  {#"^(was|were) (.*)ing" ;; posting -> post
   {:replace-with "$2"
    :unify-with {:synsem {:cat :verb
                          :infl :imperfect}}}})

(def infinitive-to-imperfect3 ;; flaming -> flame, causing -> cause, tasting -> taste
  {#"^(was|were) (.*[mstv])ing"
   {:replace-with "$2e"
    :unify-with {:synsem {:cat :verb
                          :infl :imperfect}}}})

(def infinitive-to-imperfect4 ;; tripping -> trip
  {#"^(was|were) (.*)[nsp]ing"
   {:replace-with "$2"
    :unify-with {:synsem {:cat :verb
                          :infl :imperfect}}}})

(def infinitive-to-past-1
  {#"d$"
   {:replace-with ""
    :unify-with {:synsem {:cat :verb
                          :infl :past}}}})

(def infinitive-to-past-2
  {#"ed$"
   {:replace-with ""
    :unify-with {:synsem {:cat :verb
                          :infl :past}}}})

(def infinitive-to-1plur-present ;; infinitive "read" -> "we read"
  {:identity4
   {:unify-with {:synsem {:cat :verb
                          :infl :present
                          :subcat {:1 {:agr {:number :plur
                                             :person :1st}}}}}}})

(def infinitive-to-2plur-present ;; infinitive "read" -> "you all read"
  {:identity5
   {:unify-with {:synsem {:cat :verb
                          :infl :present
                          :subcat {:1 {:agr {:number :plur
                                             :person :2nd}}}}}}})

(def infinitive-to-3plur-present ;; infinitive "read" -> "they read"
  {:identity6
   {:replace-with ""
    :unify-with {:synsem {:cat :verb
                          :infl :present
                          :subcat {:1 {:agr {:number :plur
                                             :person :3rd}}}}}}})

(def lexical-noun-to-singular
  {:identity
   {:unify-with {:synsem {:cat :noun
                          :agr {:number :sing}}}}})

(defn analyze [surface-form lexicon]
  "return the map incorporating the lexical information about a surface form."
  (let [replace-pairs
        (merge 
         gender-symbol-female
         gender-symbol-male
         plural-to-singular-noun
         infinitive-to-infinitive
         infinitive-to-1sing-present
         infinitive-to-2sing-present
         infinitive-to-3sing-present

         infinitive-to-1plur-present
         infinitive-to-2plur-present
         infinitive-to-3plur-present

         lexical-noun-to-singular ;; turns :number :top to :number :sing

         infinitive-to-conditional ;; "would love" => love
         infinitive-to-future ;; "will love" => love
         infinitive-to-imperfect1 ;; "used to love" => love
         infinitive-to-imperfect2 ;; "was posting" => post
         infinitive-to-imperfect3 ;; "was causing" => cause
         infinitive-to-imperfect4 ;; "was stopping" => stop
         
         infinitive-to-past-1 ;; love => loved
         infinitive-to-past-2 ;; talked => talk
         
         )
        
        analyzed
        (remove fail?
                (mapcat
                 (fn [key]
                   (if (and (not (keyword? key)) (re-find key surface-form))
                     (let [lexical-form (string/replace surface-form key
                                                        (:replace-with (get replace-pairs key)))
                           looked-up (get lexicon lexical-form)]
                       (map #(unify % (:unify-with (get replace-pairs key)))
                            looked-up))))
                 (keys replace-pairs)))

        analyzed-via-identity
        (remove fail?
                (mapcat
                 (fn [key]
                   (if (keyword? key)
                     (let [lexical-form surface-form
                           looked-up (get lexicon lexical-form)]
                       (map #(unify % (:unify-with (get replace-pairs key)))
                            looked-up))))
                 (keys replace-pairs)))]

    ;; rethink how we retrieve lexical entries here: should we
    ;; do additional analysis via analyzed-via-identity, or just look up directly
    ;; with (get lexicon surface-form)? also, (set) is expensive due to need
    ;; to check for duplicates.
    (set
     (concat
      (or analyzed analyzed-via-identity)

      ;; also lookup the surface form itself, which
      ;; might be either the canonical form of a word, or an irregular conjugation of a word.

      ;; In english, the canonical form of a verb looks the same as the 1st and 2nd person present tense,
      ;; (regardless of number), and the same as the 3rd person plural present tense; e.g.:
      ;; "sleep" is the canonical form, which is used in: "I sleep","you sleep","you all sleep",
      ;; "they sleep","we sleep". The following causes canonical forms to be expanded to all of
      ;; these. We don't want to simply leave it as an unconjugated, canonical form, as this
      ;; will lead to a profusion of unintended parses (e.g. "sleep" will be treated as past, present, and
      ;; future, when it is only meant as one of the above-described present forms.
      (->> (get lexicon surface-form)
           (mapcat (fn [entry]
                     (map (fn [variant]
                            (let [result
                                  (unify entry variant)]
                              (if (not (fail? result))
                                result
                                entry)))
                          [{:synsem {:cat :verb
                                     :infl :present
                                     :subcat {:1 {:agr {:person :1st}}}}}
                           {:synsem {:cat :verb
                                     :infl :present
                                     :subcat {:1 {:agr {:person :2nd}}}}}
                           {:synsem {:cat :verb
                                     :infl :present
                                     :subcat {:1 {:agr {:number :plur
                                                        :person :3rd}}}}}]))))))))




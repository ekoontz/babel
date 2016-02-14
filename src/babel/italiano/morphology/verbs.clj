(ns babel.italiano.morphology.verbs)

(def replace-patterns-present-tense
  [
   {:p [#"^([^' ]+)o$"         "$1are"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :1st}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)o$"         "$1ere"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :1st}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)isco$"      "$1ire"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :1st}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)o$"         "$1ire"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :1st}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)o$"         "$1arsi"] ;; alzo -> alzarsi
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :1st}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)o$"         "$1irsi"] ;; diverto -> divertirso
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :1st}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)ico$"       "$1ire"] ;; dico -> dire
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :1st}}}
                 :infl :present}}}
   ;; 2nd sing
   {:p [#"^([^' ]+)i$"         "$1are"] ;; lavi -> lavare
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)i$"         "$1iare"] ;; studi -> studiare
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)i$"         "$1arsi"] ;; lavi -> lavarsi
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)cci$"       "$1cciare"] ;; abbracci -> abbracciare
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)i$"         "$1ere"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)i$"         "$1ire"] ;; senti -> sentire
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+c)hi$"       "$1are"] ;; cerchi -> cercare
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}
        
   {:p [#"^([^' ]+)i$"         "$1iarsi"] ;; arrabi -> arrabiarsi
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)sci$"       "$1re"] ;; finisci -> finire
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)i$"         "$1irsi"] ;; diverti -> divertirsi
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)ici$"       "$1ire"] ;; dici -> dire
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)hi$"        "$1are"] ;; pieghi -> piegare
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :2nd}}}
                 :infl :present}}}

   {:p [#"^([^' ]+)a$"         "$1are"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)e$"         "$1ere"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)e$"         "$1ire"]
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)a$"         "$1arsi"] ;; prepara -> preperarsi
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)sce$"       "$1re"] ;; finisce -> finire
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)te$"        "$1tirsi"] ;; diverte -> divertirsi
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)ice$"      "$1ire"] ;; dice -> dire
    :u {:synsem {:subcat {:1 {:agr {:number :sing
                                    :person :3rd}}}
                 :infl :present}}}
   
   {:p [#"^([^' ]+)iamo$"      "$1are"]  ;; parliamo -> parlare
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}

   {:p [#"^([^' ]+)iamo$"      "$1iare"] ;; mangiamo -> mangiare
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)iamo$"      "$1ere"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)iamo$"      "$1ire"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}
   {:p [#"^([^' ]+c)hiamo$"    "$1are"] ;; sprechiamo -> sprecare
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)iamo$"      "$1iarsi"] ;; arrabiamo -> arrabiarsi
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)iamo$"      "$1arsi"] ;; chiamiamo -> chiamarsi
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)iamo$"      "$1irsi"] ;; divertiamo -> divertirsi
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)ciamo$"     "$1re"] ;; diciamo -> dire
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)hiamo$"     "$1are"] ;; pieghiamo -> piegare
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}
   ;; 2nd plur
   {:p [#"^([^' ]+)([aei])te$" "$1$2re"] ;; parlate -> parlare; leggere -> leggere
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)([aei])te$" "$1$2rsi"] ;; chiamate -> chiamarsi
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :2nd}}}
                 :infl :present}}}

   {:p [#"^([^' ]+)ano$"       "$1are"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)ono$"       "$1ere"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)ono$"       "$1ire"]
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}
                 :infl :present}}}

   {:p [#"^([^' ]+)ano$"       "$1arsi"] ;; alzano -> alzarsi
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)scono$"     "$1re"] ;; finiscono -> finire
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)ono$"       "$1irsi"] ;; divertono -> divertirsi
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :1st}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)cono$"      "$1re"] ;; dicono -> dire
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}
                 :infl :present}}}
   {:p [#"^([^' ]+)ono$"       "$1irsi"] ;; vestono -> vestirsi
    :u {:synsem {:subcat {:1 {:agr {:number :plur
                                    :person :3rd}}}
                 :infl :present}}}
   ])

(def replace-patterns
  (concat
   replace-patterns-present-tense))

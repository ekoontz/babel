(ns babel.italiano.morphology.articles)

;; TODO: unify with (babel.italiano.morphology/conjugate-italian-prep)
(def l-apostrophe
  {

   #"^l$"
   {:replace-with "la"
    :unify-with :top} ;; TODO: unify with :cat :determiner, :def :def

   #"^l$"
   {:replace-with "il"
    :unify-with :top} ;; TODO  unify with :cat :determiner, :def :def
   
   #"^gli$"
   {:replace-with "i"
    :unify-with :top} ;; TODO unify with :cat :determiner, :def :def

   }
  )




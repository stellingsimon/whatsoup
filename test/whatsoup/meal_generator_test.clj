(ns whatsoup.meal-generator-test
  (:require [clojure.spec.alpha :as spec]
            [clojure.test :refer :all]
            [whatsoup.meal-generator :as mg :refer :all]))

; TODO: (2017-07-15, sst) leverage spec to generate arbitrary recipes for us
(def ex-recipe
  {:recipe/name        "Püree-Suppe"
   :recipe/ingredients [[:= :food/lauch]
                        [:= :food/zwiebel]
                        [:= :food/bouillon]
                        ["Püree-Basis"
                         := [:all-of :property/gemüse :property/stärkehaltig]]
                        ["Weitere Zutaten"
                         :* [:any-of :property/gemüse :property/fleisch]]
                        ["Einlage" :* :property/knusprig]]})

(deftest -normalize-constraint
  (is (= {:op :all-of :elems [:food/bouillon]}
         (normalize-constraint (spec/conform ::mg/constraint :food/bouillon))))
  (is (= {:op :all-of :elems [:property/gemüse :property/saison-jun]}
         (normalize-constraint (spec/conform ::mg/constraint [:all-of :property/gemüse :property/saison-jun]))))
  (is (= {:op :any-of :elems [:food/lauch :food/zwiebel]}
         (normalize-constraint (spec/conform ::mg/constraint [:any-of :food/lauch :food/zwiebel])))))

(deftest -ingredients
  (is (every? #(contains? #{:any-of :all-of} (get-in % [:constraint :op]))
              (ingredients (spec/conform ::mg/recipe ex-recipe)))
      "constraint-normalization performed")
  (is (= (range (count (:recipe/ingredients ex-recipe)))    ; (0 1 2 ...)
         (map :idx (ingredients (spec/conform ::mg/recipe ex-recipe))))
      "idx-assignment starting at 0 counting up"))

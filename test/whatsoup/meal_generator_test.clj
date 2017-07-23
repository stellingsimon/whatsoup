(ns whatsoup.meal-generator-test
  (:require [clojure.spec.alpha :as spec]
            [clojure.test :refer :all]
            [com.stuartsierra.component :as component]
            [whatsoup.meal-generator :as mg :refer :all]
            [whatsoup.food-kb :as kb]
            [whatsoup.food-kb-test :refer [test-kb]]
            [whatsoup.test-util :refer :all]))

; TODO: (2017-07-15, sst) leverage spec to generate arbitrary recipes for us
(def ex-recipe
  {:recipe/name        "Püree-Suppe"
   :recipe/ingredients [[:= :food/lauch]
                        [:= :food/zwiebel]
                        [:= :food/bouillon]
                        ["Püreebasis"
                         := [:all-of :property/gemüse :property/stärkehaltig]]
                        ["Weitere Zutaten"
                         :* [:any-of :property/gemüse :property/fleisch]]
                        ["Einlage" :* :property/knusprig]]})
(def ex-meal
  {:meal/name        "Püree-Suppe"
   :meal/ingredients [["" :food/lauch]
                      ["" :food/zwiebel]
                      ["" :food/bouillon]
                      ["Püreebasis" :food/kartoffel]
                      ["Weitere Zutaten" :food/schweinefleisch]
                      ["Einlage"]]})

(def test-mg
  ; TODO: (2017-07-16, sst) component/start probably not warranted here.
  (component/start
    (->MealGenerator test-kb first)))

(def empty-mg (->MealGenerator {} first))

(deftest -normalize-constraint
  (is (= {:op :all-of :elems [:food/bouillon]}
         (normalize-constraint (spec/conform ::mg/constraint :food/bouillon))))
  (is (= {:op :all-of :elems [:property/gemüse :property/saison-jun]}
         (normalize-constraint (spec/conform ::mg/constraint [:all-of :property/gemüse :property/saison-jun]))))
  (is (= {:op :any-of :elems [:food/lauch :food/zwiebel]}
         (normalize-constraint (spec/conform ::mg/constraint [:any-of :food/lauch :food/zwiebel])))))

(deftest -with-candidates
  (let [recipe (spec/conform ::mg/recipe ex-recipe)]
    (is (= (range (count (:recipe/ingredients ex-recipe)))  ; (0 1 2 ...)
           (map :idx (:recipe/ingredients (with-candidates test-mg recipe))))
        "idx-assignment starting at 0 counting up")
    (is (every?
          #(contains? #{:any-of :all-of} (get-in % [:constraint :op]))
          (:recipe/ingredients (with-candidates test-mg recipe)))
        "constraint-normalization performed")
    (is (= [#{:food/lauch}
              #{:food/zwiebel}
              #{:food/bouillon}
              #{:food/kartoffel}
              #{:food/broccoli
                :food/kartoffel
                :food/schweinefleisch}
              #{}]
           (mapv :candidate-foods (:recipe/ingredients (with-candidates test-mg recipe))))
        "candidates present")))

(deftest -satisfying-foods
  (let [all-of-constraint {:op :all-of :elems [:property/gemüse :property/stärkehaltig]}
        any-of-constraint {:op :any-of :elems [:property/fleisch :property/stärkehaltig]}]
    (is (= #{:food/kartoffel} (satisfying-foods test-mg all-of-constraint)))
    (is (= #{:food/schweinefleisch :food/kartoffel} (satisfying-foods test-mg any-of-constraint)))
    (is (thrown? RuntimeException (satisfying-foods test-mg :food/zwiebel))
        "report error on unnormalized simple constraint")
    (is (thrown? RuntimeException (satisfying-foods test-mg [:any-of :food/lauch :food/zwiebel]))
        "report error on unnormalized combined constraint")))

(deftest -match-ingredient
  (let [ingredients (->> (spec/conform ::mg/recipe ex-recipe)
                         (with-candidates test-mg)
                         (:recipe/ingredients))
        selected-foods #{:food/lauch}
        puree-base (first (filter #(= "Püreebasis" (:role %)) ingredients))
        others (first (filter #(= "Weitere Zutaten" (:role %)) ingredients))
        food-compatibility-matrix {[:food/broccoli :food/lauch]        0.1
                                   [:food/lauch :food/schweinefleisch] 5.0}]
    (is (= #{:food/kartoffel}
           (:selected-foods (match-ingredient empty-mg puree-base selected-foods)))
        "unambiguous selection works")
    (is (= #{:food/schweinefleisch}
           (:selected-foods (match-ingredient test-mg others selected-foods)))
        "food-compatibility-matrix lookup works")))

(deftest -match-ingredients
  (let [recipe (spec/conform ::mg/recipe ex-recipe)
        selected-foods #{:food/lauch}
        food-compatibility-matrix {[:food/broccoli :food/lauch]        0.1
                                   [:food/lauch :food/schweinefleisch] 5.0}]
    (is (= [#{:food/lauch}
            #{:food/zwiebel}
            #{:food/bouillon}
            #{:food/kartoffel}
            #{:food/schweinefleisch}
            #{}]                                            ; no match for :property/knusprig
           (map :selected-foods (:recipe/ingredients (match-ingredients test-mg recipe)))))))

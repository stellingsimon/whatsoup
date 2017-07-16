(ns whatsoup.meal-generator-test
  (:require [clojure.spec.alpha :as spec]
            [clojure.test :refer :all]
            [whatsoup.meal-generator :as mg :refer :all]
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
(def ex-food-catalog
  [{:food/name       :food/kartoffel
    :food/properties #{:property/gemüse :property/stärkehaltig}}
   {:food/name       :food/broccoli
    :food/properties #{:property/gemüse}}
   {:food/name       :food/schweinefleisch
    :food/properties #{:property/fleisch}}])


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


(deftest -resolve-food
  (let [property-catalog (property-catalog ex-food-catalog)]
    (is (= #{:food/zwiebel} (resolve-food property-catalog :food/zwiebel)))
    (is (= #{:food/kartoffel} (resolve-food property-catalog :property/stärkehaltig)))
    (is (= #{:food/broccoli :food/kartoffel} (resolve-food property-catalog :property/gemüse)))
    (is (= #{} (resolve-food property-catalog :property/inexistant)))
    (is (thrown? RuntimeException (resolve-food property-catalog :something-else)))))


(deftest -satisfying-foods
  (let [property-catalog (property-catalog ex-food-catalog)
        all-of-constraint {:op :all-of :elems [:property/gemüse :property/stärkehaltig]}
        any-of-constraint {:op :any-of :elems [:property/fleisch :property/stärkehaltig]}]
    (is (= #{:food/kartoffel} (satisfying-foods all-of-constraint property-catalog)))
    (is (= #{:food/schweinefleisch :food/kartoffel} (satisfying-foods any-of-constraint property-catalog)))
    (is (thrown? RuntimeException (satisfying-foods :food/zwiebel property-catalog))
        "report error on unnormalized simple constraint")
    (is (thrown? RuntimeException (satisfying-foods [:any-of :food/lauch :food/zwiebel] property-catalog))
        "report error on unnormalized combined constraint")))


(deftest -score
  (is (almost= 1.0 (score :food/broccoli #{} {})))
  (is (almost= 1.0 (score :food/broccoli #{:food/lauch} {})))
  (is (almost= 0.1 (score :food/broccoli #{:food/broccoli} {}))))


(deftest -match-ingredient
  (let [property-catalog (property-catalog ex-food-catalog)
        ingredients (-> (spec/conform ::mg/recipe ex-recipe)
                        (ingredients)
                        (with-candidates property-catalog))
        selected-foods #{:food/lauch}
        puree-base (first (filter #(= "Püreebasis" (:role %)) ingredients))
        others (first (filter #(= "Weitere Zutaten" (:role %)) ingredients))
        food-compatibility-matrix {[:food/broccoli :food/lauch]        0.1
                                   [:food/lauch :food/schweinefleisch] 5.0}]
    (is (= #{:food/kartoffel}
           (:selected-foods (match-ingredient puree-base selected-foods {})))
        "unambiguous selection works")
    (is (= #{:food/schweinefleisch}
           (:selected-foods (match-ingredient others selected-foods food-compatibility-matrix)))
        "food-compatibility-matrix lookup works")))


(deftest -match-ingredients
  (let [property-catalog (property-catalog ex-food-catalog)
        ingredients (-> (spec/conform ::mg/recipe ex-recipe)
                        (ingredients)
                        (with-candidates property-catalog))
        selected-foods #{:food/lauch}
        food-compatibility-matrix {[:food/broccoli :food/lauch]        0.1
                                   [:food/lauch :food/schweinefleisch] 5.0}]
    (is (= [#{:food/lauch}
            #{:food/zwiebel}
            #{:food/bouillon}
            #{:food/kartoffel}
            #{:food/schweinefleisch}
            #{}]                                            ; no match for :property/knusprig
           (map :selected-foods (match-ingredients ingredients property-catalog food-compatibility-matrix))))))


(deftest -meal
  (let [property-catalog (property-catalog ex-food-catalog)
        ingredients (-> (spec/conform ::mg/recipe ex-recipe)
                        (ingredients)
                        (with-candidates property-catalog))
        selected-foods #{:food/lauch}
        food-compatibility-matrix {[:food/broccoli :food/lauch]        0.1
                                   [:food/lauch :food/schweinefleisch] 5.0}]
    (is (= {:meal/name        "Püree-Suppe"
            :meal/ingredients [["Zutat(en)" :food/lauch]
                               ["Zutat(en)" :food/zwiebel]
                               ["Zutat(en)" :food/bouillon]
                               ["Püreebasis" :food/kartoffel]
                               ["Weitere Zutaten" :food/schweinefleisch]
                               ["Einlage"]]}
           (meal (:recipe/name ex-recipe)
                 ingredients
                 property-catalog
                 food-compatibility-matrix)))))
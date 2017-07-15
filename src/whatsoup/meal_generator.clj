(ns whatsoup.meal-generator
  "Generates a meal from a recipe, satisfying its constraints on the ingredients."
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as spec]
            [whatsoup.util :refer :all]))

;; Recipes mainly consist of an ordered list of ingredient descriptors.
;; These specify one or more `properties` that the ingredient food(s) needs to satisfy.
;; Alternatively, they may also just list acceptable food choices.
;; Quantities are deliberately omitted for the moment.
;;
;; Here's an example:
;;
#_(def ex-recipe
    {:recipe/name        "Püree-Suppe"
     :recipe/ingredients [[:= :food/lauch]
                          [:= :food/zwiebel]
                          [:= :food/bouillon]
                          ["Püree-Basis"
                           := [:all-of :property/gemüse :property/stärkehaltig]]
                          ["Weitere Zutaten"
                           :* [:any-of :property/gemüse :property/fleisch]]
                          ["Einlage" :* :property/knusprig]]})


(defn food? [x]
  (and (keyword? x) (= "food" (namespace x))))
(defn property? [x]
  (and (keyword? x) (= "property" (namespace x))))


(spec/def ::food food?)
(spec/def ::property property?)
(spec/def ::food-or-property
  (spec/or :food ::food
           :property ::property))
(spec/def ::constraint
  (spec/or :simple ::food-or-property
           :combined (spec/cat :op #(contains? #{:all-of :any-of} %)
                               :first ::food-or-property
                               :rest (spec/+ ::food-or-property))))
(spec/def ::ingredient
  (spec/cat :role (spec/? string?)
            :quantifier #(contains? #{:= :*} %)
            :constraint ::constraint))
(spec/def :recipe/ingredients (spec/coll-of ::ingredient))
(spec/def :recipe/name string?)
(spec/def ::recipe (spec/keys :req [:recipe/name :recipe/ingredients]))


;; Meals list their ingredient foods, which may be grouped by `role`.
;; Ingredients are listed in the same order as in the recipe.
;;
;; Here's an example:
;;
#_(def ex-out-meal
    {:meal/name        "Püree-Suppe"
     :meal/ingredients [["Zutat(en)" :food/lauch]
                        ["Zutat(en)" :food/zwiebel]
                        ["Zutat(en)" :food/bouillon]
                        ["Püreebasis" :food/kartoffel]
                        ["Weitere Zutaten" :food/karotten :food/broccoli]
                        ["Einlage" :food/crouton]]})


;; parse (conform) recipe using spec
;; extend recipe with algorithmic info, including properties->food mapping


(defn normalize-constraint
  "takes a spec-conformed constraint and returns a map with only :op and :elems"
  [[constraint-type detail :as constraint']]
  (cond
    (= :simple constraint-type)
    {:op    :all-of
     :elems [(second detail)]}

    (= :combined constraint-type)
    {:op    (:op detail)
     :elems (into [] (map second (concat [(:first detail)] (:rest detail))))}))


(defn ingredients [recipe]
  (->> (:recipe/ingredients recipe)
       (map #(update-in % [:constraint] normalize-constraint))
       (map-indexed #(merge {:idx %1 :role "Zutat(en)"} %2))))


(defn properties [food-def]
  (let [food (:food/name food-def)
        props (:food/properties food-def)]
    (apply merge (for [p props] {p #{food}}))))


(defn property-catalog [food-catalog]
  (apply merge-with set/union (map properties food-catalog)))


(defn resolve-food
  "maps properties to foods using the property-catalog and foods to themselves"
  [property-catalog food-or-property]
  (cond
    (food? food-or-property) (hash-set food-or-property)
    (property? food-or-property) (apply hash-set (get property-catalog food-or-property))
    :else (throw (RuntimeException. (str "Expected a :food or :property, got: " food-or-property)))))


(defn satisfying-foods
  "all foods from catalog that satisfy the given conformed constraint"
  [normalized-constraint property-catalog]
  (let [{:keys [op elems]} normalized-constraint
        food-sets (map #(resolve-food property-catalog %) elems)]
    (cond
      (= :all-of op) (apply set/intersection food-sets)
      (= :any-of op) (apply set/union food-sets)
      :else (throw (RuntimeException. (str "Unexpected op: " op))))))


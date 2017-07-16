(ns whatsoup.meal-generator
  "Generates a meal from a recipe, satisfying its constraints on the ingredients."
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as spec]
            [whatsoup.food-kb :as kb]
            [whatsoup.util :refer :all]))

;; Recipes mainly consist of an ordered list of ingredient descriptors.
;; These specify one or more `properties` that the ingredient food(s) needs to satisfy.
;; Alternatively, they may also just list acceptable food choices.
;; Quantities are deliberately omitted for the moment.
;;
;; For an example, refer to the ns meal-generator-test.



(spec/def ::food kb/food?)
(spec/def ::property kb/property?)
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


;; basic algorithm:
;; - compute candidates for each ingredient
;; - remove forbidden foods from the candidates
;; - for every mandatory food f:
;;   - choose a var where f is assignable
;;   - assign f to said var, remove it from candidates
;;   - list f as used food
;; - iterate until no assignable vars left:
;;   - choose an assignable var to assign next
;;   - from that var's candidates, pick a food f that's not yet used depending on some strategy
;;   - remove the picked food from candidates, add it to assigned-foods for that var


(defn satisfying-foods
  "all foods from catalog that satisfy the given conformed constraint"
  [kb normalized-constraint]
  (let [{:keys [op elems]} normalized-constraint
        food-sets (map #(kb/resolve-food kb %) elems)]
    (cond
      (= :all-of op) (apply set/intersection food-sets)
      (= :any-of op) (apply set/union food-sets)
      :else (throw (RuntimeException. (str "Unexpected op: " op))))))


(defn with-candidates
  "initializes the keys used in the computation of the food selection"
  [kb ingredients]
  (letfn [(candidates [ingredient]
            (satisfying-foods kb (:constraint ingredient)))
          (merge-candidates [ingredient]
            (merge {:selected-foods  #{}
                    :candidate-foods (candidates ingredient)} ingredient))]
    (mapv merge-candidates ingredients)))


(defn candidate-foods [ingredients] (mapcat :candidate-foods ingredients))
(defn selected-foods [ingredients] (mapcat :selected-foods ingredients))


(defn matchable?
  ([ingredient food]
   (and (matchable? ingredient)
        (contains? (:candidate-foods ingredient) food)))
  ([ingredient]
   (and (empty? (:selected-foods ingredient))
        (not (empty? (:candidate-foods ingredient))))))


(defn unambiguous-selection? [ingredient]
  (and (= 1 (count (:candidate-foods ingredient)))
       (matchable? ingredient)))


(defn select-food [ingredient food]
  (-> ingredient
      (update :selected-foods conj food)
      (update :candidate-foods disj food)))


(defn select-highest-scoring-food [kb ingredient selected-foods]
  (->> (:candidate-foods ingredient)
       (sort-by #(kb/score kb % selected-foods) #(> %1 %2))
       (first)                                              ; TODO: (2017-07-15, sst) randomize if several are best-matching
       (select-food ingredient)))


(defn match-ingredient [kb ingredient selected-foods]
  (if (unambiguous-selection? ingredient)
    (select-food ingredient (first (:candidate-foods ingredient)))
    (select-highest-scoring-food kb ingredient selected-foods)))


(defn next-matchable [ingredients]
  (first (filter matchable? ingredients)))                  ; TODO: (2017-07-15, sst) randomize order in which foods are fixed


(defn match-ingredients
  "matches up ingredients with foods that satisfy the given constraints and fit well with the other selected foods"
  [kb ingredients]
  (if-let [next-match (next-matchable ingredients)]
    (recur kb
           (as-> next-match ingredient
                 (match-ingredient kb ingredient (selected-foods ingredients))
                 (assoc ingredients (:idx next-match) ingredient)))
    ingredients))


(defn meal [kb name ingredients]
  (let [resulting-recipe (match-ingredients kb ingredients)]
    {:meal/name        name
     :meal/ingredients (mapv #(into [] (concat [(:role %)] (:selected-foods %))) resulting-recipe)}))
(ns whatsoup.meal-generator
  "Generates a meal from a recipe, satisfying its constraints on the ingredients."
  (:require [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.spec.alpha :as spec]
            [com.stuartsierra.component :as component]
            [whatsoup.food-kb :as kb]
            [whatsoup.util :refer :all]))

;; Recipes mainly consist of an ordered list of ingredient descriptors.
;; These specify one or more `properties` that the ingredient food(s) needs to satisfy.
;; Alternatively, they may also just list acceptable food choices.
;; Quantities are deliberately omitted for the moment.
;;
;; For an example, refer to the ns meal-generator-test.

(spec/def ::constraint
  (spec/or :simple ::kb/food-or-property
           :combined (spec/cat :op #(contains? #{:all-of :any-of} %)
                               :first ::kb/food-or-property
                               :rest (spec/+ ::kb/food-or-property))))
(spec/def ::ingredient
  (spec/cat :role (spec/? string?)
            :quantifier #(contains? #{:= :*} %)
            :constraint ::constraint))
(spec/def :recipe/ingredients (spec/coll-of ::ingredient))
(spec/def :recipe/name string?)
(spec/def ::recipe (spec/keys :req [:recipe/name :recipe/ingredients]))

(defrecord MealGenerator [food-kb picker config]
  component/Lifecycle

  (start [this]
    (let [recipe-catalog (:recipe-catalog config)]
      (assoc this :recipe-catalog recipe-catalog)
      (dissoc this :config)))

  (stop [this] this))

(defn create-meal-generator [config-file]
  (let [config (-> (slurp config-file)
                   (edn/read-string))]
    (-> (map->MealGenerator (select-keys config [:recipe-catalog]))
        (component/using [:food-kb :picker]))))

(defn normalize-constraint
  "takes a spec-conformed constraint and returns a map with only :op and :elems"
  [[constraint-type detail :as constraint']]
  (cond
    (= :simple constraint-type)
    {:op    :all-of
     :elems [(second detail)]}

    (= :combined constraint-type)
    {:op    (:op detail)
     :elems (mapv second (concat [(:first detail)] (:rest detail)))}))

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
  [mg normalized-constraint]
  (let [{:keys [op elems]} normalized-constraint
        food-sets (map #(kb/resolve-food (:food-kb mg) %) elems)]
    (cond
      (= :all-of op) (apply set/intersection food-sets)
      (= :any-of op) (apply set/union food-sets)
      :else (throw (RuntimeException. (str "Unexpected op: " op " in constraint: " normalized-constraint))))))

(defn inline-candidates
  "initializes the keys used in the computation of the food selection"
  [mg recipe]
  (letfn [(merge-idx [ingredients]
            (->> (map-indexed #(merge {:idx %1 :role ""} %2) ingredients)
                 (map #(update % :constraint normalize-constraint))))
          (merge-candidates [ingredients]
            (mapv #(merge {:selected-foods  #{}
                           :candidate-foods (satisfying-foods mg (:constraint %))} %) ingredients))]
    (-> recipe
        (update :recipe/ingredients merge-idx)
        (update :recipe/ingredients merge-candidates))))

(defn candidate-foods [recipe] (mapcat :candidate-foods (:recipe/ingredients recipe)))
(defn selected-foods [recipe] (mapcat :selected-foods (:recipe/ingredients recipe)))

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
      (update :candidate-foods disj food)
      (update :selected-foods conj food)))

(defn deselect-food [ingredient food]
  (-> ingredient
      (update :selected-foods disj food)
      (update :candidate-foods conj food)))

(defn deselect-all-foods [ingredient]
  (let [foods (:selected-foods ingredient)]
    (-> ingredient
        (assoc :selected-foods #{})
        (update :candidate-foods set/union foods))))

(defn sample-foods [scored-foods]
  (let [total-score (apply + (vals scored-foods))
        approx-sample-size 1000]
    (letfn [(sample-n [score]
              (int (/ (* approx-sample-size score) total-score)))
            (create-sample [[food score]]
              (repeat (sample-n score) food))]
      (mapcat create-sample (seq scored-foods)))))

(defn select-sampled-food [mg ingredient selected-foods]
  (->> (:candidate-foods ingredient)
       (index-with #(kb/score (:food-kb mg) % selected-foods))
       (sample-foods)
       (sort-by #(kb/score (:food-kb mg) % selected-foods) #(> %1 %2))
       ((:picker mg),,,)
       (select-food ingredient)))

(defn match-ingredient [mg ingredient selected-foods]
  (if (unambiguous-selection? ingredient)
    (select-food ingredient ((:picker mg) (:candidate-foods ingredient)))
    (select-sampled-food mg ingredient selected-foods)))

(defn next-matchable-ingredient [mg recipe]
  ((:picker mg) (filter matchable? (:recipe/ingredients recipe))))

(defn match-ingredients
  "matches up ingredients with foods that satisfy the given constraints and fit well with the other selected foods"
  ([mg recipe]
   (as-> recipe r
         (spec/conform ::recipe r)
         (inline-candidates mg r)
         (match-ingredients mg r 100)))
  ([mg recipe max-loops]
   (when (zero? max-loops)
     (throw (IllegalStateException. (str "aborted unsafe loop, recipe: " recipe))))
   (if-let [next-match (next-matchable-ingredient mg recipe)]
     (recur mg
            (as-> next-match ingredient
                  (match-ingredient mg ingredient (selected-foods recipe))
                  (assoc-in recipe [:recipe/ingredients (:idx next-match)] ingredient))
            (dec max-loops))
     recipe)))

(defn remove-food-from-ingredient [mg recipe idx]
  (update-in recipe [:recipe/ingredients idx]
             #(deselect-food % ((:picker mg) (:selected-foods %)))))

(defn add-food-to-ingredient [mg recipe idx]
  (as-> (get-in recipe [:recipe/ingredients idx]) ingredient
        (match-ingredient mg ingredient (selected-foods recipe))
        (assoc-in recipe [:recipe/ingredients idx] ingredient)))

(defn exchange-ingredient-foods [mg recipe idx]
  (let [food-count (count (get-in recipe [:recipe/ingredients idx :selected-foods]))
        removed (update-in recipe [:recipe/ingredients idx] deselect-all-foods)]
    (nth (iterate #(add-food-to-ingredient mg % idx) removed) food-count)))

(ns whatsoup.food-kb
  "exposes a knowledge base about food and its properties"
  (:require [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.spec.alpha :as spec]
            [com.stuartsierra.component :as component]))


(defn food? [x] (and (keyword? x) (= "food" (namespace x))))
(defn property? [x] (and (keyword? x) (= "property" (namespace x))))


(spec/def ::food food?)
(spec/def ::property property?)
(spec/def ::food-or-property
  (spec/or :food ::food
           :property ::property))


(defn- properties [food-def]
  (let [food (:food/name food-def)
        props (:food/properties food-def)]
    (apply merge (for [p props] {p #{food}}))))


(defrecord FoodKnowledgeBase [config]
  component/Lifecycle

  (start [this]
    (let [{:keys [food-catalog food-compatibility-matrix]} config]
      (-> this
          (assoc :food-catalog food-catalog)
          (assoc :property-catalog
                 (apply merge-with set/union (map properties food-catalog)))
          (assoc :food-compatibility-matrix food-compatibility-matrix)))))


(defn create-food-kb [config-file]
  (let [config (-> (slurp config-file)
                   (edn/read-string))]
    (component/using (map->FoodKnowledgeBase config) [])))


(defn resolve-food
  "maps foods to themselves and properties to foods using the property-catalog"
  [kb food-or-property]
  (cond
    (food? food-or-property) (hash-set food-or-property)
    (property? food-or-property) (apply hash-set (get (:property-catalog kb) food-or-property))
    :else (throw (RuntimeException. (str "Expected a :food or :property, got: " food-or-property)))))


(defn score [kb candidate-food selected-foods]
  "computes a numerical value reflecting how well the candidate-food fits the previously selected-foods"
  (letfn [(matrix-lookup [food-a food-b]
            (or (get (:food-compatibility-matrix kb) [food-a food-b])
                (get (:food-compatibility-matrix kb) [food-b food-a])
                (if (= food-a food-b) 0.1 1.0)))]
    (float (apply * (for [food selected-foods] (matrix-lookup food candidate-food))))))

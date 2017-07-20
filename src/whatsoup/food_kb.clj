(ns whatsoup.food-kb
  "exposes a knowledge base about food and its properties"
  (:require [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.spec.alpha :as spec]
            [com.stuartsierra.component :as component]
            [whatsoup.util :as util]))

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

(defn property-weights [property-catalog]
  (let [counted-properties (util/map-val count property-catalog)
        total (apply + (vals counted-properties))]
    (util/map-val #(float (/ total %)) counted-properties)))

(defn food-property-catalog [food-catalog]
  (zipmap (map :food/name food-catalog) (map :food/properties food-catalog)))

(defrecord FoodKnowledgeBase [config]
  component/Lifecycle

  (start [component]
    (let [{:keys [food-catalog food-compatibility-matrix]} config]
      (as-> component this
            (assoc this :food-catalog food-catalog)
            (assoc this :property-catalog
                        (apply merge-with set/union (map properties food-catalog)))
            (assoc this :food-compatibility-matrix food-compatibility-matrix)
            (assoc this :food-property-catalog (food-property-catalog food-catalog))
            (assoc this :property-weights (property-weights (:property-catalog this)))
            (dissoc this :config)))))

(defn expand-catalog [catalog]
  "{:a #{:b :c}} -> {:food/name :food/a
                     :food/properties #{:property/b :property/c}"
  (vec (for [kv catalog] {:food/name       (keyword "food" (name (key kv)))
                          :food/properties (apply hash-set (map #(keyword "property" (name %)) (val kv)))})))

(defn create-food-kb [config-file]
  (let [config (-> (slurp config-file)
                   (edn/read-string)
                   (update :food-catalog expand-catalog))]
    (component/using (->FoodKnowledgeBase config) [])))

(defn resolve-food
  "maps foods to themselves and properties to foods using the property-catalog"
  [kb food-or-property]
  (cond
    (food? food-or-property) (hash-set food-or-property)
    (property? food-or-property) (apply hash-set (get (:property-catalog kb) food-or-property))
    :else (throw (RuntimeException. (str "Expected a :food or :property, got: " food-or-property)))))

(defn diversity-score
  "A score with values between 0 and 1 that promotes diversity.
   Think of a 'weighted Jaccard distance'.
   - If two foods overlap in a rare property, the score is diminished.
   - If two foods do not overlap in a rare property, the score is boosted.
   - Common properties have a smaller impact than rare properties."
  [kb food-a food-b]
  (letfn [(sum-weights [properties]
            (apply + (vals (select-keys (:property-weights kb) properties))))]
    (let [a-properties (get-in kb [:food-property-catalog food-a])
          b-properties (get-in kb [:food-property-catalog food-b])
          weights-both (sum-weights (set/intersection a-properties b-properties))
          weights-any (sum-weights (set/union a-properties b-properties))]
      (- 1 (/ weights-both weights-any)))))

(defn score [kb candidate-food selected-foods]
  "computes a numerical value reflecting how well the candidate-food fits the previously selected-foods"
  (letfn [(matrix-lookup [food-a food-b]
            (or (get (:food-compatibility-matrix kb) [food-a food-b])
                (get (:food-compatibility-matrix kb) [food-b food-a])
                (diversity-score kb food-a food-b)))]
    (float (apply * (for [food selected-foods] (matrix-lookup food candidate-food))))))

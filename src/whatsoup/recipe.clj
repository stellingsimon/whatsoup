(ns whatsoup.recipe
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as spec]))


(defn food? [x]
  (and (keyword? x) (= "food" (namespace x))))
(defn property? [x]
  (and (keyword? x) (= "property" (namespace x))))

;; Recipes mainly consist of an ordered list of ingredient descriptors.
;; These specify one or more `properties` that the ingredient needs to satisfy.
;; Alternatively, they may also just list acceptable food choices.
;; Quantities are deliberately omitted for the moment.
;;
;; Here's an example:
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


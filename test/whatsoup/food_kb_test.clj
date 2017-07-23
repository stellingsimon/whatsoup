(ns whatsoup.food-kb-test
  (:require [clojure.test :refer :all]
            [com.stuartsierra.component :as component]
            [whatsoup.food-kb :refer :all]
            [whatsoup.test-util :refer :all]))

(def test-kb
  ; TODO: (2017-07-16, sst) component/start probably not warranted here.
  (component/start
    (->FoodKnowledgeBase
      {:food-catalog              [{:food/name       :food/kartoffel
                                    :food/properties #{:property/gemüse :property/stärkehaltig}}
                                   {:food/name       :food/broccoli
                                    :food/properties #{:property/gemüse}}
                                   {:food/name       :food/schweinefleisch
                                    :food/properties #{:property/fleisch}}]
       :food-compatibility-matrix {[:food/broccoli :food/lauch]        0.5
                                   [:food/lauch :food/schweinefleisch] 5.0}})))

(deftest -resolve-food
  (is (thrown? RuntimeException (resolve-food test-kb :something-else)))
  (is (= #{} (resolve-food test-kb :property/inexistant)))
  (is (= #{:food/zwiebel} (resolve-food test-kb :food/zwiebel)))
  (is (= #{:food/kartoffel} (resolve-food test-kb :property/stärkehaltig)))
  (is (= #{:food/broccoli :food/kartoffel} (resolve-food test-kb :property/gemüse))))

(deftest -score
  (is (almost= 1.0 (score {} :food/broccoli #{})))
  (is (almost= 0.0 (score {} :food/broccoli #{:food/lauch})))
  (is (almost= 0.0 (score {} :food/broccoli #{:food/broccoli})))
  (is (almost= 0.5 (score test-kb :food/broccoli #{:food/lauch})))
  (is (almost= 5.0 (score test-kb :food/schweinefleisch #{:food/lauch}))))

(deftest -diversity-score
  (let [kb
        (component/start
          (->FoodKnowledgeBase
            {:food-catalog              [{:food/name       :food/a
                                          :food/properties #{:property/one :property/two :property/three :property/all}}
                                         {:food/name       :food/b
                                          :food/properties #{:property/two :property/three :property/all}}
                                         {:food/name       :food/c
                                          :food/properties #{:property/three :property/all}}
                                         {:food/name  :food/d
                                          :food/properties #{:property/de-only :property/d-only :property/all}}
                                         {:food/name  :food/e
                                          :food/properties #{:property/de-only :property/e-only :property/all}}
                                         {:food/name  :food/f
                                          :food/properties #{:property/fgh-only :property/f-only :property/all}}
                                         {:food/name  :food/g
                                          :food/properties #{:property/fgh-only :property/g-only :property/all}}
                                         {:food/name  :food/h
                                          :food/properties #{:property/fgh-only :property/h-only :property/all}}]
             :food-compatibility-matrix {}}))]
    (is (= (diversity-score kb :food/a :food/b) (diversity-score kb :food/b :food/a)) "commutativity")
    (is (< (diversity-score kb :food/a :food/b) (diversity-score kb :food/b :food/c)) "smaller overlap -> higher score")
    (is (< (diversity-score kb :food/d :food/e) (diversity-score kb :food/f :food/g)) "overlap in less specific group -> higher score")))
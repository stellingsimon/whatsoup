(ns whatsoup.core
  (:require [clojure.pprint :refer [pprint]]
            [com.stuartsierra.component :as component]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [whatsoup.food-kb :as kb]
            [whatsoup.meal-generator :as meal-generator])
  (:import (java.io StringWriter)))


(defn production-system [config-file]
  (component/system-map
    :food-kb (kb/create-food-kb config-file)))


(def system (production-system "resources/config/food-kb.edn"))


(defn -main [& args]
  (alter-var-root #'system component/start-system))


(defn ring-init []
  (alter-var-root #'system component/start-system))


(defn puree-soup-handler []
  (let [recipe {:recipe/name        "Püree-Suppe"
                :recipe/ingredients [[:= :food/lauch]
                                     [:= :food/zwiebel]
                                     [:= :food/bouillon]
                                     ["Püreebasis"
                                      := [:all-of :property/gemüse :property/stärkehaltig]]
                                     ["Weitere Zutaten"
                                      :* [:any-of :property/gemüse :property/fleisch]]
                                     ["Einlage" :* :property/knusprig]]}]
    (let [s (StringWriter.)]
      (binding [*out* s]
        (pprint (meal-generator/meal (:food-kb system) recipe)))
      (str "<pre>" (.toString s) "</pre>"))))


(defroutes app-routes
           (GET "/" [] "Hello World")
           (GET "/puree-soup" [] (puree-soup-handler))
           (route/not-found "Not Found"))

(def app
  (wrap-defaults app-routes site-defaults))

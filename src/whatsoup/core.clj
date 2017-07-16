(ns whatsoup.core
  (:require [com.stuartsierra.component :as component]
            [whatsoup.food-kb :as kb]))


(defn production-system [config-file]
  (component/system-map
    :food-kb (kb/create-food-kb config-file)))


(def system (production-system "resources/config/food-kb.edn"))


(defn -main [& args]
  (component/start-system system))
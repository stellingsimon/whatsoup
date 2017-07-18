(ns whatsoup.core
  (:require [com.stuartsierra.component :as component]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [whatsoup.food-kb :as kb]
            [whatsoup.handler :as handler]
            [whatsoup.meal-generator :as meal-generator]))


(defn production-system [config-file]
  (component/system-map
    :food-kb (kb/create-food-kb config-file)))


(def system (production-system "resources/config/food-kb.edn"))


(defn -main [& args]
  (alter-var-root #'system component/start-system))


(defn ring-init []
  (alter-var-root #'system component/start-system))


; TODO: (2017-07-18, sst) As soon as the namespaces are reloaded, the system is reset. How to fix that properly?
(ring-init)


(defroutes app-routes
           (GET "/" [] (handler/handle-meal system))
           (GET "/puree-soup" [] (handler/handle-meal system))
           (route/not-found "Not Found"))
(def app (wrap-defaults app-routes site-defaults))

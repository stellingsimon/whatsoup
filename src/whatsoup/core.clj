(ns whatsoup.core
  (:require [clojure.spec.alpha :as spec]
            [com.stuartsierra.component :as component]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [noir.session :as session]
            [noir.response :as response]
            [ring.middleware.session.memory :refer [memory-store]]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [whatsoup.food-kb :as kb]
            [whatsoup.meal-generator :as meal-generator]
            [whatsoup.web :as web]
            [whatsoup.util :as util]))

(defn production-system [food-kb-data recipe-data]
  (component/system-map :food-kb (kb/create-food-kb food-kb-data)
                        :meal-generator (meal-generator/create-meal-generator recipe-data)
                        :picker util/pick-random))

(def system (production-system "resources/config/food-kb.edn"
                               "resources/config/recipe-catalog.edn"))

(defn -main [& args]
  (alter-var-root #'system component/start-system))

(defn ring-init []
  (alter-var-root #'system component/start-system))

; TODO: (2017-07-18, sst) As soon as the namespaces are reloaded, the system is reset. How to fix that properly?
(ring-init)

(defn ingredient-route [action]
  (GET [(str "/" (name action) "/:idx") :idx #"[0-9]+"] [idx]
    (let [recipe (session/get :recipe)
          mg (:meal-generator system)
          ingredient-idx (Integer/parseInt idx)]
      (when recipe
        (->> (web/handle-update action mg recipe ingredient-idx)
             (session/put! :recipe)))
      (response/redirect "/"))))

(defroutes app-routes
           (GET "/" []
             (if-let [recipe (session/get :recipe)]
               (do (session/put! :interactions-count (inc (session/get :interactions-count 0)))
                   (web/display-meal recipe (session/get :interactions-count)))
               (response/redirect "/new")))
           (GET "/new" []
             (session/put! :recipe (web/generate-meal (:meal-generator system)))
             (response/redirect "/"))
           (ingredient-route :new)
           (ingredient-route :add)
           (ingredient-route :remove)
           (GET "/about" [] (web/about-page))
           (GET "/dump" [] (web/dump-config system))
           (route/not-found (web/handle-404)))

(def app (-> app-routes
             (session/wrap-noir-session*)
             (wrap-defaults site-defaults)))

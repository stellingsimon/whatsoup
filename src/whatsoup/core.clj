(ns whatsoup.core
  (:require [com.stuartsierra.component :as component]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.session.memory :refer [memory-store]]
            [noir.session]
            [noir.response]
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


(def ex-recipe
  {:recipe/name        "Püree-Suppe"
   :recipe/ingredients [[:= :food/lauch]
                        [:= :food/zwiebel]
                        [:= :food/bouillon]
                        ["Püreebasis"
                         := [:all-of :property/gemüse :property/stärkehaltig]]
                        ["Weitere Zutaten"
                         :* [:any-of :property/gemüse :property/fleisch]]
                        ["Einlage" :* :property/knusprig]]})


(def app
  (-> (routes
        (GET "/" []
          (do (when (nil? (noir.session/get :recipe))
                (noir.session/put! :recipe ex-recipe))
              (handler/handle-meal system (noir.session/get :recipe))))
        (GET "/new" [ingr]
          (if ingr
            (str ingr)
            (noir.session/clear!))
          (noir.response/redirect "/"))
        (GET "/remove" [ingr]
          (if ingr
            (str ingr)
            #_(noir.session/clear!))
          (noir.response/redirect "/"))
        (GET "/add" [ingr]
          (if ingr
            (str ingr)
            (noir.session/clear!))
          #_(noir.response/redirect "/"))
        (route/not-found "Not Found"))
      (noir.session/wrap-noir-session {:store (memory-store)})
      (wrap-defaults site-defaults)))

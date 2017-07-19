(ns whatsoup.core
  (:require [clojure.spec.alpha :as spec]
            [com.stuartsierra.component :as component]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.session.memory :refer [memory-store]]
            [noir.session :as session]
            [noir.response :as response]
            [whatsoup.food-kb :as kb]
            [whatsoup.handler :as handler]
            [whatsoup.meal-generator :as meal-generator]))


(defn production-system [config-file]
  (component/system-map :food-kb (kb/create-food-kb config-file)))


(def system (production-system "resources/config/food-kb.edn"))


(defn -main [& args]
  (alter-var-root #'system component/start-system))


(defn ring-init []
  (alter-var-root #'system component/start-system))


; TODO: (2017-07-18, sst) As soon as the namespaces are reloaded, the system is reset. How to fix that properly?
(ring-init)


(def ex-recipe
  (spec/conform ::meal-generator/recipe
                {:recipe/name        "Püree-Suppe"
                 :recipe/ingredients [[:= :food/lauch]
                                      [:= :food/zwiebel]
                                      [:= :food/bouillon]
                                      ["Püreebasis"
                                       := [:all-of :property/gemüse :property/stärkehaltig]]
                                      ["Weitere Zutaten"
                                       :* [:any-of :property/gemüse :property/fleisch]]
                                      ["Einlage" :* :property/knusprig]]}))


(defn update-ingredient! [action ingredient-idx]
  (when-let [recipe (session/get :recipe)]
    (session/put! :recipe (handler/handle-update action system recipe (Integer/parseInt ingredient-idx)))))


(defroutes app-routes
           (GET "/" []
             (if-let [recipe (session/get :recipe)]
               (handler/display-meal recipe)
               (response/redirect "/new")))
           (GET "/new" []
             (session/put! :recipe (handler/generate-meal system ex-recipe))
             (response/redirect "/"))
           (GET ["/new/:ingredient-idx" :ingredient-idx #"[0-9]+"] [ingredient-idx]
             (update-ingredient! :new ingredient-idx)
             (response/redirect "/"))
           (GET ["/add/:ingredient-idx" :ingredient-idx #"[0-9]+"] [ingredient-idx]
             (update-ingredient! :add ingredient-idx)
             (response/redirect "/"))
           (GET ["/remove/:ingredient-idx" :ingredient-idx #"[0-9]+"] [ingredient-idx]
             (update-ingredient! :remove ingredient-idx)
             (response/redirect "/"))
           (route/not-found (handler/handle-404)))


; TODO: (2017-07-19, sst) for some inexplicable reason, we loose session state after a few seconds if anti-forgery is enabled. Investigate why...
(def mangled-site-defaults (merge site-defaults {:security {:anti-forgery false}}))
(def app (-> app-routes
             (session/wrap-noir-session {:store (memory-store)})
             (wrap-defaults mangled-site-defaults)))

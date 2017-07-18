(ns whatsoup.handler
  (:require [clojure.spec.alpha :as spec]
            [clojure.string :as str]
            [hiccup.core :as h :refer :all]
            [hiccup.element :as h-elem :refer :all]
            [hiccup.page :as h-page :refer :all]
            [whatsoup.food-kb :as food-kb]
            [whatsoup.meal-generator :as meal-generator]))


(defn render-page [title content]
  (html
    [:html {:xmlns "http://www.w3.org/1999/xhtml"}
     [:head
      [:title "Whatsoup?"]
      [:meta {:http-equiv "Content Type" :content "text/html; charset=utf-8"}]
      (h-page/include-css "css/default.css" "css/font-awesome.css")]
     [:body
      [:div#header
       [:div#app-icon
        [:a {:href "/"} [:img {:src "img/whatsoup.png" :alt "Whatsoup?"}]]]
       (into [] (concat [:h1] title))
       [:hr]]
      [:div#content
       content]]]))


(defn render-foods [foods]
  (if (empty? foods)
    "-"
    (map #(str/capitalize (name %)) foods)))


(defn render-recipe-table [ingredients]
  (let [contents (map #(vector (:role %) (:selected-foods %) (count (:candidate-foods %))) ingredients)]
    [:table#meal-ingredients
     [:tr
      [:th ""]
      [:th "Zutaten"]
      [:th [:a {:href "/"} [:i.fa.fa-refresh]]]]
     (for [[role foods candidate-count] contents]
       [:tr
        [:td role]
        [:td (str/join ", " (render-foods foods))]
        [:td
         (when (> candidate-count 1) [:a {:href "/"} [:i.fa.fa-refresh]])
         (when (> (count foods) 1) [:a {:href "/"} [:i.fa.fa-remove]])
         (when (pos? candidate-count) [:a {:href "/"} [:i.fa.fa-plus]])]])]))


; TODO: (2017-07-18, sst) don't pass system here!
(defn handle-meal [system]
  (let [ex-recipe {:recipe/name        "Püree-Suppe"
                   :recipe/ingredients [[:= :food/lauch]
                                        [:= :food/zwiebel]
                                        [:= :food/bouillon]
                                        ["Püreebasis"
                                         := [:all-of :property/gemüse :property/stärkehaltig]]
                                        ["Weitere Zutaten"
                                         :* [:any-of :property/gemüse :property/fleisch]]
                                        ["Einlage" :* :property/knusprig]]}
        matched-ingredients (->> (spec/conform ::meal-generator/recipe ex-recipe)
                                 (meal-generator/ingredients)
                                 (meal-generator/with-candidates (:food-kb system))
                                 (meal-generator/match-ingredients (:food-kb system)))]
    (render-page
      [(:recipe/name ex-recipe) [:a {:href "/"} [:i.fa.fa-refresh]]]
      (render-recipe-table matched-ingredients))))

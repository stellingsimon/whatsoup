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
      (h-page/include-css "/css/default.css" "/css/font-awesome.css")]
     [:body
      [:div#header
       [:div#app-icon
        [:img {:src "/img/whatsoup.png" :alt "Whatsoup?"}]]
       (into [] (concat [:h1] title))]
      [:hr]
      [:div#content content]]]))


(defn render-foods [foods]
  (cond
    (empty? foods) "-"
    (= 1 (count foods)) (str/capitalize (name (first foods)))
    :else [:ul (for [f foods] [:li (str/capitalize (name f))])]))


(defn render-action
  ([action icon]
   (render-action action icon nil))
  ([action icon idx]
   [:a {:href (str "/" (name action) (when idx (str "/" idx)))}
    [(keyword (str "i.fa.fa-" icon))]]))


(defn zero-to-many-constraint [ingredient]
  (= :* (:quantifier ingredient)))


(defmulti ingredient-action
          "renders the action icon iff applicable"
          (fn [action ingredient] action))


(defmethod ingredient-action :new [_ ingredient]
  (when (or (and (zero-to-many-constraint ingredient)
                 (not (empty? (:selected-foods ingredient))))
            (and (not (zero-to-many-constraint ingredient))
                 (not (empty? (:candidate-foods ingredient)))))
    (render-action :new "refresh" (:idx ingredient))))


(defmethod ingredient-action :add [_ ingredient]
  (when (and (not (empty? (:candidate-foods ingredient)))
             (zero-to-many-constraint ingredient))
    (render-action :add "plus" (:idx ingredient))))


(defmethod ingredient-action :remove [_ ingredient]
  (when (and (zero-to-many-constraint ingredient)
             (not (empty? (:selected-foods ingredient))))
    (render-action :remove "minus" (:idx ingredient))))


(defn render-recipe-table [ingredients]
  [:table#meal-ingredients
   [:tr
    [:th ""]
    [:th "Zutaten"]
    [:th [:a {:href "/"} [:i.fa.fa-refresh]]]]
   (for [{:keys [role selected-foods] :as ingredient} ingredients]
     [:tr
      [:td role]
      [:td (render-foods selected-foods)]
      [:td
       (ingredient-action :new ingredient)
       (ingredient-action :remove ingredient)
       (ingredient-action :add ingredient)]])])


(defn display-meal [recipe]
  (render-page
    [(:recipe/name recipe) (render-action :new "refresh")]
    (render-recipe-table (:recipe/ingredients recipe))))


; TODO: (2017-07-18, sst) don't pass system here!
; TODO: (2017-07-18, sst) match-ingredients should really do all the work here
(defn generate-meal [system recipe]
  (let [kb (:food-kb system)]
    (->> recipe
         (meal-generator/with-candidates kb)
         (meal-generator/match-ingredients kb))))


; TODO: (2017-07-18, sst) don't pass system here!
(defn handle-update [action system recipe idx]
  (let [kb (:food-kb system)]
    (case action
      :new (meal-generator/exchange-ingredient-foods kb recipe idx)
      :add (meal-generator/add-food-to-ingredient kb recipe idx)
      :remove (meal-generator/remove-food-from-ingredient recipe idx))))


(defn handle-404 []
  (render-page "404 - We're out of soup." nil))
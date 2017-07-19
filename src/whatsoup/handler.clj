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


(defn render-food [food]
  (->> (str/split (name food) #"-")
       (map str/capitalize)
       (str/join " ")))

(defn render-foods [foods]
  (cond
    (empty? foods) "-"
    (= 1 (count foods)) (render-food (first foods))
    :else [:ul (for [f foods] [:li (render-food f)])]))


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
    [:th #_[:a {:href "/"} [:i.fa.fa-refresh]]]]
   (for [{:keys [role selected-foods] :as ingredient} ingredients]
     [:tr
      [:td role]
      [:td (render-foods selected-foods)]
      [:td
       (ingredient-action :new ingredient)
       (ingredient-action :remove ingredient)
       (ingredient-action :add ingredient)]])])


(defn display-meal [recipe interactions-count]
  (render-page
    [(str (:recipe/name recipe) ", v" interactions-count) (render-action :new "refresh")]
    (render-recipe-table (:recipe/ingredients recipe))))


; TODO: (2017-07-18, sst) match-ingredients should really do all the work here
(defn generate-meal [mg recipe]
  (->> recipe
       (meal-generator/with-candidates mg)
       (meal-generator/match-ingredients mg)))


(defn handle-update [action mg recipe idx]
  (case action
    :new (meal-generator/exchange-ingredient-foods mg recipe idx)
    :add (meal-generator/add-food-to-ingredient mg recipe idx)
    :remove (meal-generator/remove-food-from-ingredient mg recipe idx)))


(defn handle-404 []
  (render-page "404 - We're out of soup." nil))
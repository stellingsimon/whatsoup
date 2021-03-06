(ns whatsoup.web
  (:require [clojure.java.shell :refer [sh]]
            [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as spec]
            [clojure.string :as str]
            [hiccup.core :as h :refer :all]
            [hiccup.element :as h-elem :refer :all]
            [hiccup.page :as h-page :refer :all]
            [whatsoup.food-kb :as food-kb]
            [whatsoup.meal-generator :as meal-generator]))

(defn render-page [title content & [home-href]]
  (html
    [:html {:xmlns "http://www.w3.org/1999/xhtml"}
     [:head
      [:title "Whatsoup?"]
      [:meta {:http-equiv "Content Type" :content "text/html; charset=utf-8"}]
      (h-page/include-css "/css/default.css" "/css/font-awesome.css")]
     [:body
      [:div#header
       [:div#app-icon
        [:a {:title "Hey you! Whatsoup?" :href (or home-href "/about")}
         [:img {:src "/img/whatsoup.png" :alt "Whatsoup?"}]]]
       (into [] (concat [:h1] title))]
      [:hr]
      [:div#content content]]]))

(defn render-food [food]
  (->> (str/split (name food) #"-")
       (map str/capitalize)
       (str/join " ")))

(defn render-foods [foods candidate-count]
  (let [tooltip {:title (case candidate-count
                          0 "no other options"
                          1 "1 other option"
                          (str candidate-count " other options"))}]
    (cond
      (empty? foods) [:span tooltip "-"]
      (= 1 (count foods)) [:span tooltip (render-food (first foods))]
      :else [:ul tooltip (for [f foods] [:li (render-food f)])])))

(defn render-action
  ([action icon]
   (render-action action icon nil))
  ([action icon idx]
   [:a {:title (str (name action) (when idx " ingredient"))
        :href  (str "/" (name action) (when idx (str "/" idx)))}
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
    [:th#center-column-header "Zutaten"]
    [:th]]
   (for [{:keys [role selected-foods candidate-foods] :as ingredient} ingredients]
     [:tr
      [:td role]
      [:td (render-foods selected-foods (count candidate-foods))]
      [:td
       (ingredient-action :new ingredient)
       (ingredient-action :remove ingredient)
       (ingredient-action :add ingredient)]])])

(defn display-meal [recipe interactions-count]
  (render-page
    [(str (:recipe/name recipe) ", v" interactions-count) (render-action :new "refresh")]
    (render-recipe-table (:recipe/ingredients recipe))))

(defn generate-meal [mg]
  (let [pick (:picker mg)
        recipes (:recipe-catalog mg)]
    (meal-generator/match-ingredients mg (pick recipes))))

(defn handle-update [action mg recipe idx]
  (case action
    :new (meal-generator/exchange-ingredient-foods mg recipe idx)
    :add (meal-generator/add-food-to-ingredient mg recipe idx)
    :remove (meal-generator/remove-food-from-ingredient mg recipe idx)))

(defn loc-count []
  (let [loc-string (:out (sh "./loc.sh"))]
    (str/replace loc-string "total" "")))

(defn about-page []
  (render-page
    ["Whatsoup?"]
    [:div#about-text
     [:div
      [:blockquote "I live on good soup, not fine words."
       [:cite "Molière"]]
      [:p "Soup is delicious, quickly prepared, healthy and economic. Why don't we eat it more often then?
           There's only so many instant soup flavors, and " [:strong "we lack imagination."]]
      [:p "This software aims to provide a solution for the latter: it randomly derives new soups from a set of base
           recipes and a food catalog, while considering how well certain foods fit together."]
      [:p "Try out some of the more exotic combinations that are generated – after all,
           the culinary arts are full of surprises. Maybe we can even convince you that computers are able
           to produce art ;-)"]
      [:p "Bon appétit!"]
      [:p {:style "text-align: right; font-style: italic;"}
       "Whatsoup, made with love (and " (loc-count) " lines of Clojure)."]]]
    "/"))

(defn handle-404 []
  (render-page "404 - We're out of soup." nil))

(defn dump-config [system]
  (html [:pre (with-out-str
                (println "Recipes:")
                (println "--------")
                (pprint (:recipe-catalog (:meal-generator system)))
                (println "")
                (println "Food-Catalog:")
                (println "-------------")
                (pprint (:food-catalog (:food-kb system))))]))
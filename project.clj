(defproject stellingsimon/whatsoup "0.1.0-SNAPSHOT"
  :description "A soup generator"
  :url "https://github.com/stellingsimon/whatsoup"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :min-lein-version  "2.0.0"
  :dependencies [[compojure "1.6.0"]
                 [com.stuartsierra/component "0.3.2"]
                 [hiccup "1.0.5"]
                 [lib-noir "0.9.9"]
                 [org.clojure/clojure "1.9.0-alpha17"]
                 [ring/ring-defaults "0.3.0"]]
  :main ^:skip-aot whatsoup.core
  :ring {:init    whatsoup.core/ring-init
         :handler whatsoup.core/app
         :nrepl {:start? true}}
  :target-path "target/%s"
  :plugins [[lein-ring "0.9.7"]]
  :profiles {:uberjar {:aot :all}})

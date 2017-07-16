(defproject stellingsimon/whatsoup "0.1.0-SNAPSHOT"
  :description "A soup generator"
  :url "https://github.com/stellingsimon/whatsoup"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[com.stuartsierra/component "0.3.2"]
                 [org.clojure/clojure "1.9.0-alpha17"]]
  :main ^:skip-aot whatsoup.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

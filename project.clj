(defproject stellingsimon/whatsoup "0.1.0-SNAPSHOT"
  :description "A soup generator"
  :url "https://github.com/stellingsimon/whatsoup"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot whatsoup.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

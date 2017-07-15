(ns whatsoup.test-util
  (:require [clojure.test :refer :all]))

(defn almost=
  "compares doubles up to a resonable precision"
  ([^double a ^double b]
    (almost= a b 0.001))
  ([^double a ^double b ^double epsilon]
    (< (Math/abs (- b a)) epsilon)))
(ns whatsoup.util)

(defn throw-unexpected-cond [] (throw (RuntimeException. "uncovered cond branch")))
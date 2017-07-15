(ns whatsoup.util)


(defn throw-unexpected-cond [] (throw (RuntimeException. "uncovered cond branch")))


(defn index-with [f coll]
  (zipmap coll (map f coll)))


(defn sort-map-by [keyval-comparator m]
  (letfn [(key-comparator [x y]
            ) ])


  (let [cmp (comparator (fn [k] (vector (get m k) k)))
        keyvals (apply concat (seq m))]
    (apply sorted-map-by cmp keyvals)))
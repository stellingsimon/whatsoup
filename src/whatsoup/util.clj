(ns whatsoup.util)

(defn index-with [f coll]
  (zipmap coll (map f coll)))

(defn pick-random
  "picks a random element from seqable xs"
  [xs]
  (when (pos? (count xs))
    (rand-nth (vec xs))))

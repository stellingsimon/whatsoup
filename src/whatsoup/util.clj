(ns whatsoup.util)


(defn pick-random
  "picks a random element from seqable xs"
  [xs]
  (when (pos? (count xs))
    (rand-nth (vec xs))))
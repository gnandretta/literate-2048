(ns literate-2048.utils)

(defn compact
  "Returns a lazy sequence of the items in coll that are not nil."
  [coll]
  (remove nil? coll))

(defn take-exactly
  "Returns a lazy sequence of the first n items in coll, or all items plus
   the first (- n (count coll)) items of (cycle pad). If a pad is not supplied,
   defaults to an infinite collection of nils."
  ([n coll] (take-exactly n coll (repeat nil)))
  ([n coll pad] (take n (concat coll (cycle pad)))))

(defn nil-indexes
  "Returns a vector with the indexes of the nil elements of v."
  [v]
  (vec (for [i (range (count v)) :when (nil? (get v i))] i)))

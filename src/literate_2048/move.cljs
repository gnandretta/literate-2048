(ns literate-2048.move
  (:require [literate-2048.board :as b]
            [literate-2048.utils :refer (compact take-exactly transpose)]))

(defn synth-adjacent
  "Returns a sequence of the elements of tiles, replacing adjacent elements with
   the result of applying -synth to them when is not logically false."
  [tiles]
  (loop [xs tiles, r []]
    (if (seq xs)
      (if-let [x (b/-synth (first xs) (second xs))]
        (recur (rest (rest xs)) (conj r x))
        (recur (rest xs) (conj r (first xs))))
      r)))

(defn slide-synth-line
  "Returns the result of moving non nil elements of line to the beginning,
   synthesizing adjacent element. For each synthesis a nil is appended to the
   end of the result, keeping the original length of line."
  [line]
  (->> line compact synth-adjacent (take-exactly b/board-order)))

(defn slide-synth
  "Returns the result of sliding and synthesizing the tiles of board in the
   given direction."
  [board direction]
  (let [reverse-slide-synth-line (comp reverse slide-synth-line reverse)
        rows (b/rows board)
        cols (b/cols board)]
    (vec (flatten
          (case direction
            :left (map slide-synth-line rows)
            :right (map reverse-slide-synth-line rows)
            :up (->> cols (map slide-synth-line) transpose)
            :down (->> cols (map reverse-slide-synth-line) transpose))))))

(defn move
  "Returns a new board that results from sliding and synthesizing the
  tiles of board in the given direction and adding a tile. Unless
  sliding and synthesizing the tiles doesn't change the original
  board.  In that case nil is returned."
  [tile board direction]
  (let [board' (slide-synth board direction)]
    (when (not= board board') (b/add-tile tile board'))))

(defn won?
  "Returns true if board contains a tile for which -synth? is falsey."
  [board]
  (some (complement b/-synth?) (compact board)))

(defn lost?
  "Returns true when applying slide-synth to board and every possible direction
   always returns board."
  [board]
  (apply = board (map (partial slide-synth board) [:left :right :up :down])))

(defn ended?
  "Returns true if won? or lost? is truthy for board."
  [board]
  (or (won? board) (lost? board)))

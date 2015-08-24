(ns literate-2048.move
  (:require [literate-2048.board :as b]
            [literate-2048.utils :refer (compact take-exactly transpose)]))

(defn move-line
  [line]
  (take-exactly b/board-order
                (loop [xs line, r []]
                  (if (seq xs)
                    (if (nil? (first xs))
                      (concat r (rest xs))
                      (if-let [x (b/-synth (first xs) (second xs))]
                        (concat (conj r x) (rest (rest xs)))
                        (recur (rest xs) (conj r (first xs)))))
                    r))))

(defn move
  "Returns the result of sliding and synthesizing the tiles of board in the
   given direction."
  [board direction]
  (let [reverse-move-line (comp reverse move-line reverse)
        rows (b/rows board)
        cols (b/cols board)]
    (vec (flatten
          (case direction
            :left (map move-line rows)
            :right (map reverse-move-line rows)
            :up (->> cols (map move-line) transpose)
            :down (->> cols (map reverse-move-line) transpose))))))

;; TODO: the next two functions are very alike.
(defn- moved-rows
  [board-before board-after]
  (compact (keep-indexed #(if %2 % nil)
                         (map #(not= % %2)
                              (b/rows board-before)
                              (b/rows board-after)))))

(defn- moved-cols
  [board-before board-after]
  (compact (keep-indexed #(if %2 % nil)
                         (map #(not= % %2)
                              (b/cols board-before)
                              (b/cols board-after)))))


(defn allowed-indexes
  [board-before board-after direction]
  (let [rows (moved-rows board-before board-after)
        cols (moved-cols board-before board-after)]
    (apply (fn [is js] (vec (for [i is j js] (+ (* i b/board-order) j))))
           (case direction
             :left [rows [3]]
             :right [rows [0]]
             :up [[3] cols]
             :down [[0] cols]))))

(defn attempt-move
  "Returns a new board that results from moving the tiles of board in
  the given direction and adding a tile. Unless the move doesn't
  change the original board. In that case nil is returned."
  [tile board direction]
  (let [board' (move board direction)]
    (when (not= board board')
      (b/add-tile tile board' (allowed-indexes board board' direction)))))

(defn won?
  "Returns true if board contains a tile for which -synth? is falsey."
  [board]
  (some (complement b/-synth?) (compact board)))

(defn lost?
  "Returns true when applying move to board and every possible direction
   always returns board."
  [board]
  (apply = board (map (partial move board) [:left :right :up :down])))

(defn ended?
  "Returns true if won? or lost? is truthy for board."
  [board]
  (or (won? board) (lost? board)))

(ns literate-2048.board
  (:require [literate-2048.utils :refer (nil-indexes)]))

(def board-order
  "Number of squares on a board row or column. It's also the count of rows and
   columns and the square root of the total number of squares."
  4)

(def empty-board
  "Vector containing board-order^2 nil elements."
  (vec (repeat (* board-order board-order) nil)))

(defn add-tile
  "Takes a board and returns a new one with one empty square replaced by tile."
  ([tile board] (add-tile tile board (nil-indexes board)))
  ([tile board indexes] (assoc board (rand-nth indexes) tile)))

(defprotocol ITile
  "Implementation details of a tile."
  (-synth? [this]
    "Returns true if the tile can participate in a synthesis.")
  (-synth [this other]
    "Returns the result of the synthesis of the tile with other, which is
     another ITile when they can be synthesized or nil when they can't. No tile
     can be synthesized with nil."))

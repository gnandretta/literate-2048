(ns literate-2048.core)

(enable-console-print!)

(println "Hello world!")

;; # Board and tiles

;; The board on which the game is played consist of 16 squares ordered in 4 rows
;; and 4 columns. People seem to get really mad when they see the same number
;; over and over again, so let's give the number 4 a better name.

(def
  ^{:doc "Number of squares on a board's row or column. It is also the count of
          rows and columns and the square root of the total number of squares."}
  board-order 4)

;; The choosen representation for the board is a vector of 16 (4*4) elements.
;; It will contain the squares from the first row, followed by the squares from
;; the second, and so on. Intuitively, it seems that the natural representation
;; for such a board is a vector of vectors, but it will make things more
;; complicated than they need to be.

;; The simplest board, and our starting point, is one that is completely empty.
;; An empty square is represented by nil.

(def
  ^{:doc "A vector of board-order^2 nils."}
  empty-board (vec (repeat (* board-order board-order) nil)))

;; Every time a tile is added to the board, it randomly appears in a empty
;; square, with a value of either 2 or 4.

;; It's easy to decide the value of the new tile.

(defn rand-val
  "Returns 2 most of the time, but sometimes it returns 4."
  []
  (if (< (rand) 0.8) 2 4))

;; It's also easy to choose the empty square where the tile will be placed: get
;; the indexes of the nil elements of the board representation, shuffle them and
;; take the first.

(defn nil-indexes
  "Returns a set with the indexes of the nil elements of v."
  [v]
  (into #{} (for [i (range (count v)) :when (nil? (get v i))] i)))

(defn rand-nil-index
  "Returns the index of a randomly choosen nil element of v."
  [v]
  (-> v nil-indexes shuffle first))

;; Finally, associate the index with a new tile in the board representation. A
;; function that builds the tile from its value must be provided because the
;; details about what a tile looks like aren't really important and somebody
;; else should take care of them.

(defn add-tile
  "Takes a board and returns a new one with one more tile, where tile-fn is a
   function that will take a randomly choosen tile value and return a new tile."
  [tile-fn board]
  (assoc board (rand-nil-index board) (tile-fn (rand-val))))

;; All the conditions are now met to build a board with two tiles, which is
;; necessary for the player to make the first move.

(defn initial-board
  "Returns a board with two (random) tiles, where tile-fn is a function that
   will take a randomly choosen tile value and return a new tile."
  [tile-fn]
  (let [f (partial add-tile tile-fn)]
    (-> empty-board f f)))

;; # Tile synthesis

;; # Moves

;; # Rendering

;; # UI

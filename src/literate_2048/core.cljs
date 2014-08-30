(ns literate-2048.core
  (:require [quiescent :as q :include-macros true]
            [quiescent.dom :as d]))

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
;; square. It's easy to choose what that square would be by getting the indexes
;; of the nil elements of the board representation, shuffling them and taking
;; the first.

(defn nil-indexes
  "Returns a set with the indexes of the nil elements of v."
  [v]
  (into #{} (for [i (range (count v)) :when (nil? (get v i))] i)))

(defn rand-nil-index
  "Returns the index of a randomly choosen nil element of v."
  [v]
  (-> v nil-indexes shuffle first))

;; Then, we associate the index with a new tile in the board representation. A
;; function that builds the tile must be provided because the details about what
;; a tile looks like aren't really important and somebody else should take care 
;; of them.

(defn add-tile
  "Takes a board and returns a new one with one more tile, where tile-fn is a
   function that returns a new tile."
  [tile-fn board]
  (assoc board (rand-nil-index board) (tile-fn)))

;; All the conditions are now met to build a board with two tiles, which is
;; necessary for the player to make the first move.

(defn initial-board
  "Returns a board with two (random) tiles, where tile-fn is a function that
   returns a new tile."
  [tile-fn]
  (let [f (partial add-tile tile-fn)]
    (-> empty-board f f)))

;; # Tile synthesis

;; The goal of the game is to merge tiles together until a tile that is not able
;; to be merged appears. Thus, we need a way to tell if a tile can be merged
;; with another tiles and a mechanism to obtain the result of the merge of two
;; given tiles.

;; Since 'merge' is already a map operation, let's name the process 'synthesis'.

(defprotocol ITile
  "Implementation details of a tile."
  (-synth? [this]
    "Returns true if the tile can participate in a synthesis.")
  (-synth [this other]
    "Returns the result of the synthesis, another ITile when they can be
    synthesized or nil when they can't. No tile can be synthesized with nil."))

;; While two tiles might be able to participate in a synthesis they might not be
;; able to be synthesized together.

;; To synthesize a sequence of tiles, take the first two and try to synthesize
;; them:
;; - If they can be synthesized, accumulate the result of their synthesis and
;;   repeat the process for the rest of the tiles (a tile can't be synthesized
;;   more than once in one process).
;; - If they can't be synthesized, accumulate the first tile and repeat the
;;   process for all the tiles except that firstone.
;; - If there's only one tile, accumulate it. (This can be reduced to the
;;   previous directive if we try to synthesize the tile with nil.)
;; - When there are no more tiles the process is done, and the accumulation is
;;   its result.
(defn synth-adjacent
  "Returns a sequence of the elements of tiles, replacing adjacent elements with
   the result of applying -synth to them when is not logically false."
  [tiles]
  (loop [xs tiles, r []]
    (if (seq xs)
      (if-let [x (-synth (first xs) (second xs))]
        (recur (rest (rest xs)) (conj r x))
        (recur (rest xs) (conj r (first xs))))
      r)))

;; # Moves

;; The two following functions will make it easier to describe the player moves
;; using sequences. The first one removes all the nil elements of a sequence.

(defn compact
  "Returns a lazy sequence of the items in coll that are not nil."
  [coll]
  (remove nil? coll))

;; The second ensures that a sequence has a given length, removing or adding
;; elements as necessary. The added elements are nil by default, but they can be
;; provided.

(defn take-exactly
  "Returns a lazy sequence of the first n items in coll, or all items plus
   the first (- n (count coll)) items of (cycle pad). If a pad is not supplied,
   defaults to an infinite collection of nils."
  ([n coll] (take-exactly n coll (repeat nil)))
  ([n coll pad] (take n (concat coll (cycle pad)))))

;; When the player makes a move, tiles slide as far as possible in the chosen
;; direction until they are stopped by either another tile or the edge of the
;; board. If two tiles of the same number collide while moving, they will merge
;; into a tile with the total value of the two tiles that collided. The
;; resulting tile cannot merge with another tile again in the same move.

;; While the entire process looks overwhelming, it turns out that it is easy to
;; move a single row to the left. If a row is a sequence of tiles and/or nil
;; elements, then:
;; - applying compact to the sequence solves the 'sliding' part of the problem,
;; - and applying synth-adjacent to that solves the 'merge' part.
;; However, the previous function applications lead to an undesired consequence,
;; the resulting sequence is not a proper row. It might be shorter because of
;; the lack of nil elements. Fortunately, nils should be aligned to the right
;; and can be appended with take-exactly.

(defn slide-synth-line
  [line]
  (->> line compact synth-adjacent (take-exactly board-order)))

;; There is a very good reason the previous function was not called
;; slide-synth-row. If the input and the output of slide-synth-line are
;; reversed, the resulting sequence is the equivalent of moving the row to the
;; right. Moreover, every move can be obtained by applying the previous
;; function, it just need to be provided with the right line argument.

;; A line is a sequence that represents the flow of the tiles during a move. In
;; other words, is a row or a column of the board, where the closest a square is
;; is to edge of the board the tiles are sliding to, the sooner it appears in
;; the sequence.

;; The slide and synthesis of the whole board can be summarized as:
;; - Obtaining the lines. Rows are partitions of 4 elements of our board
;;   representation and columns are transposition of them.
;; - Slide and synth the lines.
;; - Turn the resulting sequences (rows or columns) back to our board
;; representation.

(defn slide-synth
  "Returns the result of sliding and synthesizing the tiles of board in the
   given direction."
  [board direction]
  (let [transpose #(apply map list %)
        reverse-slide-synth-line (comp reverse slide-synth-line reverse)
        rows (partition board-order board)
        cols (transpose rows)]
    (vec (flatten
          (case direction
            :left (map slide-synth-line rows)
            :right (map reverse-slide-synth-line rows)
            :up (->> cols (map slide-synth-line) transpose)
            :down (->> cols (map reverse-slide-synth-line) transpose))))))

;; A tile is added when the board changes.

(defn move
  "Slides and synthesizes the tiles of board in the given direction, and return
   the result when is different from board."
  [tile-fn board direction]
  (let [board' (slide-synth board direction)]
    (when (not= board board') (add-tile tile-fn board'))))

;; # Rendering

(defn pos-classes
  [[i j]]
  (str "pos pos-" i "-" j))

(defn tile-classes
  [val]
  (str "tile tile-" val))

(defn background-view
  [rows cols]
  (apply d/div {:className "background"}
    (for [i (range rows) j (range cols)]
      (d/div {:className (pos-classes [i j])}))))

(defn tiles-view [s]
  (apply d/div {:className "tiles"}
    (map (fn [{:keys [val pos key classes] :as tile}]
           (d/div {:className (pos-classes pos)
                   :key key}
             (d/div {:className (str classes " " (tile-classes val))}
               val)))
         s)))

(q/defcomponent SquareBoard
  [tiles order]
  (d/div {:className "square-board"}
    (background-view order order)
    (tiles-view tiles)))

;; # UI

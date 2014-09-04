(ns literate-2048.core
  (:require [goog.events :as events]
            [cljs.core.async :refer (chan put!)]
            [quiescent :as q :include-macros true]
            [quiescent.dom :as d]))

(enable-console-print!)

(println "Hello world!")

;; # Board and tiles

;; The board on which the game is played consist of 16 squares ordered in 4 rows
;; and 4 columns. People seem to get really mad when they see the same number
;; over and over again, so let's give the number 4 a better name.

(def board-order
  "Number of squares on a board's row or column. It is also the count of rows
   rows and columns and the square root of the total number of squares."
  4)

;; The choosen representation for the board is a vector of 16 (4*4) elements.
;; It will contain the squares from the first row, followed by the squares from
;; the second, and so on. Intuitively, it seems that the natural representation
;; for such a board is a vector of vectors, but it will make things more
;; complicated than they need to be.

;; The simplest board, and our starting point, is one that is completely empty.
;; An empty square is represented by nil.

(def empty-board
  "A vector of board-order^2 nils."
  (vec (repeat (* board-order board-order) nil)))

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

;; As stated in the 'Tile synthesizes' section, the player wins the game when
;; a tile that is not able to be merged appears.

(defn won?
  "Returns true if board contains a tile for which -synth? is falsey."
  [board]
  (some (complement -synth?) (compact board)))

;; The player loses the game when there are no more moves left, that is, sliding
;; and synthesizing the board on every direction yields the same original board.
;; The game ends when the player wins or loses.

(defn ended?
  "Returns true if won? is truthy for board or when applying slide-synth to
   board and every possible direction always returns board."
  [board]
  (or (won? board)
      (apply = board
             (map (partial slide-synth board) [:left :right :up :down]))))

;; # Rendering

;; Because manually manipulating the DOM is messy, we'll let
;; [Quiescent](https://github.com/levand/quiescent) do the work, which is an
;; abstraction over [React](http://facebook.github.io/react) that leverages
;; immutability. All we need to do is provide a data structure which tells how
;; we want the DOM to look like at a given moment, but before getting into it
;; lets define a couple of helper functions.

;; An element is positioned on the board by specifying the classes "pos" and
;; "pos-i-j", where i and j are numbers from 0 to 3 representing the row and
;; the column of the element, respectively. The following function translates a
;; vector with the element position, that is, the row and the column, to a
;; string of theese classes.

(defn pos-classes
  [[i j]]
  (str "pos pos-" i "-" j))

;; Also, tiles are styled differently according to their value. We'll use the
;; class "tile" to identify an element as a tile and "tile-val", where val is
;; the value of the tile, to apply the correct color, font size, shadow, etc.

(defn tile-classes
  [val]
  (str "tile tile-" val))

;; To paint the squares of the board, we need to render the following HTML

;;    <div class="squares">
;;      <div class="square pos pos-0-0"></div>
;;      <div class="square pos pos-0-1"></div>
;;      ...
;;      <div class="square pos pos-3-3></div>
;;    </div>

;; The squares-view function, when supplied with 4, returns a virtual
;; representation of that.

(defn squares-view
  [order]
  (apply d/div {:className "squares"}
    (for [i (range order) j (range order)]
      (d/div {:className (str "square " (pos-classes [i j]))}))))

;; A tile with a value of 2 that just appeared on the bottom-left corner will
;; look something like this

;;    <div class="pos pos-3-0">
;;      <div class="tile tile-2 reveal">2</div>
;;    </div>

;; The tiles-view function, takes a sequence of tiles and renders them inside a
;; div with the "tiles" class. Each element of the sequence is a map with the
;; :val and :pos keys, 2 and [3 0] from the example. A :classes key may be
;; provided to apply extra classes to the tile element, like "reveal" from the
;; example. As we'll see in a moment, theese classes are needed to animate the
;; appearence of new tiles. There is one last detail. When React updates the
;; DOM, it tries to do it in the most efficiently manner and does not provide
;; any warantee by default that the same DOM element will be used for the same
;; tile across render passes. This is a problem for us because the CSS
;; translation animations happens when the "pos-i-j" class of a DOM element
;; changes. The goods news is that we can instruct React to do it if we identify
;; indentify every tile uniquely with a :key. To learn more about React's
;; reconciliation process see
;; [this](http://facebook.github.io/react/docs/reconciliation.html) and
;; [this](http://facebook.github.io/react/docs/multiple-components.html).

(defn tiles-view
  [s]
  (apply d/div {:className "tiles"}
    (map (fn [{:keys [val pos key classes]}]
           (d/div {:className (pos-classes pos)
                   :key key}
             (d/div {:className (str classes " " (tile-classes val))}
               val)))
         s)))

;; Finally, a Quiescent component will render the squares and the tiles. To
;; learn more about Quiescent you can read the
;; [docs](https://github.com/levand/quiescent/blob/master/docs.md) or just the
;; [source](https://github.com/levand/quiescent/tree/master/src).

(q/defcomponent SquareBoard
  [tiles order]
  (d/div {:className "square-board"}
    (squares-view order)
    (tiles-view tiles)))

;; # UI

;; A map suffices to represent a tile, the UI's core element. As you may recall
;; from section 'Board and tiles', if we want to add a tile to the board we need
;; a function that returns it. Every new tile contains the following keys and
;; corresponding values:
;; - :val, the number 2 or 4
;; - :key, some keyword that uniquely identifies the tile
;; - :new, the boolean true
;; The same function will also take two tiles and return the result of merging
;; them together, that is a new tile described by:
;; - :val, the sum of the :val of the source tiles
;; - :key, some keywords that uniquely identifies the tile
;; - :src, a vector containing the source tiles that originated this one

(defn build-tile
  "Returns a map with :val associated to val and :key to an unique keyword. If
   no val is supplied it defaults to 2 most of the time, but it might be 4, and
   the key :new is mapped to true. Alternatively, when applied to two tiles, x
   and y, the result is the same as calling it with the sum of the tiles :val
   and associng the key :src to a vector of the two."
  ([] (assoc (build-tile (if (< (rand) 0.8) 2 4)) :new true))
  ([val] {:val val :key (keyword (gensym ""))})
  ([x y] (assoc (build-tile (+ (:val x) (:val y))) :src [x y])))

;; Before going further, an implementation of the ITile protocol (presented in
;; section 'Tile synthesis') must be provided. The synthesis of two tiles with
;; the same :val is another tile (described below), but the synthesis of two
;; tiles with different :val is nil. A tile with a :val of 2048 can't be
;; synthesized anymore.

(extend-type cljs.core/PersistentArrayMap
  ITile
  (-synth? [this] (not= (:val this) 2048))
  (-synth [this other]
    (when (= (:val this) (:val other))
      (build-tile this other))))

;; Apart from taking place in the computer's memory, all the events need to be
;; shown on the screen. We'll break every move in two phases, slide and reveal,
;; because we want to support animation. Only tiles that were already on the
;; board before the last move took place participate in the slide phase. On the
;; reveal phase, colliding tiles are replaced with their synthesis and a new
;; tile appears randomly.

;; As you can probably guess, our board representation needs to be transformed
;; into something that SquareBoard (defined in section 'Rendering') can make
;; sense of. Additionaly, the transformation must remove the novelty when the
;; slide phase is rendered. The next steps illustrate a way to achieve that:
;; - Keep only the tiles of the board (remove the empty squares) and associate
;;   on each one :pos to a vector with the column and the row of the square the
;;   it is located.
;; - If no novelty must be present, remove new tiles and replace synthesized
;;   tiles with the original ones. Those tiles will have the position of the
;;   synthesis because during the animation they need to slide there before
;;   being merged.
;; - Associate :classes with "fade-in" on the tiles with the key :new to animate
;;   its appearence by scaling them to its actual size from an almost invisible
;;   one.
;; - Associate :classes with "highlight" on tiles with the key :src to animate
;;   its appearence by scaling them to a size a little bigger and then back to
;;   its original size.

(defn board->tiles
  "Returns a sequence of tiles taken from board with their :pos and sorted by
   :key. Assocs :classes to \"fade-in\" and \"highlight\" for tiles with the
   :new and :src keys, respectively. When novelty? is false it removes tiles
   with the :new key and replaces the ones that contain :src with the tiles in
   it."
  [board novelty?]
  (->> board
       (keep-indexed (fn [i x]
                       (let [pos [(quot i board-order) (rem i board-order)]]
                         (when x (assoc x :pos pos)))))
       (remove (fn [x] (and (not novelty?) (:new x))))
       (reduce (fn [r {:keys [pos src] :as x}]
                 (if (and (not novelty?) src)
                   (apply conj r (map #(assoc % :pos pos) src))
                   (conj r x)))
               [])
       (map (fn [x]
              (assoc x :classes (cond (:new x) "fade-in"
                                      (:src x) "highlight"))))
       (sort-by :key)))

;; Our board representation only describes how the board looks at a given point
;; in time. Tiles that are the result of a synthesis or have been added will no
;; longer be novelty the next time the player makes a move. That's why moves are
;; made on a board with no transient data, none of its tiles have a :new or a
;; :src key

(defn handle-move
  "Performs a new move on board in the given direction after removing transient
   data from the tiles (:new and :src keys). Returns the new board or nil when
   no change occurs."
  [board direction]
  (move build-tile (map #(dissoc % :new :src) board) direction))

;; The player will make moves by pressing the arrow keys. Core.async's channels
;; come in handy when observing events from the DOM. If you are not familiar
;; with core.async there is an excellent introductory
;; [webinar](http://go.cognitect.com/core_async_webinar_recording) by David
;; Nolen. The next two functions were copied from the code examples explained on
;; it. The first can take events from a DOM element and convert them to a
;; channel which we can read from.

(defn events->chan
  "Returns a channel c where the events of event-type that happens on the DOM
   element el will be put. If c is not supplied (chan) is used."
  ([el event-type] (events->chan el event-type (chan)))
  ([el event-type c]
     (events/listen el event-type
       (fn [e] (put! c e)))
     c))

;; The second function will return a channel only with the events that we care
;; about, that is keyup events of the document that happens when an arrow key is
;; pressed. Also, we would like something more descriptive than the raw DOM
;; event. So, we'll take every keyup event of the document, map it to its key
;; code, stop if they don't belong to an arrow key or map it to either :left,
;; :up, :right or :down when they do. The second function returns a channel that
;; does just that.

(defn keys-chan
  "Returns a channel of :left, :up, :right and :down events sourced from arrow
   key presses."
  []
  (let [key-map {37 :left 38 :up 39 :right 40 :down}]
    (events->chan js/document goog.events.EventType.KEYUP
                  (chan 1 (comp (map #(.-keyCode %))
                                (filter (set (keys key-map)))
                                (map key-map))))))

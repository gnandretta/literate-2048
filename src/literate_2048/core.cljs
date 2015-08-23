(ns literate-2048.core
  (:require [goog.events :as events]
            [cljs.core.async :refer (<! chan dropping-buffer timeout put!)]
            [quiescent :as q :include-macros true]
            [quiescent.dom :as d])
  (:require-macros [cljs.core.async.macros :refer (go-loop)]))

(enable-console-print!)

(println "Hello world!")

(def board-order
  "Number of squares on a board row or column. It's also the count of rows and
   columns and the square root of the total number of squares."
  4)

(def empty-board
  "Vector containing board-order^2 nil elements."
  (vec (repeat (* board-order board-order) nil)))

(defn nil-indexes
  "Returns a set with the indexes of the nil elements of v."
  [v]
  (into #{} (for [i (range (count v)) :when (nil? (get v i))] i)))

(defn rand-nil-index
  "Returns the index of a randomly chosen nil element of v."
  [v]
  (-> v nil-indexes shuffle first))

(defn add-tile
  "Takes a board and returns a new one with one empty square replaced by the
   return value of tile-fn."
  [tile-fn board]
  (assoc board (rand-nil-index board) (tile-fn)))

(defn initial-board
  "Returns a board with two tiles returned by tile-fn, which will be called
   twice."
  [tile-fn]
  (let [f (partial add-tile tile-fn)]
    (-> empty-board f f)))

(defprotocol ITile
  "Implementation details of a tile."
  (-synth? [this]
    "Returns true if the tile can participate in a synthesis.")
  (-synth [this other]
    "Returns the result of the synthesis of the tile with other, which is
     another ITile when they can be synthesized or nil when they can't. No tile
     can be synthesized with nil."))

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

(defn slide-synth-line
  "Returns the result of moving non nil elements of line to the beginning,
   synthesizing adjacent element. For each synthesis a nil is appended to the
   end of the result, keeping the original length of line."
  [line]
  (->> line compact synth-adjacent (take-exactly board-order)))

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

(defn move
  "Returns a new board that results from sliding and synthesizing the tiles of
   board in the given direction and adding a tile, which is built with tile-fn.
   Unless sliding and synthesizing the tiles doesn't change the original board.
   In that case nil is returned."
  [tile-fn board direction]
  (let [board' (slide-synth board direction)]
    (when (not= board board') (add-tile tile-fn board'))))

(defn won?
  "Returns true if board contains a tile for which -synth? is falsey."
  [board]
  (some (complement -synth?) (compact board)))

(defn lost?
  "Returns true when applying slide-synth to board and every possible direction
   always returns board."
  [board]
  (apply = board (map (partial slide-synth board) [:left :right :up :down])))

(defn ended?
  "Returns true if won? or lost? is truthy for board."
  [board]
  (or (won? board) (lost? board)))

(defn pos-view
  "Returns a div wrapping content with positioning classes derived from pos,
   which is a two-element vector containing a row and a column number.
   Optionally, a React key can be provided."
  ([pos content] (pos-view pos nil content))
  ([[i j] key content]
     (d/div {:className (str "pos pos-" i "-" j)
             :key key}
       content)))

(defn square-view
  "Returns a div with a square class."
  []
  (d/div {:className "square"}))

(defn squares-view
  "Returns a div containing a square for each position of a board with the given
   order."
  [order]
  (apply d/div {:className "squares"}
    (for [i (range order) j (range order)]
      (pos-view [i j] (square-view)))))

(defn tile-view
  "Returns a div with tile classes whose content is val. Extra classes can be
   provided."
  ([val] (tile-view val ""))
  ([val classes] (d/div {:className (str classes " " (str "tile tile-" val))}
                   val)))

(defn tiles-view
  "Returns a div with the tiles class containing the tiles provided. Tiles are
   wrapped in a div with their position. Each tile is a map that must contain
   :val (something with a str value) and :pos (two-element vector containing a
   row and a column number). Optionally, it can also contain :key (a React key)
   and :classes (string to be concatenated with the default tile classes)."
  [tiles]
  (apply d/div {:className "tiles"}
    (map (fn [{:keys [val pos key classes]}]
           (pos-view pos key (tile-view val classes)))
         tiles)))

(q/defcomponent SquareBoard
  [tiles order]
  (d/div {:className "square-board"}
    (squares-view order)
    (tiles-view tiles)))

(defn build-tile
  "Returns a map with :val associated to val and :key to an unique keyword. If
   no val is supplied, it defaults to 2 most of the time but it might default to
   4, also the key :new is mapped to true. Alternatively, when applied to two
   tiles x and y, the result is the same as calling it with the sum of the tiles
   :val and associng the key :src to a vector containing x and y."
  ([] (assoc (build-tile (if (< (rand) 0.8) 2 4)) :new true))
  ([val] {:val val :key (keyword (gensym ""))})
  ([x y] (assoc (build-tile (+ (:val x) (:val y))) :src [x y])))

(extend-type cljs.core/PersistentArrayMap
  ITile
  (-synth? [this] (not= (:val this) 2048))
  (-synth [this other]
    (when (= (:val this) (:val other))
      (build-tile this other))))

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

(q/defcomponent Game
  [{:keys [board phase]}]
  (d/div {:className "game"}
    (SquareBoard (board->tiles board (= phase :reveal)) board-order)
    (when (ended? board)
      (d/div {:className "end-message"}
        (if (won? board) "You win!" "You lose!")))))

(defn render
  "Renders the Game component to the DOM node with the 'game' id passing board
   and and phase as its values."
  [board phase]
  (q/render (Game {:board board :phase phase})
            (.getElementById js/document "game")))

(defn handle-move
  "Performs a new move on board in the given direction after removing transient
   data from the tiles (:new and :src keys). Returns the new board, or nil when
   no change occurs."
  [board direction]
  (move build-tile (map #(dissoc % :new :src) board) direction))

(defn events->chan
  "Returns a channel c where the events of event-type that happens on the DOM
   element el will be put. If c is not supplied (chan) is used."
  ([el event-type] (events->chan el event-type (chan)))
  ([el event-type c]
     (events/listen el event-type
       (fn [e] (put! c e)))
     c))

(defn keys-chan
  "Returns a channel of :left, :up, :right and :down events sourced from arrow
   key presses."
  []
  (let [key-map {37 :left 38 :up 39 :right 40 :down}]
    (events->chan js/document goog.events.EventType.KEYUP
      (chan (dropping-buffer 1) (comp (map #(.-keyCode %))
                                      (filter (set (keys key-map)))
                                      (map key-map))))))

(let [keys (keys-chan)]
  (go-loop [board (initial-board build-tile)
            action :render]
    (case action
      :render (do (render board :slide)
                  (<! (timeout 100))
                  (render board :reveal)
                  (<! (timeout 100))
                  (when-not (ended? board)
                    (recur board :wait)))
      :wait (if-let [board' (handle-move board (<! keys))]
              (recur board' :render)
              (recur board :wait)))))

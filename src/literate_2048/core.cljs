(ns literate-2048.core
  (:require [cljs.core.async :refer (<! timeout)]
            [quiescent :as q :include-macros true]
            [quiescent.dom :as d]
            [literate-2048.board :as b]
            [literate-2048.events :as e]
            [literate-2048.move :as m]
            [literate-2048.ui :as ui])
  (:require-macros [cljs.core.async.macros :refer (go-loop)]))

(enable-console-print!)

(println "Hello world!")

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
  b/ITile
  (-synth? [this] (not= (:val this) 2048))
  (-synth [this other]
    (when (= (:val this) (:val other))
      (build-tile this other))))

(defn initial-board
  "Returns a board with two tiles."
  []
  (->> b/empty-board (b/add-tile (build-tile)) (b/add-tile (build-tile))))

(defn board->tiles
  "Returns a sequence of tiles taken from board with their :pos and sorted by
   :key. Assocs :classes to \"fade-in\" and \"highlight\" for tiles with the
   :new and :src keys, respectively. When novelty? is false it removes tiles
   with the :new key and replaces the ones that contain :src with the tiles in
   it."
  [board novelty?]
  (->> board
       (keep-indexed (fn [i x]
                       (let [pos [(quot i b/board-order) (rem i b/board-order)]]
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
    (ui/SquareBoard (board->tiles board (= phase :reveal)) b/board-order)
    (when (m/ended? board)
      (d/div {:className "end-message"}
        (if (m/won? board) "You win!" "You lose!")))))

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
  (m/move (build-tile) (map #(dissoc % :new :src) board) direction))

(let [keys (e/keys-chan)]
  (go-loop [board (initial-board)
            action :render]
    (case action
      :render (do (render board :slide)
                  (<! (timeout 100))
                  (render board :reveal)
                  (<! (timeout 100))
                  (when-not (m/ended? board)
                    (recur board :wait)))
      :wait (if-let [board' (handle-move board (<! keys))]
              (recur board' :render)
              (recur board :wait)))))

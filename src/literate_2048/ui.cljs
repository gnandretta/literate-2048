(ns literate-2048.ui
  (:require [quiescent :as q :include-macros true]
            [quiescent.dom :as d]))

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

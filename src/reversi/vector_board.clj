(ns reversi.vector-board
  "Functions for manipulating boards and generating moves."
  (:use [clojure.set :only [union intersection difference]]))

(def board-size 8)

(def empty 0)
(def black 1)
(def white 2)
(def outer 3)

(def initial-board
  [3 3 3 3 3 3 3 3 3 3
   3 0 0 0 0 0 0 0 0 3
   3 0 0 0 0 0 0 0 0 3
   3 0 0 0 0 0 0 0 0 3
   3 0 0 0 2 1 0 0 0 3
   3 0 0 0 1 2 0 0 0 3
   3 0 0 0 0 0 0 0 0 3
   3 0 0 0 0 0 0 0 0 3
   3 0 0 0 0 0 0 0 0 3
   3 3 3 3 3 3 3 3 3 3])

(defn black? [player] (= player black))
(defn white? [player] (= player white))
(defn occupied? [pos] (or (= pos black) (= pos white)))

(def opponent [0 2 1])

(defn neighbours
  "returns all occupied positions adjacent to pos (including pos, if it is occupied)."
  [board pos]
  (for [a [(- pos 10) pos (+ pos 10)]
        b [(dec a) a (inc a)]
        :when (occupied? (board b))]
    b))

(defn flip
  "Tries to flip pieces from (not including) pos in the given direction
   until a piece of the players color appears. dir is on the form [x y],
   i.e. 10 means east, 01 south, -11 southwest and so on.
   Returns the set of flipped pieces (nil if no pieces can be flipped)."
  [board player pos dir]
  (loop [piece (+ pos dir)
         flipped-pieces #{}]
    (condp = (board piece)
      (opponent player) (recur (+ piece dir) (conj flipped-pieces piece))
      player            flipped-pieces
      empty             #{}
      outer             #{})))

(defn move
  "Returns a board with a piece of the given players color placed at pos
  and all affected pieces flipped, or nil if the move is illegal."
  [board player pos]
  (when-not (occupied? (board pos))
    (let [directions (map #(- % pos) (neighbours board pos))
          flipped-pieces (apply union (map #(flip board player pos %)
                                           directions))]
      (when-not (= #{} flipped-pieces) ; faster than "empty?"
        (apply assoc board pos player
               (interleave flipped-pieces (repeat player)))))))

(defn moves
  "Returns all possible moves for the given player and board. The list
  will never be empty; if the player has no legal moves a move that does
  nothing will be returned."
  [[board player]]
  (let [mvs (for [y (range 1 (inc board-size))
                  x (range 1 (inc board-size))
                  :let [mv (move board player (+ x (* 10 y)))]
                  :when mv]
              [mv (opponent player)])]
    (if (empty? mvs)
      (list [board (opponent player)])
      mvs)))

(defn legal-positions
  "returns all legal positions for the given player and board"
  [[board player]]
  (for [y (range 1 (inc board-size))
        x (range 1 (inc board-size))
        :let [pos (+ x (* 10 y))
              mv (move board player pos)]
        :when mv]
    pos))

(defn print-board [board]
  (let [display-piece ["." "●" "○" "█"]]
    (doseq [row (partition 10 board)]
      (apply println (map display-piece row)))))

;; # Converting from and to string representation
(defn str->move [[str-player &  str-board]]
  (let [player ({\W white, \B black} str-player)
        board (vec (concat (repeat 11 outer)
                           (->> str-board
                                (map {\E empty, \O white, \X black})
                                (partition 8)
                                (interpose [outer outer])
                                (flatten))
                           (repeat 11 outer)))]
    [board player]))

(comment "NOT WORKING:"
  (defn move->str [[prev-board p1] [board _]]
    (let [[[x y]] (seq (intersection (set (legal-positions [prev-board p1]))
                                     (difference (set (keys board)) (set (keys prev-board)))))]
      (if (nil? x)
        "pass"
        (str "(" (inc y) "," (inc x) ")")))))

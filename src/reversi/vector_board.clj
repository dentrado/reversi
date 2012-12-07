(ns reversi.vector-board
  (:use [clojure.set :only [union]]))

(def *board-size* 8)

(def empty 0)
(def black 1)
(def white 2)
(def outer 3)

(def board
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
(defn occupied? [pos] (or (= pos black) (= pos white)))

(def opponent [0 2 1])

(defn neighbours
  "returns all occupied positions from [x-1 y-1] to [x+1 y+1]"
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

(defn move [board player pos]
  (when-not (occupied? (board pos))
    (let [directions (map #(- % pos) (neighbours board pos))
          flipped-pieces (apply union (map #(flip board player pos %)
                                           directions))]
      (when-not (= #{} flipped-pieces) ; faster than empty?
        (apply assoc board pos player
               (interleave flipped-pieces (repeat player)))))))

(defn moves
  "Returns all possible moves for the given player and board. The list
  will never be empty; if the player has no legal moves a move that does
  nothing will be returned."
  [[board player]]
  (let [mvs (for [y (range 1 (inc *board-size*))
                  x (range 1 (inc *board-size*))
                  :let [mv (move board player (+ x (* 10 y)))]
                  :when mv]
              [mv (opponent player)])]
    (if (empty? mvs)
      (list [board (opponent player)])
      mvs)))'

(defn legal-positions
  "returns all legal positions for the given player and board"
  [[board player]]
  (for [y (range 1 (inc *board-size*))
        x (range 1 (inc *board-size*))
        :let [mv (move board player [x y])]
        :when mv]
    [x y]))

(defn str->move [[str-player &  str-board]]
  (let [player ({\W white, \B black} str-player)
        board (vec (concat [0 0 0 0 0 0 0 0 0 0, 0]
                           (->> str-board
                                (map {\E empty, \O white, \X black})
                                (partition 8)
                                (interpose [0 0])
                                (flatten))
                           [0 0 0 0 0 0 0 0 0 0 0]))]
    [board player]))

(defn move->str [[prev-board p1] [board _]]
  (let [[[x y]] (seq (intersection (set (legal-positions [prev-board p1]))
                                   (difference (set (keys board)) (set (keys prev-board)))))]
    (if (nil? x)
      "pass"
      (str "(" (inc y) "," (inc x) ")"))))

(defn print-board [board]
  (dorun (map println (partition 10 board))))

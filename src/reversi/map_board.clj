(ns reversi.map-board
  (:use [clojure.set :only [union intersection difference]]))

(def *board-size* 8)

(def board {[3 3] \w, [4 3] \b,
            [3 4] \b, [4 4] \w})

(def black \b)
(def white \w)

(defn black? [player] (= player \b))

(defn print-ascii-board [board]
  (let [line (apply str "--" (repeat (* 2 *board-size*) \-))
        numbers (apply str " |" (interpose \| (range *board-size*)))]
    (println numbers)
    (println line)
    (doseq [y (range *board-size*)
            x (range *board-size*)]
      (when (zero? x) (print (str y \|)))
      (print (or (board [x y]) \space))
      (print \|)
      (when (= x 7)
        (println)
        (println line)))))

(defn print-board [board]
  (let [line (apply str "─┼" (repeat *board-size* "───┼"))
        numbers (apply str " │ " (interpose " │ " (range *board-size*)))
        pieces {\w " ○ ", \b " ● ", nil "   "}
        pad (fn [s] (case (count (str s))
                     1 (str \space s \space)
                     2 (str s \space)
                     s))]
    (println numbers)
    (println line)
    (doseq [y (range *board-size*)
            x (range *board-size*)]
      (when (zero? x) (print (str y "│")))
      (print (or (pieces (board [x y])) (pad (board [x y]))))
      (print "│")
      (when (= x 7)
        (println)
        (println line)))))

; (def pos- (partial mapv -))
(defn pos- [[a b] [c d]] [(- a c) (- b d)])
;(def pos+ (partial mapv +))
(defn pos+ [[a b] [c d]] [(+ a c) (+ b d)])

(defn neighbours
  "returns all occupied positions from [x-1 y-1] to [x+1 y+1]"
  [board [x y]]
  (for [a [(dec x) x (inc x)];(range (dec x) (+ x 2))
        b [(dec y) y (inc y)];(range (dec y) (+ y 2))
        :when (board [a b])]
    [a b]))

(def opponent {\w \b, \b \w})

(defn flip
  "Tries to flip pieces from (not including) pos in the given direction
   until a piece of the players color appears. dir is on the form [x y],
   i.e. [1 0] means east, [0 1] south, [-1 1] southwest and so on.
   Returns the set of flipped pieces (nil if no pieces can be flipped)."
  [board player pos dir]
  (loop [piece (pos+ pos dir)
         flipped-pieces #{}]
    (condp = (board piece)
      (opponent player) (recur (pos+ piece dir) (conj flipped-pieces piece))
      player            flipped-pieces
      nil               #{}))) ; reached the end without finding
                               ; a piece of the players color

(defn move [board player pos]
  (if (board pos) ; the position is already occupied
    nil
    (let [directions (map #(pos- % pos) (neighbours board pos))
          flipped-pieces (apply union (map #(flip board player pos %)
                                           directions))]
      (if (empty? flipped-pieces)
        nil
        (merge (assoc board pos player)
               (zipmap flipped-pieces (repeat player)))))))

(defn legal-positions
  "returns all legal positions for the given player and board"
  [[board player]]
  (for [y (range *board-size*)
        x (range *board-size*)
        :let [mv (move board player [x y])]
        :when mv]
    [x y]))

(defn moves
  "Returns all possible moves for the given player and board. The list
  will never be empty; if the player has no legal moves a move that does
  nothing will be returned."
  [[board player]]
  (let [mvs (for [y (range *board-size*)
                  x (range *board-size*)
                  :let [mv (move board player [x y])]
                  :when mv]
              [mv (opponent player)])]
    (if (empty? mvs)
      (list [board (opponent player)])
      mvs)))

(defn str->move [[str-player &  str-board]]
  (let [player ({\W \w, \B \b} str-player)
        board (into {} (remove (comp nil? second)
                               (zipmap (for [y (range *board-size*)
                                             x (range *board-size*)]
                                         [x y])
                                       (map {\E nil, \O \w, \X \b} str-board))))]
    [board player]))

(defn move->str [[prev-board p1] [board _]]
  (let [[[x y]] (seq (intersection (set (legal-positions [prev-board p1]))
                                   (difference (set (keys board)) (set (keys prev-board)))))]
    (if (nil? x)
      "pass"
      (str "(" (inc y) "," (inc x) ")"))))

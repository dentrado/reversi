(ns reversi.core)

;; TODO: tree, minimax, alphabeta, heuristics
;; tree: legal moves: inside bounds, and flips at least a piece

(def *board-size* 8)

(def board {[3 3] \b [4 3] \w
            [3 4] \w [4 4] \b})

(defn print-board [board]
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

;; (def pos- (partial mapv -))
(defn pos- [[a b] [c d]] [(- a c) (- b d)])
;;(def pos+ (partial mapv +))
(defn pos+ [[a b] [c d]] [(+ a c) (+ b d)])

(defn neighbours
  "returns all occupied positions from [x-1 y-1] to [x+1 y+1]"
  [board [x y]]
  (for [a (range (dec x) (+ x 2))
        b (range (dec y) (+ y 2))
        :when (board [a b])]
    [a b]))

(def opponent {\w \b, \b \w})

(defn flip
  "Tries to flip pieces from (not including) pos in the given direction until
   a piece of the players color appears. dir is on the form [x y],
   i.e. [1 0] means east, [0 1] south, [-1 1] southwest and so on.
   Returns the set of flipped pieces (empty set if no pieces can be flipped)."
  [board pos player dir]
  (loop [piece   (pos+ pos dir)
         flipped #{}]
    (condp = (board piece)
      (opponent player) (recur (pos+ piece dir) (conj flipped piece))
      player            flipped
      nil               #{})))   ; reached the end without finding
                                 ; a piece of the players color

(defn move [board player pos]
  (if (or (board pos) ; the position is already occupied
          (= (neighbours pos) 0))
    nil
    (let [directions (map #(pos- % pos) (neighbours pos))]

      )
    ))

(defn moves
  "returns all possible moves for the given player and board"
  [board player]
  (for [y (range *board-size*)
        x (range *board-size*)]
    ))

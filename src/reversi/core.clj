(ns reversi.core
  (:use [clojure.set :only [union]]))

;; TODO: tree, minimax, alphabeta, heuristics
;; tree: legal moves: inside bounds, and flips at least a piece

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(def *board-size* 8)

(def board {[3 3] \b, [4 3] \w,
            [3 4] \w, [4 4] \b})

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
   Returns the set of flipped pieces (nil if no pieces can be flipped)."
  [board player pos dir]
  (loop [piece   (pos+ pos dir)
         flipped-pieces #{}]
    (condp = (board piece)
      (opponent player) (recur (pos+ piece dir) (conj flipped-pieces piece))
      player            flipped-pieces
      nil               #{})))   ; reached the end without finding
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

(defn moves
  "returns all possible moves for the given player and board"
  [board player]
  (for [y (range *board-size*)
        x (range *board-size*)]
    [x y]))

(defn iter-tree
  "doc-string"
  [f a]
  [a (map #(iter-tree f %) (f a))])

(defn prune [n [a children]]
  (if (zero? n)
    [a nil]
    [a (map #(prune (dec n) %) children)]))

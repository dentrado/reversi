(ns reversi.core)

;; TODO: tree, minimax, alphabeta, heuristics
;; tree: legal moves: inside bounds, and flips at least a piece

(def *board-size* 8)

(def board {[3 3] \b [4 3] \w
            [3 4] \w [4 4] \b})

(defn print-board [board]
  (let [line (apply str (repeat (* 2 *board-size*) \-))]
    (println line)
    (doseq [y (range *board-size*)
            x (range *board-size*)]
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


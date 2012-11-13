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

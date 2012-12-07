(ns reversi.main
  (:use reversi.core)
  (:require [reversi.vector-board :as b]
            [reversi.vector-board.heuristic :as heur])
  (:gen-class))

;;(remove-ns 'reversi.main)

(defn usage []
  (println
   "Usage: program-name position time-limit

where position is a 65 character long string.
The first character should be W or B deciding which players turn it is, the 64
other characters represents the board and should be either E for empty,
O for white or X for black.
"))

(defn -main [move-str time-limit & args]
  (if-not (and move-str time-limit (zero? (count args)))
    (usage)
    (let [move (str->move move-str)
          next-move (first (ai-player-w-sort heur/position 7 (game-tree move)))]
      (println (move->str move next-move)))))

;;"WEEEEEEEEEEEEEEEEEEEEEEEEEEEOXEEEEEEXOEEEEEEEEEEEEEEEEEEEEEEEEEEE"

;;"EEEEEEEEEEEEEEEEEEEEEEEEEEEOOOEEEEEXOOEEEOOOXOEEEEEEEEEEEEEEEEEE"
;;"EEEEEEEEEEEEEEEEEEEEEEEEEEEOOOEEEEEXOOEEEOOOXOEEEEEEEEEEEEEEEEEE"

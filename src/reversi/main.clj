(ns reversi.main
  "Program entry point"
  (:use reversi.core)
  (:require [reversi.board :as b]
            [reversi.heuristic :as heur])
  (:gen-class))

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
    (let [depth (condp > (Integer/parseInt time-limit)
                  3  5,
                  10 6,
                  60 7,
                  8)
          move (b/str->move move-str)
          next-move (first (ai-player-w-sort heur/stability-heur depth
                                             (game-tree move)))]
      (println (b/move->str move next-move)))))

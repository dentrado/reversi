(ns reversi.main
  (:use [clojure.set :only [intersection difference]])
  (:use reversi.core)
  (:require [reversi.heuristic :as heur])
  (:gen-class))

(defn usage []
  (println
   "Usage: program-name position time-limit

where position is a 65 character long string.
The first character should be W or B deciding which players turn it is, the 64
other characters represents the board and should be either E for empty,
O for white or X for black.
"))

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
      (str "(" y "," x ")"))))

(defn -main [move-str time-limit & args]
  (if-not (and move-str time-limit (zero? (count args)))
    (usage)
    (let [move (str->move move-str)
          next-move (first (ai-player heur/position 7 (game-tree move)))]
      (move->str move next-move))))

;"WEEEEEEEEEEEEEEEEEEEEEEEEEEEOXEEEEEEXOEEEEEEEEEEEEEEEEEEEEEEEEEEE"

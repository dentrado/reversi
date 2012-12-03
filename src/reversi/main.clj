(ns reversi.main
  (:gen-class))

(defn usage []
  (print
   "Usage: program-name position time-limit

where position is a 65 character long string.
The first character should be W or B deciding which players turn it is, the 64
other characters represents the board and should be either E for empty,
O for white or X for black.
"))

(defn -main [position time-limit & args]
  (if-not (and position time-limit (zero? (count args)))
    (usage)
    nil ; TODO do stuff
    )

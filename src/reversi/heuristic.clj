(ns reversi.heuristic
  (:use [reversi.core :only [moves]]))

(def position-scores {[0 0] 99, [1 0]  -8, [2 0]  8, [3 0]  6,
                      [0 1] -8, [1 1] -24, [2 1] -4, [3 1] -3,
                      [0 2]  8, [1 2] -4,  [2 2]  7, [3 2]  4,
                      [0 3]  6, [1 3] -3,  [2 3]  4, [3 3]  0})

(defn score-position [[[x y] v]]
  (position-scores [(if (> x 3) (- 7 x) x)
                    (if (> y 3) (- 7 y) y)]))

(defn position-black [[board _]]
  (->> board
       (filter (fn [[k v]] (= v \b))) ; TODO black? and white? functions
       (map score-position)
       (apply +)))

(defn position [[board _]]
  (let [[blacks whites] (partition-by (comp identity second)
                                      (sort (fn [[k1 v1] [k2 v2]]
                                              (compare v1 v2))
                                            board))
        black-score (apply + (map score-position blacks))
        white-score (apply + (map score-position whites))]
    (- black-score white-score)))

(defn naive [[board _]]
  (count (filter #(= \b %) (vals board))))

(defn mobility [[board _]]
  (count (moves [board \b])))

(defn random [_] (rand-int 100))

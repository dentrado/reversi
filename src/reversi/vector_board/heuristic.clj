(ns reversi.vector-board.heuristic
  (:require [reversi.vector-board :as b]))

(let [a 99, b  -8, c  8, d  6
      ,     e -24, f -4  g -3
      ,            h  7, i  4
      ,                  j  0]
  (def position-scores
    [0 0 0 0 0 0 0 0 0 0
     0 a b c d d c b a 0
     0 b e f g g f e b 0
     0 c f h i i h f c 0
     0 d g i j j i g d 0
     0 d g i j j i g d 0
     0 c f h i i h f c 0
     0 b e f g g f e b 0
     0 a b c d d c b a 0
     0 0 0 0 0 0 0 0 0 0 ]))

(defn position [[board _]]
  (reduce (fn [acc pos]
            (condp = (board pos)
              b/black (+ acc (position-scores pos))
              b/white (- acc (position-scores pos))
              acc))
          0
          (range 11 89)))

(defn naive [[board _]]
  (count (filter #(= \b %) (vals board))))

(defn mobility [[board _]]
  (count (b/moves [board b/black])))

(defn random [_] (rand-int 100))

;; old
(comment
  (defn position [[board _]]
    (let [[empties blacks whites borders] (partition-by (comp identity second)
                                                        (sort (fn [[k1 v1] [k2 v2]]
                                                                (compare v1 v2))
                                                              (map vector (range) board)))
          black-score (reduce + (map score-position blacks))
          white-score (reduce + (map score-position whites))]
      (- black-score white-score))))

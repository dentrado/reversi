(ns reversi.heuristic
  (:use [clojure.math.combinatorics :only [selections]])
  (:require [reversi.board :as b]))

(defn random [_] (rand-int 100))

(defn naive [[board _]]
  (- (count (filter b/black? board))
     (count (filter b/white? board))))

;; # Postion strategy
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

(defn position
  [[board _]]
  (reduce (fn [acc pos]
            (condp = (board pos)
              b/black (+ acc (position-scores pos))
              b/white (- acc (position-scores pos))
              acc))
          0
          (range 11 89)))

;; # Mobility strategy
(defn mobility [[board _]]
  (- (count (b/moves [board b/black]))
     (count (b/moves [board b/white]))))

;; # Edge stability strategy

;; Common names for edge positions:
;;  │ a │ b │ c │ d │ e │ f │ g │ h │
;; ─┼───┼───┼───┼───┼───┼───┼───┼───┼
;; 0│   │ C │ A │ B │ B │ A │ C │   │
;; ─┼───┼───┼───┼───┼───┼───┼───┼───┼
;; 1│ C │ X │   │   │   │   │ X │ C │
;; ─┼───┼───┼───┼───┼───┼───┼───┼───┼
;; 2│ A │   │   │   │   │   │   │ A │
;; ─┼───┼───┼───┼───┼───┼───┼───┼───┼
;; 3│ B │   │   │   │   │   │   │ B │
;; ─┼───┼───┼───┼───┼───┼───┼───┼───┼
;; 4│ B │   │   │   │   │   │   │ B │
;; ─┼───┼───┼───┼───┼───┼───┼───┼───┼
;; 5│ A │   │   │   │   │   │   │ A │
;; ─┼───┼───┼───┼───┼───┼───┼───┼───┼
;; 6│ C │ X │   │   │   │   │ X │ C │
;; ─┼───┼───┼───┼───┼───┼───┼───┼───┼
;; 7│   │ C │ A │ B │ B │ A │ C │   │
;; ─┼───┼───┼───┼───┼───┼───┼───┼───┼

(defn find-first [pred coll] (first (filter pred coll)))

(defn corner? [pos] (or (= pos 11) (= pos 18)))
(defn x-square? [pos] (or (= pos 22) (= pos 27)))
(def adjacent-corner {22 11, 27 18})

(def edge-position-weights
  [;stable semi unstable
   [-      0    -2000] ; X
   [700    -        -] ; corner
   [1200   200    -25] ; C
   [1000   200     75] ; A
   [1000   200     50] ; B
   [1000   200     50] ; B
   [1000   200     75] ; A
   [1200   200    -25] ; C
   [700    -        -] ; corner
   [-      0    -2000] ; X
   ])

(def edges [[22 11 12 13 14 15 16 17 18 27] ;top
            [22 11 21 31 41 51 61 71 81 72] ;left
            [72 81 82 83 84 85 86 87 88 77] ;bottom
            [77 88 78 68 58 48 38 28 18 27] ;right
            ])

(def top-edge (first edges))


(defn edge-pos-stability
  "Calculates the positions stability on the given board. The position
  must be on the top edge of the board, or one of the top x-squares
  (22 or 27). Returns 0 for stable, 1 for semi-stable and 2 for
  unstable"
  [board pos]
  (let [stable 0, semi-stable 1, unstable 2]
    (cond (corner? pos) stable
          (x-square? pos) (if (b/empty? (board
                                         (adjacent-corner pos)))
                            unstable
                            semi-stable)
          :else
          (let [player      (board pos)
                not-player? #(not= player %)
                left        (find-first not-player? (map board (range pos 9 -1)))
                right       (find-first not-player? (map board (range pos 20)))
                row-full?   (not-any? b/empty? (map board (range 11 19)))]
            (cond
             row-full?                               stable,
             (and (b/empty? left)  (b/outer? right)) stable,
             (and (b/empty? right) (b/outer? left))  stable,
             (= left right (b/opponent player))      semi-stable,
             (= left right b/empty)                  semi-stable,
             ;; else one is empty and one opponent:
             :else                                   unstable)))))

(defn edge-stability [board player]
  (apply +
         (map (fn [pos pos-weights] ; three weights: [stable semi un]
                (condp = (board pos)
                  player                 (pos-weights
                                          (edge-pos-stability board pos))
                  (b/opponent player) (- (pos-weights
                                          (edge-pos-stability board pos)))
                  0)) ; empty pos
              top-edge
              edge-position-weights)))

(defn init-edge-stability-map []
  (let [possible-edge-vals (selections [b/empty b/black b/white] 10)]
    (into {}
     (for [edge-vals possible-edge-vals]
       (let [edge-board (apply assoc b/initial-board
                               (interleave top-edge
                                           edge-vals))
             stability-board (edge-stability edge-board b/black)]
         [edge-vals stability-board])))))

(def edge-stability-map
  "TODO: bad name change it"
  (init-edge-stability-map))

(defn stability-heur [[board _]]
  (apply +
         (for [edge edges]
           (edge-stability-map (map board edge)))))

(comment
  (edge-stability-map (map (assoc  b/initial-board 11 b/white) top-edge))
  (stability-heur  [(assoc  b/initial-board 22 b/white) b/black])

(count edge-stability-map)

  (edge-stability (assoc b/initial-board  11 b/black 12 b/black)  b/black)
 (edge-pos-stability (assoc b/initial-board  11 b/black 12 b/black)
                     12)

 (range 12 19 -1)
 (range 12 10 -1))

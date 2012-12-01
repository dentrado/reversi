(ns reversi.core-test
  (:use clojure.test
        reversi.core)
  (:require [clojure.zip :as z]))

(def board1 {[3 3] \b [4 3] \w
             [3 4] \w [4 4] \b})

(def board2 {         [2 2] \w [3 2] \w [4 2] \b [5 2] \w
             [1 3] \w [2 3] \w
             [1 4] \w          [3 4] \w
                                        [4 5] \b})

(deftest flip-test
  (is (= (assoc board1 [2 3] \w [3 3] \w))
      (flip board1 \w [2 3] [1 0]))
  (is (= (assoc board1 [3 5] \b [3 4] \b))
      (flip board1 \b [3 5] [0 1])))

(deftest move-test
  (testing "no neighbours -> no move (nil)"
    (is (= (move board2 \w [0 0]) nil)))
  (testing "pos already occupied -> no move"
    (is (= (move board2 \b [2 2]) nil)))
  (testing "flip only lines ending with a piece of the players color"
    (is (= (move board2 \b [1 2])
           {[1 2] \b [2 2] \b [3 2] \b [4 2] \b [5 2] \w
            [1 3] \w [2 3] \b
            [1 4] \w          [3 4] \b
            ,                          [4 5] \b} ))))


;; test if alphabeta expands less nodes than minimax

(deftest alphabeta-test
  (let [minimax-expanded (atom 0)    ; 65005
        alphabeta-expanded (atom 0)] ; 64943 64931
    (->> (game-tree board \w)
         (prune 7)
         (map-tree #(do (swap! minimax-expanded inc)
                        (naive %)
                        ;(rand-int 10)
                        ))
         maxi)
    (->> (game-tree board \w)
        (prune 7)
        (map-tree #(do (swap! alphabeta-expanded inc)
                       (naive %)
                       ;(rand-int 10)
                       ))
        lowfirst  ;64293
        maximise*
        (apply max))
    (println "minimax expanded:" @minimax-expanded)
    (println "alphabeta-expanded:" @alphabeta-expanded)
    (< @alphabeta-expanded @minimax-expanded)))

(deftest alphabeta-test2
  (let [minimax-expanded (atom 0)    ; 65005
        alphabeta-expanded (atom 0)] ; 64943 64931
    (->> (game-tree board \w)
         (prune 6)
         (map-tree #(do (swap! minimax-expanded inc)
                        ;(naive %)
                        % 1
                        ;(rand-int 10)
                        ))
         maxi
         (println "minimax result:"))

    (->> (game-tree board \w)
        (prune 6)
        (map-tree #(do (swap! alphabeta-expanded inc)
                       ;(naive %)
                       ;(rand-int 10)
                       % 1
                       ))
        ;lowfirst  ;64293
        maximise*
        (apply max)
        (println "alpha-beta result:"))
    (println "minimax expanded:" @minimax-expanded)
    (println "alphabeta-expanded:" @alphabeta-expanded)
    (< @alphabeta-expanded @minimax-expanded)))

(deftest min-maxs-max-mins-test
  (testing "Testing laziness of min-maxs and max-mins"
    (let [min-maxs-count (atom 0)
          max-mins-count (atom 0)]
      (min-maxs (take 4 (repeatedly
                         #(take 3 (repeatedly
                                   (fn [] (swap! min-maxs-count inc) 1))))))
      (max-mins (take 4 (repeatedly
                         #(take 3 (repeatedly
                                   (fn [] (swap! max-mins-count inc) 1))))))
      (is (= 6 @min-maxs-count @max-mins-count)))))

(defn game-zip [root]
  (z/zipper vector? second (fn [n c] [n c]) root))

(def root-loc (game-zip (game-tree board1 \w)))

(defn print-loc [loc]
  (print-board (first (first (z/node loc))))
  loc)

(defn ignore [x])

(defn print-game-depth-first
  [depth zipper]
  (doseq [x (iterate z/down zipper)]
    (print-loc x))
  nil)

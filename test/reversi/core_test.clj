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

(defn game-zip [root]
  (z/zipper vector? second (fn [n c] [n c]) root))

(def root-loc (game-zip (game-tree [test-board \w])))

(defn print-loc [loc]
  (print-board (first (first (z/node loc))))
  loc)

(defn ignore [x])

(defn print-game-depth-first
  [depth zipper]
  (doseq [x (iterate z/down zipper)]
    (print-loc x))
  nil)

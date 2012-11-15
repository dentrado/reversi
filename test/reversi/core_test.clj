(ns reversi.core-test
  (:use clojure.test
        reversi.core))

(def test-board {[3 3] \b [4 3] \w
                 [3 4] \w [4 4] \b})

(deftest flip-test
  (is (= (assoc test-board [2 3] \w [3 3] \w))
      (flip test-board \w [2 3] [1 0]))
  (is (= (assoc test-board [3 5] \b [3 4] \b))
      (flip test-board \b [3 5] [0 1])))

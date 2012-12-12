(ns reversi.core
  "Functions for tying things togheter and playing games"
  (:use [reversi.search :only [maxi mini maximise* minimise*
                               highfirst lowfirst]]
        [reversi.board :only [moves black? legal-positions print-board]]
        [reversi.heuristic :only [naive]]))

;; # Game tree generation and manipulation
(defn iter-tree
  "Like iterate but returns a (lazy) tree instead of a seq.
   (f a) should return a collection. Each node in the tree is
   on the form: [a (map #(iter-tree f %) (f a))]."
  [f x]
  [x (map #(iter-tree f %) (f x))])

(defn game-tree
  "Returns a lazy tree of all possible moves"
  [[board player]]
  (iter-tree moves [board player]))

(defn prune
  "Prune tree at depth n"
  [n [val subtrees]]
  (if (zero? n)
    [val nil]
    [val (map #(prune (dec n) %) subtrees)]))

(defn map-tree [f [val subtrees]]
  [(f val) (map #(map-tree f %) subtrees)])

;; # Players
(defn minimax-player [heuristic-fn depth game-tree]
  (let [[[board player] subtrees] game-tree
        counter (atom 0)
        tree (map-tree #(do (swap! counter inc)
                            (heuristic-fn %))
                       (prune depth game-tree))
        best-val (if (black? player)
                (maxi tree)
                (mini tree))]
    (println "minimax expanded: " @counter)
    ;(nth subtrees (.indexOf tree2 best-val))
    ))

(defn ai-player-w-sort [heuristic-fn depth game-tree]
  (let [[[board player] subtrees] game-tree
        tree (map-tree heuristic-fn (prune depth game-tree))
        tree2 (if (black? player)
                (maximise* (highfirst tree))
                (minimise* (lowfirst tree)))
        best-val (apply (if #(black? player) max min) tree2)]
    (nth subtrees (.indexOf tree2 best-val))))

(defn ai-player-w-expand-count [heuristic depth game-tree]
  (let [[[board player] subtrees] game-tree
        counter (atom 0)
        tree (map-tree #(do (swap! counter inc)
                            (heuristic %))
                       (prune depth game-tree))
        tree2 (if (black? player)
                (maximise* tree)
                (minimise* tree))
        best-val (apply (if #(black? player) max min) tree2)]
    (println "alpha-beta expanded: " @counter)
    (println "heuristic score: " best-val)
    (nth subtrees (.indexOf tree2 best-val))))

(defn human-player [[[board player] subtrees]]
  (println "You are " (if (= \b player) "black" "white"))
  (println "Choose a move:")
  (print-board
   (apply assoc board (interleave
                       (legal-positions [board player])
                       (range))))
  (loop [move (read-string (read-line))]
    (if (< -1 move (count subtrees))
      (nth subtrees move)
      (do (println "Illegal move.")
          (recur (read-string (read-line)))))))

;; # Functions for playing games
(defn game-over? [[val subtrees]]
  (let [[val2 subtrees2] (first subtrees)
        [val3 _]         (first subtrees2)]
    (= val val3)))

(defn score
  "calculates the score (num black pieces - num white pieces)"
  [board]
  (naive [board nil]))

(defn winner
  "Returns the winner (\\b or \\w) or nil on tie."
  [board]
  (case (Integer/signum (score board))
   -1 \w
    1 \b
    0 nil))

(defn game
  "Play a game between player1 and player2 starting from the root of the
  game-tree. Returns the final score. A positive score means that
  player1 won, a negative that player2 won."
  [player1 player2 game-tree]
  (let [next-tree (player1 game-tree)]
    (if (game-over? next-tree)
      (score (ffirst next-tree))
      (game player2 player1 next-tree))))

(defn random-game-series
  "Play n-pairs pairs of games, players swapping color. Each pair of
  games starts from a random position. Returns the score for each
  game. A positive score means that player1 won, a negative that player2
  won."
  [player1 player2 n-pairs game-tree]
  (flatten
   (for [tree (take n-pairs
                    (repeatedly
                     #(nth (iterate (comp rand-nth second) game-tree)
                           10)))]
     (do (print-board (ffirst tree))
         [(game player1 player2 tree)
          (- (game player2 player1 tree))]))))

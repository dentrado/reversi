(ns reversi.core
  (:use [clojure.set :only [union]]))

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(def *board-size* 8)

(def board {[3 3] \w, [4 3] \b,
            [3 4] \b, [4 4] \w})

(defn print-ascii-board [board]
  (let [line (apply str "--" (repeat (* 2 *board-size*) \-))
        numbers (apply str " |" (interpose \| (range *board-size*)))]
    (println numbers)
    (println line)
    (doseq [y (range *board-size*)
            x (range *board-size*)]
      (when (zero? x) (print (str y \|)))
      (print (or (board [x y]) \space))
      (print \|)
      (when (= x 7)
        (println)
        (println line)))))

(defn print-board [board]
  (let [line (apply str "─┼" (repeat *board-size* "───┼"))
        numbers (apply str " │ " (interpose " │ " (range *board-size*)))
        pieces {\w " ○ ", \b " ● ", nil "   "}
        pad (fn [s] (case (count (str s))
                     1 (str \space s \space)
                     2 (str s \space)
                     s))]
    (println numbers)
    (println line)
    (doseq [y (range *board-size*)
            x (range *board-size*)]
      (when (zero? x) (print (str y "│")))
      (print (or (pieces (board [x y])) (pad (board [x y]))))
      (print "│")
      (when (= x 7)
        (println)
        (println line)))))

; (def pos- (partial mapv -))
(defn pos- [[a b] [c d]] [(- a c) (- b d)])
;(def pos+ (partial mapv +))
(defn pos+ [[a b] [c d]] [(+ a c) (+ b d)])

(defn neighbours
  "returns all occupied positions from [x-1 y-1] to [x+1 y+1]"
  [board [x y]]
  (for [a (range (dec x) (+ x 2))
        b (range (dec y) (+ y 2))
        :when (board [a b])]
    [a b]))

(def opponent {\w \b, \b \w})

(defn flip
  "Tries to flip pieces from (not including) pos in the given direction
   until a piece of the players color appears. dir is on the form [x y],
   i.e. [1 0] means east, [0 1] south, [-1 1] southwest and so on.
   Returns the set of flipped pieces (nil if no pieces can be flipped)."
  [board player pos dir]
  (loop [piece (pos+ pos dir)
         flipped-pieces #{}]
    (condp = (board piece)
      (opponent player) (recur (pos+ piece dir) (conj flipped-pieces piece))
      player            flipped-pieces
      nil               #{}))) ; reached the end without finding
                               ; a piece of the players color

(defn move [board player pos]
  (if (board pos) ; the position is already occupied
    nil
    (let [directions (map #(pos- % pos) (neighbours board pos))
          flipped-pieces (apply union (map #(flip board player pos %)
                                           directions))]
      (if (empty? flipped-pieces)
        nil
        (merge (assoc board pos player)
               (zipmap flipped-pieces (repeat player)))))))

(defn legal-positions
  "returns all legal positions for the given player and board"
  [[board player]]
  (for [y (range *board-size*)
        x (range *board-size*)
        :let [mv (move board player [x y])]
        :when mv]
    [x y]))

(defn moves
  "Returns all possible moves for the given player and board. The list
  will never be empty; if the player has no legal moves a move that does
  nothing will be returned."
  [[board player]]
  (let [mvs (for [y (range *board-size*)
                  x (range *board-size*)
                  :let [mv (move board player [x y])]
                  :when mv]
              [mv (opponent player)])]
    (if (empty? mvs)
      (list [board (opponent player)])
      mvs)))

(defn game-over? [[val subtrees]]
  (let [[val2 subtrees2] (first subtrees)
        [val3 _]         (first subtrees2)]
    (= val val3)))

(defn winner
  "Returns the winner (b or w) or nil on tie."
  [board]
  (let [[blacks whites] (->> (sort (vals board))
                             (partition-by identity)
                             (map count))
        [blacks whites] [(or blacks 0) (or whites 0)]]
    (cond
     (< blacks whites) \w
     (> blacks whites) \b
     :else nil))) ; tie

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

;; # Heuristics
(defn naive [[board _]]
  (count (filter #(= \b %) (vals board))))

(defn possibilities-heuristic [[board _]]
  (count (moves [board \b])))

(defn random [_] (rand-int 100))

;; # Minimax
(declare maxi mini)

(defn maxi [[val subtrees]]
  (if (empty? subtrees)
    val
    (apply max (map mini subtrees))))

(defn mini [[val subtrees]]
  (if (empty? subtrees)
    val
    (apply min (map maxi subtrees))))

;; # Alpha-beta pruning
(defn some<=n? [n nums] ; Doesn't look at all numbers if it isn't necessary
  (some #(<= % n) nums))

(defn some>=n? [n nums] ; Doesn't look at all numbers if it isn't necessary
  (some #(>= % n) nums))

(defn mapomit
  "maps f over the coll but omits values for which (pred (f previous-val) val)
   returns true (previous-val is the last previous value that was not omitted)."
  ([pred f [val & more]]
     (when val
       (let [start-val (f val)]
         (cons start-val (mapomit pred f start-val more)))))
  ([pred f start-val [val & more]]
     (when val
       (lazy-seq
        (if (pred start-val val)
          (mapomit pred f start-val more)
          (let [new-val (f val)]
            (cons new-val (mapomit pred f new-val more))))))))

(defn max-mins
  "Takes a list of lists of numbers and applies min on the lists
   but skips lists wich contains numbers smaller than the largest
   min-value of the previous lists."
  [num-lists]
  (mapomit some<=n? #(apply min %) num-lists))

(defn min-maxs
  "Takes a list of lists of numbers and applies max on the lists
   but skips lists wich contains numbers larger than the smallest
   max-value of the previous lists."
  [num-lists]
  (mapomit some>=n? #(apply max %) num-lists))

(declare minimise*)
(defn maximise* [[val subtrees]]
  (lazy-seq (if (empty? subtrees)
              (list val)
              (max-mins (map minimise* subtrees)))))
(defn minimise* [[val subtrees]]
  (lazy-seq (if (empty? subtrees)
              (list val)
              (min-maxs (map maximise* subtrees)))))

;; # Code for playing a game:

(declare lowfirst)
(defn highfirst [[val subtrees]]
  [val (lazy-seq (sort #(> (first %1) (first %2))
                       (map lowfirst subtrees)))])
(defn lowfirst [[val subtrees]]
  [val (lazy-seq (sort #(< (first %1) (first %2))
                       (map highfirst subtrees)))])

(lowfirst [3 (list [2 (list)] [1 (list)])])

(defn minimax-player [[[board player] subtrees :as game-tree]]
  (let [counter (atom 0)
        tree (map-tree #(do (swap! counter inc)
                            ;(naive %)
   ;                         (possibilities-heuristic %)
                            % (rand-int 100)
                            )
                       (prune 7 game-tree))
        best-val (if (= player \b)
                (maxi tree)
                (mini tree))]
    (println "minimax expanded: " @counter)
    ;(nth subtrees (.indexOf tree2 best-val))
    ))

(defn ai-player-w-sort [[[board player] subtrees :as game-tree]]
  (let [counter (atom 0)
        tree (map-tree #(do (swap! counter inc)
                            (naive %)
                            ;(possibilities-heuristic %)
                            )
                       (prune 7 game-tree))
        tree2 (if (= player \b)
                (maximise* (lowfirst tree))
                (minimise* (highfirst tree)))
        best-val (apply (if #(= player \b) max min) tree2)]
    (println "with-sort expanded: " @counter)
    (nth subtrees (.indexOf tree2 best-val))))

(defn ai-player [[[board player] subtrees :as game-tree]]
  (let [counter (atom 0)
        tree (map-tree #(do (swap! counter inc)
                            ;(naive %)
                            (possibilities-heuristic %)
                             )
                       (prune 5 game-tree))
        tree2 (if (= player \b)
                (maximise* tree)
                (minimise* tree))
        best-val (apply (if #(= player \b) max min) tree2)]
;    (ai-player-w-sort game-tree)
    (println "alpha-beta expanded: " @counter)
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
          (recur (read-string (read-line)))))
    ))

(declare game-ai-starts)
(defn game-human-starts [game-tree]
  (game-ai-starts (human-player game-tree)))
(defn game-ai-starts [game-tree]
  (game-human-starts (ai-player game-tree)))

(defn game [player1 player2 game-tree]
  (let [next-tree (player1 game-tree)]
    (if (game-over? next-tree)
      (do
        (println "The winner is" ({\b "black" \w "white"} (winner (ffirst next-tree))))
        (ffirst next-tree))
      (game player2 player1 next-tree))))

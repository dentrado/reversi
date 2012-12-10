(ns reversi.core
  (:use [reversi.vector-board :only [moves black? legal-positions print-board]]
        [reversi.vector-board.heuristic :only [naive]]))
;(remove-ns 'reversi.core)

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

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

;; # Minimax
(declare mini)

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
  ;; Note: We can't use destructuring ([val & more-vals]) since that will force
  ;; the first element in more-vals, and the alpha-beta pruning won't be optimal.
  ([pred f vals]
     (when vals
       (let [start-val (f (first vals))]
         (cons start-val (lazy-seq (mapomit pred f start-val (next vals)))))))
  ([pred f start-val vals]
     (when vals
       (if (pred start-val (first vals))
         (mapomit pred f start-val (next vals))
         (let [new-val (f (first vals))]
           (cons new-val (lazy-seq (mapomit pred f new-val (next vals)))))))))

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

(declare lowfirst)
(defn highfirst [[val subtrees]]
  [val (lazy-seq (sort #(> (first %1) (first %2))
                       (map lowfirst subtrees)))])
(defn lowfirst [[val subtrees]]
  [val (lazy-seq (sort #(< (first %1) (first %2))
                       (map highfirst subtrees)))])

;; # Code for playing a game:

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

(defn ai-player [heuristic depth game-tree]
  (let [[[board player] subtrees] game-tree
        counter (atom 0)
        tree (map-tree #(do (swap! counter inc)
                            (heuristic %))
                       (prune depth game-tree))
        tree2 (if (black? player)
                (maximise* tree)
                (minimise* tree))
        best-val (apply (if #(black? player) max min) tree2)]
             ;    (ai-player-w-sort game-tree)
;    (binding [*out* (java.io.PrintWriter. System/err)])
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


;; #
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

(defn game [player1 player2 game-tree]
  (let [next-tree (player1 game-tree)]
    (if (game-over? next-tree)
      (do
        (println "The winner is" ({\b "black" \w "white"} (winner (ffirst next-tree))))
        (ffirst next-tree))
      (game player2 player1 next-tree))))

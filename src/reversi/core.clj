(ns reversi.core
  (:use [clojure.set :only [union]]))

;; TODO: tree ✓, minimax, alphabeta, heuristics

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(def *board-size* 8)

(def board {[3 3] \b, [4 3] \w,
            [3 4] \w, [4 4] \b})

(defn print-board [board]
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

;; (def pos- (partial mapv -))
(defn pos- [[a b] [c d]] [(- a c) (- b d)])
;;(def pos+ (partial mapv +))
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
  "Tries to flip pieces from (not including) pos in the given direction until
   a piece of the players color appears. dir is on the form [x y],
   i.e. [1 0] means east, [0 1] south, [-1 1] southwest and so on.
   Returns the set of flipped pieces (nil if no pieces can be flipped)."
  [board player pos dir]
  (loop [piece (pos+ pos dir)
         flipped-pieces #{}]
    (condp = (board piece)
      (opponent player) (recur (pos+ piece dir) (conj flipped-pieces piece))
      player            flipped-pieces
      nil               #{})))   ; reached the end without finding
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

(defn moves
  "returns all possible moves for the given player and board"
  [[board player]]
  (for [y (range *board-size*)
        x (range *board-size*)
        :let [mv (move board player [x y])]
        :when mv]
    [mv (opponent player)]))

(defn iter-tree
  "Like iterate but returns a (lazy) tree instead of a seq.
   (f a) should return a collection. Each node in the tree is
   on the form: [a (map #(iter-tree f %) (f a))]."
  [f x]
  [x (map #(iter-tree f %) (f x))])

(defn game-tree
  "Returns a lazy tree of all possible moves"
  [board player]
  (iter-tree moves [board player]))

(defn prune
  "Prune tree at depth n"
  [n [val subtrees]]
  (if (zero? n)
    [val nil]
    [val (map #(prune (dec n) %) subtrees)]))

(defn map-tree [f [val subtrees]]
  [(f val) (map #(map-tree f %) subtrees)])

(defn naive [[board player]]
  (count (filter #(= \b %) (vals board))))

(declare maxi mini)

(defn maxi [[val subtrees]]
  (if (empty? subtrees)
    val
    (apply max (map mini subtrees))))

(defn mini [[val subtrees]]
  (if (empty? subtrees)
    val
    (apply min (map maxi subtrees))))

(defn some<=n? [n nums] ; Doesn't look at all numbers if it isn't necessary
  (some #(<= % n) nums))

(defn some>=n? [n nums] ; Doesn't look at all numbers if it isn't necessary
  (some #(>= % n) nums))

(defn mapomit
  "maps f over the list but omits values for which (pred (f previous-val) val)
   returns true (previous-val is the last previous value that was not omitted)."
  ([pred f list] (mapomit pred f (f (first list)) (rest list)))
  ([pred f start-val list]
     (when-let [[val & rest] list]
       (if (pred start-val val)
         (recur pred f start-val rest)
         (let [new-val (f val)]
           (lazy-seq (cons new-val (mapomit pred f new-val rest))))))))

(defn mapmin
  "Takes a list of lists of numbers and applies min on the lists
   but skips lists wich contains numbers smaller than the largest
   min-value of the previous lists."
  [num-lists]
  (mapomit some<=n? #(apply min %) num-lists))

(defn mapmax
  "Takes a list of lists of numbers and applies max on the lists
   but skips lists wich contains numbers larger than the smallest
   max-value of the previous lists."
  [num-lists]
  (mapomit some>=n? #(apply max %) num-lists))
(comment
  (->> (game-tree board \w)
       (prune 2)
       (clojure.walk/prewalk-demo)))

;; Unused:
(defn reduce-tree [f g val [a subtrees]]
  (f a
     (if (empty? subtrees)
       val
       (reduce g (map #(reduce-tree f g val %) subtrees)))))

(defn map-tree [f tree]
  (reduce-tree #(vector (f %1) %2) cons nil tree))

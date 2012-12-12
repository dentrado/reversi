(ns reversi.search)

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

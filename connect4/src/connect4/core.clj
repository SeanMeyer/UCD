(ns connect4.core
  (:gen-class)
  (:require [clojure.data.json :as json]
            [clojure.set :refer :all]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]))

(def precept
  {:height 6
   :player 1
   :grid [[0 0 0 0 0 0][0 0 0 0 0 2][0 0 0 0 0 0][0 0 0 1 2 1][0 0 0 0 0 0][0 0 0 0 0 0][0 0 0 0 0 0]]
   :width 7})


(def inf Double/POSITIVE_INFINITY)
(def -inf Double/NEGATIVE_INFINITY)

(defn stderr
  [err-text]
  (binding [*out* *err*]
    (println err-text)))

(defn indices
  "Sequences of indicies for which a predicate is satisfied"
  [pred coll]
  (keep-indexed #(when (pred %2) %1) coll))

(defn indices-of-zeroes
  [coll]
  (indices zero? coll))

(defn valid-moves
  [grid]
  (indices-of-zeroes (map first grid)))

(defn perform-move
  [move grid player]
  (let [grid-col (nth grid move)
        new-col (assoc grid-col (last (indices-of-zeroes grid-col)) player)]
    (assoc grid move new-col)))

(defn rand-move
  [moves]
  (nth moves (rand-int (count moves))))

(defn find-best-move-rand
  [valid-moves precept]
  (rand-move valid-moves))

(defn index-columns
  "Takes grid and returns list-grid with values replaced by list: (y-pos value)"
  [grid]
  (map #(reverse (map-indexed list (reverse %))) grid))

(defn index-rows
  "Takes grid and returns list-grid with each column replaced by list: (x-pos (column))"
  [grid]
  (map-indexed list grid))

(defn index-grid
  "Given a grid, returns list-grid with values replaced by list: (x-pos y-pos value)"
  [grid]
  (let [indexed-columns (index-columns grid)
        all-indexed (index-rows indexed-columns)]
    (map (fn [[x-idx col]]
           (map #(conj % x-idx) col))
         all-indexed)))

(defn get-positions
  "Returns set of current occupied positions by player, of form (x-pos y-pos)"
  [grid player]
  (let [flat-grid (apply concat (index-grid grid))]
    (into #{} (map butlast (filter #(= (last %) player) flat-grid)))))

(defn available?
  [grid height width positions [x y]]
  (cond
    (contains? positions (list x y)) true
    (or (< x 0) (> x (dec width)) (< y 0) (> y (dec height))) false
    (= 0 ((grid x) (- (dec height) y))) true
    :else false))

(defn all-rows-accumulator
  [grid positions]
  (fn [acc pos]
    (if (= (count acc) 4)
      (reduced acc)
      (if (available? grid 6 7 positions pos)
        (conj acc pos)
        (reduced acc)))))

(defn current-rows-accumulator
  [positions]
  (fn [acc pos]
    (if (contains? positions pos)
      (conj acc pos)
      (reduced acc))))

(defn get-rows
  [position accumulator order]
  (let [inc (if (= order :backwards) dec inc)
        dec (if (= order :backwards) #(+ 1 %) dec)
        horizontal+ (reduce accumulator #{}
                            (iterate (fn [[x y]] (list (inc x) y)) position))
        horizontal (reduce accumulator horizontal+
                           (iterate (fn [[x y]] (list (dec x) y)) position))
        vertical+ (reduce accumulator #{}
                          (iterate (fn [[x y]] (list x (inc y))) position))
        vertical (reduce accumulator vertical+
                         (iterate (fn [[x y]] (list x (dec y))) position))
        lr-diag+ (reduce accumulator #{}
                         (iterate (fn [[x y]] (list (inc x) (inc y))) position))
        lr-diag (reduce accumulator lr-diag+
                        (iterate (fn [[x y]] (list (dec x) (dec y))) position))
        rl-diag+ (reduce accumulator #{}
                         (iterate (fn [[x y]] (list (dec x) (inc y))) position))
        rl-diag (reduce accumulator rl-diag+
                        (iterate (fn [[x y]] (list (inc x) (dec y))) position))]
    {:h horizontal :v vertical :lr  lr-diag :rl rl-diag}))

(defn reduce-to-wins
  [positions fb-row]
  (let [wins (filter #(= (count %) 4) fb-row)
        sorted-wins (sort-by count
                             (map #(clojure.set/difference % positions)
                                  wins))]
    (cond
      (empty? sorted-wins) nil
      (= 1 (count sorted-wins)) sorted-wins
      :else (if (clojure.set/subset? (first sorted-wins) (second sorted-wins))
              (list (first sorted-wins))
              sorted-wins))))

(defn win-move-sets
  [grid positions]
  (reduce
    (fn [wins position]
      (let [f-rows (get-rows
                     position
                     (all-rows-accumulator grid positions)
                     :forwards)
            b-rows (get-rows
                     position
                     (all-rows-accumulator grid positions)
                     :backwards)
            fb-rows (merge-with conj
                                {:h '() :v '() :lr '() :rl '()}
                                f-rows
                                b-rows)
            position-wins (flatten
                            (remove nil?
                                    (map #(reduce-to-wins positions %)
                                         (vals fb-rows))))]
        (into wins position-wins)))
    #{} positions))

(defn to-subsets
  "Takes a sequence of sets ordered from largest to smallest"
  [ coll]
  (loop [result () coll coll]
    (if (empty? coll) result
                      (let  [x  (first coll)
                             xs (rest coll)]
                        (if (some #(clojure.set/superset? x %) xs)
                          (recur result xs)
                          (recur (cons x result) xs))))))

(defn remove-supersets
  [win-sets]
  (to-subsets (reverse (sort-by count win-sets))))

(defn filter-subsets
  [wins subsets]
  (reduce (fn [wins subset]
            (filter #(not (clojure.set/superset?
                            % subset)) wins))
          wins subsets))

(defn filter-col-wins
  [wins]
  (reduce (fn [wins win]
            (if (= 1 (count win))
              (filter-subsets wins
                              (let [[x y] (first win)]
                                (map (fn [y] #{(list x y)})
                                     (remove #(= y %) (filter (if (even? y) even? odd?) (range 0 8))))))
              (reduced wins)))
          wins wins))

(defn value-wins
  [board-height win-set]
  (let [win-len (count win-set)
        heights (map inc (map second win-set))
        avg-height (if (zero? win-len) 1 (/ (apply + heights) win-len))
        height-value (/ board-height avg-height)]
    (case win-len
      0 inf
      1 (* height-value 50)
      2 (* height-value 10)
      3 (* height-value 1))))

(defn eval-by-positions-needed
  [grid positions]
  (let [win-sets (remove-supersets (win-move-sets grid positions))
        win-values (map #(value-wins (count (first grid)) %) win-sets)
        counts-of-wins (frequencies (map count win-sets))]
    (apply + win-values)))

(defn eval-by-positions-needed-old
  [grid positions]
  (let [win-sets (remove-supersets (win-move-sets grid positions))
        counts-of-wins (frequencies (map count win-sets))]
    (reduce (fn [acc n]
              (let [count (get counts-of-wins n 0)
                    value (case n
                            0 (if (zero? count) 0 inf)
                            1 (* (int (Math/pow count 1)) 50)
                            2 (* count 10)
                            3 (* count 2))]
                (+ acc value)))
            0 (range 0 4))))

(defn len-value-fn
  "Gives the value of a row based on it's length, used to do evalution by row lenth"
  [length]
  (case length
    0 0
    1 1
    2 5
    3 20
    inf))

(defn eval-by-row-lengths
  "Returns a static evaluation number based on current row lengths"
  [positions]
  (loop [loop-positions positions
         all-rows #{}]
    (if-not (empty? loop-positions)
      (let [rows (get-rows
                   (first loop-positions)
                   (current-rows-accumulator positions)
                   :forwards)]
        (recur (rest loop-positions)
               (conj all-rows (:h rows) (:v rows) (:lr rows) (:rl rows))))
      (reduce (fn [acc val]
                (+ acc (len-value-fn (count val))))
              0 all-rows))))

(defn static-eval2
  [node player opponent]
  (if (= inf (eval-by-row-lengths (get-positions node opponent)))
    -inf
    (eval-by-row-lengths (get-positions node player))))

(defn static-eval
  [node player opponent]
  (if (= inf (eval-by-positions-needed node (get-positions node opponent)))
    -inf
    (eval-by-positions-needed node (get-positions node player))))

(defn static-eval-unknown
  [node player opponent]
  (eval-by-positions-needed node (get-positions node player)))

(defn game-over?
  [node]
  (let [result (static-eval node 1 2)]
    (or (= inf result)
        (= -inf result)
        (empty? (valid-moves node)))))

(defn move-player
  [color]
  (case color
    1 1
    -1 2))

(defn ordered-moves
  [node player opponent]
  (let [state-move-pairs (pmap #(assoc-in %
                                 [:state]
                                 (static-eval (:state %) player opponent))
                              (map (fn [move]
                                     {:state (perform-move move node player) :move move})
                                   (valid-moves node)))]
    (map :move (reverse (sort-by :state state-move-pairs)))))

(defn negamax-ab
  [node depth alpha beta color root player opponent]
  (if (or (zero? depth) (game-over? node))
    (* color (static-eval node player opponent))
    (let [bestValue (atom -inf)
          moves (valid-moves node)
          width (count node)
          moves (sort-by #(Math/abs (- (int (/ width 2)) %)) moves)
          moves (reverse moves)
          bestMove (atom (nth moves (rand-int (count moves))))]
      (loop [move (first moves)
             moves (rest moves)]
        (when-let [v (- (negamax-ab (perform-move move node (if (= color 1) player opponent))
                                    (dec depth)
                                    (- beta)
                                    (- alpha)
                                    (- color)
                                    false
                                    player
                                    opponent))]
          (when (> v @bestValue)
            (reset! bestValue v)
            (reset! bestMove move))
          (let [alpha (max alpha v)]
            (when-not (or (empty? moves) (>= alpha beta))
              (recur (first moves) (rest moves))))))
      (if root
        @bestMove
        @bestValue))))

(defn negamax-ab-recur
  [node depth alpha beta color player opponent]
  (if (or (zero? depth) (p ::game-over-check (game-over? node)))
    {:value (* color (static-eval node player opponent)) :move 0}
    (reduce (fn [maxResult move]
              (let [result (negamax-ab-recur
                             (perform-move move node (if (= color 1) player opponent))
                             (dec depth)
                             (- beta)
                             (- alpha)
                             (- color)
                             player
                             opponent)
                    newMax (max-key #(:value %)
                             maxResult
                             {:value (- (:value result))
                              :move move})
                    newAlpha (max alpha (- (:value result)))]
                (if (>= newAlpha beta)
                  (reduced newMax)
                  newMax)))
            {:value -inf :move nil}
            (shuffle (valid-moves node)))))

(defn parallel-negamax
  [node depth player opponent]
  (let [moves (p ::get-valid-moves (valid-moves node))
        successors (map #(perform-move % node player)
                        moves)
        best-result (apply min-key #(:value %)
                        (map-indexed #(assoc %2 :idx %1)
                          (map #(negamax-ab-recur % (dec depth) -inf inf -1 player opponent)
                             successors)))]
    (nth moves (:idx best-result))))

(defn negamax
  [node depth color root player opponent]
  (if (or (zero? depth) (game-over? node))
    (* color (static-eval node player opponent))
    (let [bestValue (atom -inf)
          moves (valid-moves node)
          bestMove (atom (nth moves (rand-int (count moves))))]
      (loop [move (first moves)
             moves (rest moves)]
        (when-let [v (- (negamax (perform-move move node (if (= color 1) player opponent))
                                 (dec depth)
                                 (- color)
                                 false
                                 player
                                 opponent))]
          (when (> v @bestValue)
            (reset! bestValue v)
            (reset! bestMove move))
          (when-not (empty? moves)
            (recur (first moves) (rest moves)))))
      (if root
        @bestMove
        @bestValue))))

(defn player-select
  [maximizingPlayer]
  (case maximizingPlayer
    true 1
    false 2))

(defn minimax
  [node depth maximizingPlayer]
  (if (or (zero? depth) (= inf))
    (static-eval node 1)
    (if maximizingPlayer
      (let [bestValue (atom -inf)
            moves (valid-moves node)]
        (loop [move (first moves)
               moves (rest moves)]
          (when-let [v (minimax (perform-move))]))))))

(defn find-best-move
  [moves precept]
  (let [opponent (case (:player precept)
                   1 2
                   2 1)
        successors (map #(perform-move % (:grid precept) opponent)
                        moves)
        evaluations (map #(eval-by-row-lengths (get-positions % opponent))
                         successors)
        max (apply max evaluations)
        max-idx (first (indices #(= % max) evaluations))]
    evaluations))

(def plies 5)

(defn -main
  [x]
  (stderr "Connect Four")
  (loop [input-line (read-line)
         move# 1]
    (if-not (or (nil? input-line) (= input-line ":exit"))
      (do
        (stderr input-line)
        (let [precept (json/read-str input-line :key-fn keyword)
              moves (valid-moves (:grid precept))
              player (:player precept)
              opponent (case player
                         1 2
                         2 1)
              move (case move#
                     1 (int (/ (count (:grid precept)) 2))
                     (parallel-negamax (:grid precept) plies player opponent))
              move-str (json/write-str {:move move})]
          (stderr move-str)
          (println move-str))
        (recur (read-line) (inc move#)))
      (shutdown-agents))))
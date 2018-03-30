(ns lg-project1.core
  (:gen-class))

(defn arities [v]
  (->> v
       meta
       :arglists
       (map #(remove #{'&} %))
       (map count)))

(defn square?
  [coll]
  (let [count (count coll)
        square (Math/sqrt count)]
    (== (* square square) count)))

(defn ones-and-zeroes?
  [coll]
  (every? #(or (= 0 %) (= 1 %)) coll))

(defn create-open?-map
  [elem]
  (hash-map :open?
            (cond
              (= 0 elem) true
              (= 1 elem) false)))

(defn convert-elements-to-maps
  [coll]
  (map create-open?-map coll))

(defn convert-into-matrix
  [seq size]
  (let [size (int (Math/sqrt (count seq)))]
    (mapv vec (partition size seq))))

(def required-input-keys
  [:board :location :reachability])

(defn input-has-all-keys?
  [map]
  (every? map required-input-keys))

(defn valid-input?
  [input]
  (and
    (input-has-all-keys? input)
    (square? (:board input))
    (ones-and-zeroes? (:board input))))

(defn calculate-size
  [board-seq]
  (let [size (int (Math/sqrt (count board-seq)))]
    size))

(defn generate-2d-board
  [side-length initial-element-data]
  (->> (repeat side-length initial-element-data)
       (into [])
       (repeat side-length)
       (into [])))

(defn assoc-at-positions
  [board key val pos-vecs]
  (reduce (fn [board pos]
            (assoc-in board (conj pos key) val))
          board pos-vecs))

(defn assoc-at-positions-t
  [board key val pos-vecs]
  (reduce (fn [board [y x]]
            (let [row (get board y)
                  elem (get row x)
                  new-elem (assoc! elem key val)
                  new-row (assoc! row x new-elem)]
              (assoc! board y new-row)))
          board pos-vecs))

(defn generate-yx-vectors
  [start-index max]
  (let [range (range start-index max)]
    (for [y range, x range]
      [y x])))

(defn generate-positions-list
  [size]
  (generate-yx-vectors 0 size))

(defn reachable?
  [[y1 x1] reachability-fn [y2 x2]]
  (if (and (= y1 y2) (= x1 x2))
    true
    (let [x-dist (Math/abs (- x2 x1))
          y-dist (Math/abs (- y2 y1))]
      (reachability-fn x-dist y-dist x1 y1 x2 y2))))


(defn valid-moves
  [board moves]
  (filter #(let [pos-map (get-in board %)]
             (and (:open? pos-map)
                  (nil? (:moves-to-reach pos-map))))
          moves))

(defn apply-moves
  [board moves move-num]
  (assoc-at-positions board :moves-to-reach move-num moves))

(defn apply-moves-t
  [board moves move-num]
  (assoc-at-positions-t board :moves-to-reach move-num moves))

(defn move-towards
  [n1 n2 amount]
  (cond
    (< n2 n1) (- n1 amount)
    (= n1 n2) n1
    :else (+ n1 amount)))

(defn king
  [x-dist y-dist & xy]
  (and
    (<= x-dist 1)
    (<= y-dist 1)))

(defn knight
  [x-dist y-dist & xy]
  (or
    (and
      (= x-dist 2)
      (= y-dist 1))
    (and
      (= x-dist 1)
      (= y-dist 2))))

(defn queen
  [x-dist y-dist & xy]
  (or
    (= x-dist 0)
    (= y-dist 0)
    (= x-dist y-dist)))

(defn rook
  [x-dist y-dist x1 y1 x2 y2]
  (or
    (and
      (= x-dist 0)
      (reachable? [y1 x1] rook [(move-towards y2 y1 1) x2]))
    (and
      (= y-dist 0)
      (reachable? [y1 x1] rook [y2 (move-towards x2 x1 1)]))))

(defn bishop
  [x-dist y-dist & xy]
  (= x-dist y-dist))

(defn pawn
  [x-dist y-dist x1 y1 x2 y2]
  (and
    (= x-dist 0)
    (= (- y1 y2) 1)))

(def test-input
  {:board [0 0 0 0 0 0 0 0 0 0
           0 0 0 0 0 0 0 0 0 0
           0 0 0 0 0 0 0 0 0 0
           0 0 0 0 0 0 0 0 0 0
           0 0 0 1 0 0 0 0 0 0
           0 0 0 1 0 0 0 0 0 0
           0 0 0 1 0 0 0 0 0 0
           0 0 0 0 0 0 0 0 0 0
           0 0 0 0 0 0 0 0 0 0
           0 0 0 0 0 0 0 0 0 0]
   :location [5 4]
   :reachability rook})

(def fifteen-input
  {:board (vec (replicate (* 15 15) 0))
   :location [7 7]
   :reachability pawn})


(defn add-reachability-for-pos
  [state pos]
  (let [reachable-positions (filter
                              (partial reachable? pos (:reachability state))
                              (:positions state))
        valid-moves (valid-moves (:board state) reachable-positions)
        new-board (apply-moves (:board state) valid-moves (:move-num state))]
    (assoc state :board new-board
                 :next-next-moves (concat (:next-next-moves state) valid-moves))))

(defn add-reachability-for-pos-t
  [state pos]
  (let [reachable-positions (filter
                              (partial reachable? pos (:reachability state))
                              (:positions state))
        valid-moves (valid-moves (:board state) reachable-positions)
        new-board (apply-moves-t (:board state) valid-moves (:move-num state))]
    (assoc state :board new-board
                 :next-next-moves (concat (:next-next-moves state) valid-moves))))


(defn process-next-moves-till-empty
  [state]
  (let [new-state (reduce (fn [state next] (add-reachability-for-pos state next))
                          state (:next-moves state))]
    (if (empty? (:next-moves new-state))
      new-state
      (recur (assoc new-state
               :move-num (inc (:move-num new-state))
               :next-moves (:next-next-moves new-state)
               :next-next-moves nil)))))

(defn process-next-moves-till-empty-t
  [state]
  (let [new-state (reduce (fn [state next] (add-reachability-for-pos-t state next))
                          state (:next-moves state))]
    (if (empty? (:next-moves new-state))
      new-state
      (recur (assoc new-state
               :move-num (inc (:move-num new-state))
               :next-moves (:next-next-moves new-state)
               :next-next-moves nil)))))

(defn output-reachability-board
  [board]
  (map #(map :moves-to-reach %) board))

(defn char-range [start end]
  (map char (range (+ (dec start) (int \a))
                   (+ end (int \a)))))

(defn add-rank-file
  [simple-matrix]
  (let [size (count simple-matrix)
        char-seq (char-range 1 size)
        add-ranks (map-indexed (fn [idx itm]
                                 (conj itm  (list (- size idx))))
                               simple-matrix)
        heading-list (conj char-seq :/)]
    (conj add-ranks heading-list)))

(defn convert-to-printable-table
  [simple-matrix]
  (let [size (count simple-matrix)
        char-seq (char-range 1 size)]
    (map-indexed (fn [idx itm]
                   (assoc (zipmap char-seq itm) :rank/file (inc idx)))
                 simple-matrix)))

(defn test-run
  [input]
  (if (valid-input? input)
    (let [size (calculate-size (:board input))
          seq-of-maps (convert-elements-to-maps (:board input))
          matrix-of-maps (convert-into-matrix seq-of-maps size)
          start-pos (:location input)
          start-board (assoc-at-positions matrix-of-maps :moves-to-reach 0 [start-pos])
          positions (generate-positions-list size)
          state (assoc input :board start-board
                             :positions positions
                             :size size
                             :next-moves [start-pos]
                             :move-num 1)
          state (process-next-moves-till-empty state)]
      (output-reachability-board (:board state)))
    (println "Input is not valid")))

(defn test-run-t
  [input]
  (if (valid-input? input)
    (let [size (calculate-size (:board input))
          seq-of-maps (convert-elements-to-maps (:board input))
          matrix-of-maps (convert-into-matrix seq-of-maps size)
          transient-board (transient (vec (map transient matrix-of-maps)))
          start-pos (:location input)
          start-board (assoc-at-positions-t transient-board :moves-to-reach 0 [start-pos])
          positions (generate-positions-list size)
          state (assoc input :board start-board
                             :positions positions
                             :size size
                             :next-moves [start-pos]
                             :move-num 1)
          state (process-next-moves-till-empty-t state)]
      (output-reachability-board (map persistent! (persistent! (:board state)))))
    (println "Input is not valid")))
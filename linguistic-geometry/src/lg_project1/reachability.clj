(ns lg-project1.reachability
  (:require [clojure.core.matrix :refer :all])
  (:gen-class))

(set-current-implementation :vectorz)

(def blocked-sentinel 99.0)

(def king
  [{:x inc :max 1}
   {:x dec :max 1}
   {:y inc :max 1}
   {:y dec :max 1}
   {:x inc :y inc :max 1}
   {:x inc :y dec :max 1}
   {:x dec :y inc :max 1}
   {:x dec :y dec :max 1}])

(def knight
  (let [add2 (partial + 2)
        min2 #(- % 2)]
    [{:x add2 :y inc :max 1}
     {:x inc :y add2 :max 1}
     {:x add2 :y dec :max 1}
     {:x dec :y add2 :max 1}
     {:x min2 :y inc :max 1}
     {:x inc :y min2 :max 1}
     {:x min2 :y dec :max 1}
     {:x dec :y min2 :max 1}]))


(defn define-movement
  [{:keys [y x max] :or {y identity, x identity}}]
  {:fn (fn [[a b]] [(y a) (x b)]) :max max})

(def rook
  [{:x inc}
   {:x dec}
   {:y inc}
   {:y dec}])

(def bishop
  [{:x inc :y inc}
   {:x inc :y dec}
   {:x dec :y inc}
   {:x dec :y dec}])

(def queen
  (concat bishop rook))

(def pawn-black
  [{:y inc :max 1}])

(def pawn-white
  [{:y dec :max 1}])

(defn loc-conv
  [matrix [x y]]
  [(- (row-count matrix) y) (- x 1)])

(defn print-mget
  [matrix x y]
  (if (or (> x 7) (> y 7) (< x 0) (< y 0))
    (println [x y])
    (mget matrix x y)))

(defn print-mset
  [matrix x y v]
  (mset matrix x y v))

(defn r-get
  [matrix x y]
  (mget matrix (- (row-count matrix) y) (- x 1)))

(defn r-set
  [matrix x y v]
  (mset matrix (- (row-count matrix) y) (- x 1) v))

(defn ones-and-zeroes?
  [seq]
  (every? #(or (== 0 %) (== 9 %)) seq))

(def required-input-keys
  [:board :location :reachability])

(defn input-has-all-keys?
  [map]
  (every? map required-input-keys))

(defn valid-input-map?
  [input-map]
  (let [board (:board input-map)]
    (and
      (input-has-all-keys? input-map)
      (if (square? board)
        (ones-and-zeroes? (to-vector board))
        (and (= (count board) 2) (= (first board) (last board)))))))

(defn input-board-to-matrix
  [board]
  (if (square? board)
    (matrix board)
    (zero-matrix (first board) (second board))))

(defn input-board-convert-sentinels
  [board]
  (let [old-sen 9
        sentinel-check #(== old-sen %)
        conversion #(if (sentinel-check %) blocked-sentinel %)]
    (emap conversion board)))

(defn set-start-position
  [board [x y]]
  (r-set board x y 0.1))

(defn generate-yx-vectors
  [start-index max]
  (let [range (range start-index max)]
    (for [y range x range]
      [y x])))

(defn generate-positions-list
  [size]
  (generate-yx-vectors 0 size))

(def test-input
  {:board (zero-matrix 8 8)
   :location [6 6]
   :reachability (map define-movement knight)
   :all-positions (set (generate-positions-list 8))
   :size 8
   :next-moves [[6 6]]
   :next-next-moves #{}
   :move-num 1})

(defn reachable?
  [board [y1 x1] reachability-fn [y2 x2]]
  (let [x-dist (Math/abs (- x2 x1))
        y-dist (Math/abs (- y2 y1))]
    (and
      (reachability-fn x-dist y-dist x1 y1 x2 y2)
      (== 0.0 (mget board y2 x2)))))

(defn reachable-positions-matrix
  [reachable-positions-set size move-num]
  (let [zeroed-matrix (zero-matrix size size)
        update-element (fn [idx elm]
                          (if (contains? reachable-positions-set idx) move-num elm))]
    (emap-indexed update-element zeroed-matrix)))

(defn movement-sequence
  [start-pos all-positions {:keys [fn max]}]
  (let [mov-seq (drop 1 (iterate fn start-pos))
        on-board? (partial contains? all-positions)]
    (if (nil? max)
      (take-while on-board? mov-seq)
      (take-while on-board? (take max mov-seq)))))

(defn all-movement-sequences
  [start-pos all-positions reachability]
  (map (partial movement-sequence start-pos all-positions) reachability))

(defn get-reachable-positions-broken
  [{:keys [board reachability all-positions move-num]} start-pos]
  (let [valid-pos? (fn [[x y]] (== 0.0 (mget board x y)))
        take-while-valid #(filter valid-pos? %)
        mov-sequences (all-movement-sequences start-pos all-positions reachability)]
    (set (apply concat (map take-while-valid mov-sequences)))))

(defn get-reachable-positions
  [{:keys [board reachability all-positions move-num]} start-pos]
  (let [valid-pos? (fn [[x y]] (let [val (mget board x y)]
                                 (and (not (== 99.0 val))
                                    (or (== 0.0 val)
                                        (<= move-num val)))))
        take-while-valid #(take-while valid-pos? %)
        mov-sequences (all-movement-sequences start-pos all-positions reachability)]
    (set (apply concat (map take-while-valid mov-sequences)))))

(defn add-reachability-for-position
  [{:keys [board next-next-moves move-num size] :as state} pos]
  (let [reachable-positions (get-reachable-positions state pos)
        blank? (fn [[x y]] (== 0.0 (mget board x y)))
        reachable-positions (set (filter blank? reachable-positions))
        reachability-matrix (reachable-positions-matrix reachable-positions (first (shape board)) move-num)]
    (assoc state :board (add board reachability-matrix)
                 :next-next-moves (concat next-next-moves reachable-positions))))

(defn process-next-moves-till-empty
  [{:keys [board move-num next-moves] :as state}]
  (let [new-state (reduce (fn [state pos] (add-reachability-for-position state pos))
                          state next-moves)]
    (if (empty? (:next-moves new-state))
      new-state
      (recur (assoc new-state
               :move-num (inc (:move-num new-state))
               :next-moves (:next-next-moves new-state)
               :next-next-moves nil)))))

(defn convert-input
  [{:keys [size blocked-list start-pos reachability]}]
  (let [matrix (input-board-to-matrix size)
        conv-blocked-list (map (partial loc-conv matrix) blocked-list)
        blocked-vals (repeat (count blocked-list) blocked-sentinel)
        matrix (set-indices matrix conv-blocked-list blocked-vals)
        matrix (set-start-position matrix start-pos)
        input-map {:board matrix
                   :location (loc-conv matrix start-pos)
                   :reachability (map define-movement reachability)
                   :all-positions (set (generate-positions-list (first size)))
                   :size (first size)
                   :next-moves [(loc-conv matrix start-pos)]
                   :next-next-moves #{}
                   :move-num 1}]
    input-map))

(defn reachability-set
  [matrix]
  #{})

(defn get-reachability
  [input-map]
  (let [internal-map (->> (convert-input input-map)
                          process-next-moves-till-empty)
        board (r-set (:board internal-map) (first (:start-pos input-map)) (last (:start-pos input-map)) 0)
        output board]
    output))

(defn clean-vector
  [board-vec]
  (let [blocked-fn #(if (= % 99) "x" %)]
    (vec (map (comp str blocked-fn int) board-vec))))

(defn print-reachability
  [board]
  (->> (to-nested-vectors board)
       (map clean-vector)
       (run! println)))

(defn get-size
  []
  (println "")
  (print "Input board dimensions [x y] (e.g. [8 8] for standard board): ")
  (flush)
  (read-string (read-line)))

(defn get-blocked-list
  []
  (println "")
  (println "Next, input list of blocked positions [[x y][x y]].")
  (println "For example, enter [[1 1][1 2]]) to block 2 squares at bottom left.")
  (println "To block no positions leave blank.")
  (print "Input blocked positions: ")
  (flush)
  (let [input (read-line)]
    (if (empty? input)
      nil
      (read-string input))))

(defn get-start-pos
  []
  (println "")
  (print "Input start position [x y] (e.g. [1 1] for bottom left): ")
  (flush)
  (read-string (read-line)))

(defn get-reachability-fn
  []
  (println "")
  (println "Prebuilt Reachabilities Available")
  (println "1. King")
  (println "2. Queen")
  (println "3. Bishop")
  (println "4. Knight")
  (println "5. Pawn (White)")
  (println "6. Pawn (Black)")
  (print "Select reachability by entering number: ")
  (flush)
  (let [choice (read-string (read-line))]
    (case choice
      1 king
      2 queen
      3 bishop
      4 knight
      5 pawn-white
      6 pawn-black
      "Error")))

(defn reachability-in-out
  []
  (let [size (get-size)
        blocked-list (get-blocked-list)
        start-pos (get-start-pos)
        reachability (get-reachability-fn)]
    (print-reachability (get-reachability {:size size :blocked-list blocked-list
                                           :start-pos start-pos :reachability reachability}))))

(defn run-examples
  []
  (let [output-board (comp print-reachability get-reachability)]
    (println "")
    (println "King on an 8x8 board with obstacles:")
    (output-board {:size [8 8] :start-pos [2 7]
                   :blocked-list [[5 4] [5 5] [5 6] [6 4] [6 5] [6 6]] :reachability king})
    (println "Queen on a 10x10 board with no obstacles:")
    (output-board {:size [10 10] :start-pos [5 5] :reachability queen})
    (println "Rook on a 10x10 board with obstacles:")
    (output-board {:size [10 10] :start-pos [5 5] :reachability rook
                   :blocked-list [[4 4] [4 5] [4 6]]})))

(defn input-loop
  []
  (flush)
  (println "")
  (println "1. Print example reachabilities")
  (println "2. Compute a specific reachability")
  (println "3. Exit")
  (print "Please select an option from choices shown above: ")
  (flush)
  (let [input (read-line)]
    (case input
      "1" (do (run-examples)
              (recur))
      "2" (do (reachability-in-out)
              (recur))
      "3" nil
      (do (println "")
          (println "That is not a valid selection. Try again")
          (recur)))))

(defn -main-old
  []
  (input-loop))

(defn test-run
  []
  (:board (time (process-next-moves-till-empty test-input))))
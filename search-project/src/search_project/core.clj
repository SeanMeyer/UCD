(ns search-project.core
  (:gen-class)
  (:require [clojure.data.json :as json]
            [clara.rules :refer :all]
            [clojure.data.priority-map :refer :all]))

(def boards-generated (atom 0))

(def game-json)
(def game-start)

(defrecord search-node
  [state path-cost heuristic-cost])

(defn get-position
  "Returns a map with keys x and y representing position of element within matrix"
  [elem matrix]
  (first
    (for [[y row] (map-indexed vector matrix)
          [x val] (map-indexed vector row)
          :when (= elem val)]
         {:x x :y y})))

(defn distance-between-points
  [x1 x2 y1 y2]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn get-distance-for-element
  [elem start-state end-state]
  (let [pos1 (get-position elem start-state)
        pos2 (get-position elem end-state)]
    (distance-between-points (:x pos1) (:x pos2) (:y pos1) (:y pos2))))

(defn manhattan-distance
  [state]
  (let [end-state (:goal game-json)
        max (dec (* (:n game-json) (:n game-json)))]
    (reduce #(+ %1 (get-distance-for-element %2 state end-state))
            0 (range 1 max))))

(defn perform-move
  "Return board with the 0 tile moved"
  [state cur-pos new-pos]
  (swap! boards-generated inc)
  (let [moved-0 (assoc state (:y new-pos)
                             (assoc (state (:y new-pos))
                               (:x new-pos) 0))]
    (assoc moved-0 (:y cur-pos)
                   (assoc (moved-0 (:y cur-pos))
                     (:x cur-pos)
                     ((state (:y new-pos)) (:x new-pos))))))

(defn do-actions
  [state]
  (let [pos (get-position 0 state)
        x (:x pos)
        y (:y pos)]
    (remove #(nil? %)
      (conj ()
        (when (< (inc y) (:n game-json))
          {:action :down, :new-state (perform-move state pos {:x x :y (inc y)})})
        (when (> y 0)
          {:action :up, :new-state (perform-move state pos {:x x :y (dec y)})})
        (when (< (inc x) (:n game-json))
          {:action :right, :new-state (perform-move state pos {:x (inc x) :y y})})
        (when (> x 0)
          {:action :left, :new-state (perform-move state pos {:x (dec x) :y y})})))))

(defn match-goal?
  [cur-state]
  (= cur-state (:goal game-json)))

(defn build-solution-path
  ([node] (build-solution-path [] node))
  ([solution-list node] (if (nil? (:parent node))
                          (conj solution-list
                                {:action (:action node) :new-state (:state node)})
                          (build-solution-path
                            (conj solution-list
                                  {:action (:action node) :new-state (:state node)}) (:parent node)))))

(defn solution-length
  [end-node]
  (-> (build-solution-path end-node)
      (count)
      (dec)))

(defn print-stats
  [end-node switch]
  (case switch
    :graph (do
             (println "Boards Generated: " @boards-generated)
             (println "Solution Length: " (solution-length end-node))
             (run! println (rest (reverse (build-solution-path end-node)))))
    :backtrack (do
                 (println "Boards Generated: " @boards-generated)
                 (println "Solution Length: " (count end-node))
                 (run! println end-node))))

(defn uniform-cost-heuristic
  [state]
  0)

(defn a*-heuristic
  [state]
  (manhattan-distance state))

(defn create-new-frontier-node
  [result node heuristic-fn]
  {:state (:new-state result)
          :parent node
          :path-cost (inc (:path-cost node))
          :total-cost (+ (inc (:path-cost node))
                         (heuristic-fn (:new-state result)))
          :action (:action result)})

(defn uniform-cost-search
  [initial-state goal-test? heuristic-fn]
  (loop [node {:state initial-state :parent nil :path-cost 0 :total-cost (heuristic-fn initial-state) :action nil}
         explored {}
         frontier (priority-map-keyfn :total-cost (:state node) node)]
    (cond
      (empty? frontier) :failure
      (goal-test? (:state node)) node
      :else (let [frontier
                  (reduce (fn [frontier result]
                            (cond
                              (and (not (contains? frontier (:new-state result)))
                                   (not (contains? explored (:new-state result))))
                              (conj frontier {(:new-state result) (create-new-frontier-node result node heuristic-fn)})
                              (and (contains? frontier (:new-state result))
                                   (> (+ (inc (:path-cost node))
                                         (heuristic-fn (:new-state result)))
                                      (:total-cost (get frontier (:new-state result)))))
                              (assoc frontier (:new-state result) (create-new-frontier-node result node heuristic-fn))
                              :else frontier))
                          frontier (do-actions (:state node)))]
              (recur
                (val (peek frontier))
                (conj explored {(:state node) node})
                (pop frontier))))))

(defn backtrack1
  [datalist bound]
  (let [data (first datalist)]
    (cond
      (.contains (rest datalist) data) :fail
      (match-goal? (:new-state data)) nil
      (> (count datalist) bound) :fail
      :else
      (loop [successors (do-actions (:new-state data))]
        (if (empty? successors)
          :fail
          (let [rdata (first successors)
                successors (rest successors)
                rdatalist (cons rdata datalist)
                path (backtrack1 rdatalist bound)]
            (if (= path :fail)
              (recur successors)
              (cons rdata path))))))))

(defn backtrack1-vector
  [datalist bound]
  (let [data (first datalist)]
    (cond
      (.contains (rest datalist) data) :fail
      (match-goal? data) nil
      (> (count datalist) bound) :fail
      :else
      (loop [successors (do-actions data)]
        (if (empty? successors)
          :fail
          (let [rdata (:new-state(first successors))
                successors (rest successors)
                rdatalist (cons rdata datalist)
                path (backtrack1-vector rdatalist bound)]
            (if (= path :fail)
              (recur successors)
              (cons rdata path))))))))

(defn backtrack1-hash
  [data datalist bound]
  (if (contains? datalist data)
    :fail
    (let [datalist (conj datalist data)]
      (cond
        (match-goal? data) nil
        (> (count datalist) bound) :fail
        :else
        (loop [successors (do-actions data)]
          (if (empty? successors)
            :fail
            (let [rdata (:new-state (first successors))
                  successors (rest successors)
                  path (backtrack1-hash rdata datalist bound)]
              (if (= path :fail)
                (recur successors)
                (cons rdata path)))))))))

(def my-bound 26)

(defn validate-json
  [game]
  (cond
    (not (contains? game :n))
    (do (println "json does not contain 'n' key") false)
    (not (contains? game :start))
    (do (println "json does not contain 'start' key") false)
    (not (contains? game :goal))
    (do (println "json does not contain 'goal' key") false)
    (not (> (:n game) 1))
    (do (println "value of 'n' in json must be greater than 1") false)
    :else true))

(defn hash-test
  [json-file]
  (def game-json
    (json/read-str (slurp json-file)
                   :key-fn keyword))
  (def game-start
    (game-json :start))
  (reset! boards-generated 0)
  ;(println "Solution Length: "(count (backtrack1-hash game-start #{} my-bound)))
  (print-stats (backtrack1-hash game-start #{} my-bound) :backtrack))

(defn -main
  [json-file]
  (def game-json
    (json/read-str (slurp json-file)
                   :key-fn keyword))
  (def game-start
    (game-json :start))
  (reset! boards-generated 0)
  (println "Graph Search")
  (println "------------")
  (print-stats (uniform-cost-search game-start match-goal? uniform-cost-heuristic) :graph)
  (reset! boards-generated 0)
  (newline)
  (println "A* Search")
  (println "----------")
  (print-stats (uniform-cost-search game-start match-goal? a*-heuristic) :graph)
  (reset! boards-generated 0)
  (newline)
  (println "Backtracking Search")
  (println "-------------------")
  ;(print-stats (backtrack1 [{:new-state game-start}] my-bound) :backtrack)
  (print-stats (backtrack1-hash game-start #{} my-bound) :backtrack)
  ;(print-stats (backtrack1-vector [game-start] my-bound) :backtrack)
  (reset! boards-generated 0)
  (newline)
  (println "Iterative Deepening Search Using Backtracking")
  (println "---------------------------------------------")
  (loop [bound 1]
    ;(let [solution (backtrack1 [{:new-state game-start}] bound)]
    (let [solution (backtrack1-hash game-start #{} bound)]
    ;(let [solution (backtrack1-vector [game-start] bound)]
      (if (= solution :fail)
        (do
          (println bound ": Cumulative boards generated: " @boards-generated)
          (recur (inc bound)))
        (do
          (println bound ": Cumulative boards generated: " @boards-generated)
          (println "Solution Length: " (count solution))
          (run! println solution))))))




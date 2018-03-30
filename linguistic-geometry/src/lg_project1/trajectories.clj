(ns lg-project1.trajectories
  (:require [lg-project1.reachability :refer :all]
            [clojure.core.matrix :refer :all]
            [clojure.math.combinatorics :as combo]
            [jutsu.core :as j])
  (:gen-class))

(def traj-input
  {:size         [8 8]
   :blocked-list [[4 5] [5 6] [6 7] [7 3] [7 4]]
   :start-pos    [4 7]
   :target-pos   [8 1]
   :reachability bishop
   :backwards nil})

(defn conv-xy
  [board [x y]]
  [(+ y 1) (- (count board) x)])

(defn get-set
  [matrix reachability]
  (set
    (let [board (to-nested-vectors matrix)]
      (for [[x row] (map-indexed vector board)
            [y val] (map-indexed vector row)
            :when (= (double reachability) val)]
        (conv-xy board [x y])))))

(defn generate-start-state
  [{:keys [target-pos size] :as input}]
  (let [map-start (get-reachability input)
        target-input (assoc input :start-pos target-pos)
        map-target (get-reachability (if (not (nil? (:backwards input)))
                                       (assoc target-input :reachability (:backwards input))
                                       target-input))
        distance (r-get map-start (first target-pos) (last target-pos))
        sum (add map-start map-target)]
    (assoc input
      :map-start map-start
      :map-target map-target
      :distance distance
      :sum-board sum
      :sum (get-set sum distance)
      :cur-path []
      :cur-move 1)))

(defn flat-trajectories
  [{:keys [map-start cur-path target-pos cur-move sum distance] :as state} cur-pos]
  (if (= cur-pos target-pos)
    (conj cur-path cur-pos)
    (let [map-cur (get-set map-start cur-move)
          reach-1 (get-set (get-reachability (assoc state :start-pos cur-pos)) 1)
          intersection (clojure.set/intersection sum map-cur reach-1)
          next-state (assoc state :cur-path (conj cur-path cur-pos)
                                  :cur-move (inc cur-move))]
      (mapcat (partial flat-trajectories next-state) intersection))))

(defn get-trajectories
  [{:keys [target-pos size] :as input}]
  (if (or (> (first target-pos) (first size))
          (> (last target-pos) (last size)))
    (list)
    (let [{:keys [distance start-pos] :as state} (generate-start-state input)]
      (->> (flat-trajectories state start-pos)
           (partition (int (inc distance)))))))

(defn admissable
  [input]
  (let [{:keys [sum-board distance] :as state} (generate-start-state input)
        sum (get-set sum-board (inc distance))
        traj-from-and-to (fn [mid]
                           {:first (map drop-last (get-trajectories (assoc input :target-pos mid)))
                            :second (get-trajectories (assoc input :start-pos mid))})]
    (->> (map traj-from-and-to sum)
         (map #(combo/cartesian-product (:first %) (:second %)))
         (mapcat #(map (partial mapcat identity) %)))))

(defn both-trajectories
  [input]
  (concat (get-trajectories input) (admissable input)))

(def test-data
  [{:x [4 2 4 5 8]
    :y [7 5 3 4 1]
    :mode "lines+markers"
    :type "scatter"
    :name "move"
    :marker {:size 8}
    :line {:color "#67818a"}
    :showlegend true}
   {:x [4 2 5 6 8]
    :y [7 5 2 3 1]
    :mode "lines+markers"
    :type "scatter"
    :name "move"
    :marker {:size 8}
    :line {:color "#67818a"}
    :showlegend false}
   {:x [4]
    :y [7]
    :mode "markers"
    :type "scatter"
    :name "Start"
    :marker {:size 14
             :color "#000"}}
   {:x [8]
    :y [1]
    :mode "markers"
    :type "scatter"
    :name "End"
    :marker {:size 14
             :color "Red"}}
   {:x [5]
    :y [5]
    :mode "markers"
    :type "scatter"
    :name "blocked"
    :marker {:size 36
             :color "#d3d3d3"
             :symbol "square"}
    :legendgroup "fix"}])

(def plot-layout
  {:xaxis {:range [0.5 8.5]
           :autotick false
           :fixedrange true
           :tick0 0.5
           :dtick 1
           :showticklabels false
           :zeroline true
           :showline true
           :gridcolor "#A9A9A9"
           :linecolor "#A9A9A9"
           :linewidth 1
           :mirror "ticks"}
   :yaxis {:range [0.5 8.5]
           :autotick false
           :fixedrange true
           :tick0 0.5
           :dtick 1
           :showticklabels false
           :zeroline true
           :showline true
           :gridcolor "#A9A9A9"
           :linecolor "#A9A9A9"
           :linewidth 1
           :mirror "ticks"}
   :hovermode "closest"
   :staticPlot true
   :height 500
   :width 500})

(defn start-marker
  [traj]
  (let [[x y] (first traj)]
    {:x [x]
     :y [y]
     :mode "markers"
     :type "scatter"
     :name "Start"
     :marker {:size 16
              :color "#000"}}))

(defn end-marker
  [traj]
  (let [[x y] (last traj)]
    {:x [x]
     :y [y]
     :mode "markers"
     :type "scatter"
     :name "End"
     :marker {:size 16
              :color "Red"}}))

(defn traj-line
  [showlegend color traj]
  {:x (map first traj)
   :y (map second traj)
   :mode "lines+markers"
   :type "scatter"
   :name "move"
   :marker {:size 8}
   :line {:color color}
   :showlegend showlegend})

(defn block-marker
  [showlegend [x y]]
  (if-not (nil? x)
    {:x [x]
     :y [y]
     :mode "markers"
     :type "scatter"
     :name "blocked"
     :legendgroup "fix"
     :marker {:size 36
              :color "#d3d3d3"
              :symbol "square"}
     :showlegend showlegend}
    {}))

(defn traj-input-to-plot
  [traj-input color]
  (let [trajectories (get-trajectories traj-input)
        blocked (:blocked-list traj-input)]
    (-> [(traj-line true color (first trajectories))]
        (into (map (partial traj-line false color) (rest trajectories)))
        (conj (start-marker (first trajectories)))
        (conj (end-marker (first trajectories)))
        (conj (block-marker true (first blocked)))
        (into (map (partial block-marker false) (rest blocked))))))

(defn add-graph
  [title short-text traj-input]
  (j/graph! title
            (traj-input-to-plot traj-input "#67818a")
            (assoc plot-layout :title short-text)))

(defn traj-input-to-plot-admis
  [traj-input color]
  (let [trajectories (admissable traj-input)
        blocked (:blocked-list traj-input)]
    (-> [(traj-line true color (first trajectories))]
        (into (map (partial traj-line false) (rest trajectories)))
        (conj (start-marker (first trajectories)))
        (conj (end-marker (first trajectories)))
        (conj (block-marker true (first blocked)))
        (into (map (partial block-marker false) (rest blocked))))))

(defn add-graph-admis
  [title short-text traj-input]
  (j/graph! title
            (traj-input-to-plot-admis traj-input "#67818a")
            (assoc plot-layout :title short-text)))

(def fast-king
  [{:x inc :max 3}
   {:x dec :max 3}
   {:y inc :max 3}
   {:y dec :max 3}
   {:x inc :y inc :max 3}
   {:x inc :y dec :max 3}
   {:x dec :y inc :max 3}
   {:x dec :y dec :max 3}])

(defn get-target-pos
  []
  (println "")
  (print "Input target position [x y] (e.g. [1 1] for bottom left): ")
  (flush)
  (read-string (read-line)))

(defn add-custom-graph
  []
  (let [start-pos (get-start-pos)
        target-pos (get-target-pos)
        reachability (get-reachability-fn)
        blocked-list (get-blocked-list)]
    (add-graph "Custom Trajectory" "" {:size [8 8] :blocked-list blocked-list :target-pos target-pos
                                           :start-pos start-pos :reachability reachability :backwards nil})))

(defn choice-loop
  []
  (println "")
  (println "1. Add a custom trajectory")
  (println "2. Exit")
  (print "Select an option: ")
  (flush)
  (case (read-line)
    "1" (do (add-custom-graph)
            (recur))
    "2" (System/exit 0)))

(defn main-old [& args]
  (println "Opening browser to display chess boards...")
  (j/start-jutsu!)
  (println "Displaying trajectory examples...")
  (add-graph "King Basic" "King from [1 5] to [8 5]"
             {:size         [8 8]
              :blocked-list []
              :start-pos    [1 5]
              :target-pos   [8 5]
              :reachability king
              :backwards nil})
  (add-graph "King with Blocks" "King from [1 5] to [8 5] with blocked squares"
             {:size         [8 8]
              :blocked-list [[2 4] [2 5] [2 6]]
              :start-pos    [1 5]
              :target-pos   [8 5]
              :reachability king
              :backwards nil})
  (add-graph "Queen" "Queen from [1 1] to [1 8]"
             {:size [8 8]
              :blocked-list []
              :start-pos [1 1]
              :target-pos [1 8]
              :reachability queen
              :backwards nil})
  (add-graph-admis "Queen Admissable" "Queen from [1 1] to [1 8] with admissable degree 2"
             {:size [8 8]
              :blocked-list []
              :start-pos [1 1]
              :target-pos [1 8]
              :reachability queen
              :backwards nil})
  (add-graph "Custom Piece" "King that can move 3 times per turn, from [1 5] to [8 5]"
             {:size         [8 8]
              :blocked-list []
              :start-pos    [1 5]
              :target-pos   [8 5]
              :reachability fast-king
              :backwards nil})
  (println "Examples should now be visible in browser.")
  (choice-loop))


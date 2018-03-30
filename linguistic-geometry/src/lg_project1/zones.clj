(ns lg-project1.zones
  (:require [lg-project1.reachability :refer :all]
            [lg-project1.trajectories :refer :all]
            [jutsu.core :as j])
  (:gen-class))

(defn piece-side
  [game piece-name]
  (let [piece (get-in game [:pieces piece-name])]
    (:side piece)))

(def layout
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

(def side-color
  {:white "#FF69B4"
   :black "#a9a9a9"})

(defn piece-marker
  [name side [x y]]
  {:x [x]
   :y [y]
   :mode "markers"
   :type "scatter"
   :name (str name)
   :legendgroup (str side)
   :marker {:size 16
            :color (side side-color)}})

(defn draw-line
  [showlegend color name traj]
  {:x (map first traj)
   :y (map second traj)
   :mode "lines+markers"
   :type "scatter"
   :name name
   :marker {:size 10}
   :line {:color color
          :width 5}
   :showlegend showlegend})

(defn draw-zone-line
  [game {:keys [piece trajectory side]}]
  {:x (map first trajectory)
   :y (map second trajectory)
   :mode "lines+markers"
   :type "scatter"
   :name (str piece)
   :showlegend false
   :marker {:size 8}
   :line {:color (side side-color)}})

(defn starting-piece-traces
  [{:keys [pieces] :as game}]
  (for [piece pieces
        :let [piece-name (key piece)
              side (:side (val piece))
              pos (:pos (val piece))]]
    (piece-marker piece-name side pos)))

(defn main-traj-trace
  [traj side]
  [(draw-line true (side side-color) "main" traj)])

(def traject
  {:size         [8 8]
   :blocked-list [[4 5] [5 6] [6 7] [7 3] [7 4]]
   :start-pos    [6 6]
   :target-pos   [4 5]
   :reachability king})

(def test-game
  {:pieces {:bishop {:reachability bishop
                     :pos          [4 7]
                     :side :white}
            :king {:reachability king
                    :pos          [6 6]
                    :side :white}
            :pawn {:reachability pawn-black
                   :backwards pawn-white
                   :pos [8 5]
                   :side :black}
            :king2 {:reachability king
                    :pos [2 4]
                    :side :black}}
   :size [8 8]
   :blocked-list [[4 5] [5 6] [6 7] [7 3] [7 4]]})

(def reti
  {:pieces {:pawn-white {:reachability pawn-white
                         :backwards pawn-black
                         :pos          [3 6]
                         :side :white}
            :king-white {:reachability king
                         :pos          [8 8]
                         :side :white}
            :pawn-black {:reachability pawn-black
                         :backwards pawn-white
                         :pos [8 5]
                         :side :black}
            :king-black {:reachability king
                         :pos [1 6]
                         :side :black}}
   :size [8 8]
   :blocked-list []})

(defn make-pawn
  [side pos]
  (if (= :white side)
    {:reachability pawn-white
     :backwards pawn-black
     :side side
     :pos pos}
    {:reachability pawn-black
     :backwards pawn-white
     :side side
     :pos pos}))

(def figure1
  {:pieces {:king-w {:reachability king
                     :pos [2 1]
                     :side :white}
            :king-b {:reachability king
                     :pos [2 7]
                     :side :black}
            :pawn-w1 (make-pawn :white [3 3])
            :pawn-w2 (make-pawn :white [4 5])
            :pawn-b (make-pawn :black [5 5])
            :bishop {:reachability bishop
                     :pos [6 2]
                     :side :white}
            :knight {:reachability knight
                     :pos [7 7]
                     :side :black}}
   :size [8 8]
   :blocked-list []})

(defn block-pieces
  [{:keys [pieces blocked-list] :as game} exception]
  (let [positions (remove #(= % exception) (map :pos (vals pieces)))]
    (->> (concat blocked-list positions)
         (assoc game :blocked-list))))

(defn unblock-target
  [{:keys [blocked-list] :as game} target-pos]
  (let [removed (remove #(= % target-pos) blocked-list)]
    (assoc game :blocked-list removed)))

(defn convert-to-reachability-input
  [game piece-name target-pos]
  (let [piece (get-in game [:pieces piece-name])]
    {:size (:size game)
     :blocked-list (:blocked-list game)
     :start-pos (:pos piece)
     :target-pos target-pos
     :reachability (:reachability piece)
     :backwards (:backwards piece)}))

(defn pull-trajectories
  [game piece-name target-pos]
  (->> (convert-to-reachability-input game piece-name target-pos)
       (get-trajectories)))

(defn get-new-pawn-targets
  [[tx ty] game reachability]
  (let [y-fun (:y (first reachability))
        new-targets [[(dec tx) (y-fun ty)] [(inc tx) (y-fun ty)]]
        size (first (:size game))]
    (filter (fn [[x y]] (and (<= x size) (<= y size)
                             (> x 0) (> y 0)))
            new-targets)))

(defn piece-reachability
  [game piece-name]
  (let [piece (get-in game [:pieces piece-name])]
    (:reachability piece)))

(defn get-pawn-trajectories
  [game piece-name target-pos]
  (let [reachability (piece-reachability game piece-name)
        new-targets (get-new-pawn-targets target-pos game reachability)
        trajectories (map #(pull-trajectories game piece-name %) new-targets)
        traj (into [] (first (some #(when-not (empty? %) %) trajectories)))]
    (if (empty? traj)
      traj
      (list (conj traj target-pos)))))

(defn is-pawn?
  [game piece-name]
  (let [reachability (piece-reachability game piece-name)]
    (or (= reachability pawn-white) (= reachability pawn-black))))

(defn get-piece-trajectories
  [game piece-name target-pos main?]
  (let [game (unblock-target game target-pos)]
    (if main?
      (pull-trajectories game piece-name target-pos)
      (if (is-pawn? game piece-name)
        (get-pawn-trajectories game piece-name target-pos)
        (pull-trajectories game piece-name target-pos)))))

(defn update-map-values [f map]
  (reduce #(update-in % [%2] f) map (keys map)))

(defn reach-to-position
  [game piece-name target-pos]
  (let [game (unblock-target game target-pos)]
    (r-get (get-reachability (convert-to-reachability-input game piece-name target-pos))
           (first target-pos) (last target-pos))))

(defn get-reach-value
  [game piece-name target-pos]
  (if (is-pawn? game piece-name)
    (let [reaches (->> (piece-reachability game piece-name)
                       (get-new-pawn-targets target-pos game)
                       (map (partial reach-to-position game piece-name))
                       (map inc))]
      (if (empty? reaches)
        0.0
        (apply max reaches)))
    (reach-to-position game piece-name target-pos)))

(defn piece-can-reach?
  [reach-to-pos time-val same-side]
  (if same-side
    (= reach-to-pos 1.0)
    (and (<= reach-to-pos time-val)
         (not (zero? reach-to-pos)))))

(defn no-moves-along-time?
  [time trajectory]
  (let [nil-or-true (->> (drop-last trajectory)
                         (some (partial contains? time)))]
    (if (nil? nil-or-true)
      true
      false)))

(defn traj-to-time-val
  [side traj time-val]
  (reduce (fn [acc pos]
            (assoc acc pos {:val time-val :side side}))
          {} (drop-last traj)))

(defn next-negation
  [{:keys [pieces blocked-list] :as game} {:keys [time] :as zone}]
  (filter some?
    (for [piece pieces
          target time
          :let [pos (key target)
                time-val (:val (val target))
                piece-name (key piece)
                side (:side (val piece))
                same-side (= side (:side (val target)))
                reach-to-pos (get-reach-value game piece-name pos)]
          :when (piece-can-reach? reach-to-pos time-val same-side)]
      (let [potential-trajectories (->> (get-piece-trajectories game piece-name pos false)
                                        (filter (partial no-moves-along-time? time)))
            valid-trajectories (filter #(<= (dec (count %)) time-val) potential-trajectories)
            traj (first valid-trajectories)
            len (dec (count traj))
            next-time (inc (- time-val len))]
        (if (not (nil? traj))
          {:piece piece-name
           :trajectory traj
           :length time-val
           :side side
           :time (traj-to-time-val side traj next-time)})))))

(defn time-reducer
  [acc item]
  (let [pos (key item)
        time-val (:val (val item))
        acc-time-val (get-in acc [pos :val])]
    (cond
      (nil? acc-time-val) (conj acc item)
      (> time-val acc-time-val) (assoc-in acc [pos :val] time-val)
      :else acc)))

(defn negation-reducer
  [acc item]
  (let [piece (dissoc item :time)
        new-output (conj (:output acc) piece)
        new-time (reduce time-reducer (:time acc) (:time item))]
      (-> (assoc acc :output new-output)
          (assoc :time new-time))))

(defn remove-pieces-from-game
  [game piece-names]
  (let [remove-piece (fn [game piece]
                       (update-in game [:pieces] dissoc piece))]
    (reduce remove-piece game piece-names)))

(defn process-negation
  [game zone count negation]
  (let [negation-pieces (set (map :piece negation))
        negation-zone (reduce negation-reducer {:output []
                                                :time {}}
                              negation)
        game (remove-pieces-from-game game negation-pieces)
        zone (-> (assoc zone :output (conj (:output zone) {:grammar (:output negation-zone)
                                                           :time (:time negation-zone)
                                                           :negation count}))

                 (assoc :time (:time negation-zone)))]
     [game zone]))

(defn create-attack-zone
  ([game {:keys [piece-name target-pos] :as attack}]
   (let [main-traj-game (block-pieces game target-pos)
         trajectories (get-piece-trajectories main-traj-game piece-name target-pos true)
         main-traj (first trajectories)]
     (create-attack-zone game attack main-traj)))
  ([game {:keys [piece-name target-pos]} main-trajectory]
   (let [game (block-pieces game nil)]
     (if (empty? main-trajectory)
       nil
       (let [side (get-in game [:pieces piece-name :side])
             zone {:output [{:grammar [{:piece piece-name
                                        :trajectory main-trajectory
                                        :length (count main-trajectory)
                                        :side side}]
                             :time nil
                             :negation 0}]
                   :time (->> (zipmap (rest main-trajectory) (iterate inc 2))
                              (update-map-values (fn [v] {:val v :side side})))}
             zone (assoc-in zone [:output 0 :time] (:time2 zone))
             game (remove-pieces-from-game game [piece-name])]
         (loop [game game
                zone zone
                count 1]
           (if (or (empty? (:pieces game))
                   (empty? (:time zone)))
             (:output zone)
             (let [[game zone] (->> (next-negation game zone)
                                    (process-negation game zone count))]
               (recur game zone (inc count))))))))))

(defn find-all-attacks
  [{:keys [pieces] :as game}]
  (for [piece1 pieces
        piece2 pieces
        :let [piece-name (key piece1)
              piece1 (val piece1)
              piece2 (val piece2)
              target-pos (:pos piece2)
              same-side (= (:side piece1) (:side piece2))]
        :when (and (not (= piece1 piece2))
                   (not same-side))]
    {:piece-name piece-name :target-pos target-pos}))

(defn find-pawn-upgrades
  [{:keys [pieces] :as game}]
  (for [piece pieces
        :let [piece-name (key piece)
              pos (:pos (val piece))]
        :when (is-pawn? game piece-name)]
    (let [reachability (piece-reachability game piece-name)
          y-fun (:y (first reachability))
          to-end "(iterate y-fun (last pos))"
          y-end (last (take-while #(not (or (= % 0) (= % 9))) (iterate y-fun (last pos))))
          y-end (if (= y-end 1) 8 1)]
      {:piece-name piece-name :target-pos [(first pos) y-end]})))

(defn all-attack-zones
  [game]
  (let [possible-attacks (concat (find-pawn-upgrades game) (find-all-attacks game))
        attack-zones (->> possible-attacks
                          (pmap (partial create-attack-zone game))
                          (remove nil?))]
    attack-zones))

(defn negation-graph-reducer
  [acc {:keys [grammar time negation] :as negation}]
  (let [game (:game acc)]
    (if (zero? negation)
      (let [data (-> (starting-piece-traces game)
                     (into (main-traj-trace (:trajectory (first grammar)) (:side (first grammar)))))]
        (do
          (j/graph! "Main Trajectory"
            data (assoc layout :title "More detailed values shown in console"))
          (assoc acc :data data)))
      (if (empty? grammar)
        acc
        (let [data (into (:data acc) (map (partial draw-zone-line game) grammar))]
          (do
            (j/graph! (str "Negation " negation)
              data (assoc layout :title "More detailed values shown in console"))
            (assoc acc :data data)))))))

(defn negation-graph-reducer-no-side
  [acc {:keys [grammar time negation] :as negation}]
  (let [game (:game acc)]
    (if (zero? negation)
      (let [data (-> (starting-piece-traces game)
                     (into (main-traj-trace (:trajectory (first grammar)) (:side (first grammar)))))]
        (assoc acc :data data))
      (if (empty? grammar)
        acc
        (let [data (into (:data acc) (map (partial draw-zone-line game) grammar))]
          (assoc acc :data data))))))

(defn print-explainer []
  (println "")
  (println "Above are the details of zone generation, including the piece,
            trajectory, length components of grammar and next-time values at each negation step.")
  (println "The zone is shown graphically in the browser"))

(defn zone-choice-loop []
  (println "Available actions")
  (println "1. Zone from assignment 5, figure 1")
  (println "2. Modified zone from lecture notes 8")
  (println "3. Reti endgame, all attack zones")
  (println "4. Exit")
  (print "Choice: ")
  (flush)
  (case (read-line)
    "1" (let [zone (create-attack-zone figure1 {:piece-name :bishop :target-pos [5 5]}
                                       [[6 2] [4 4] [5 5]])]
          (println "")
          (clojure.pprint/pprint zone)
          (print-explainer)
          (reduce negation-graph-reducer {:game figure1} zone)
          (zone-choice-loop))
    "2" (let [zone (create-attack-zone test-game {:piece-name :pawn :target-pos [8 1]}
                                       [[8 5] [8 4] [8 3] [8 2] [8 1]])]
          (println "")
          (clojure.pprint/pprint zone)
          (print-explainer)
          (reduce negation-graph-reducer {:game test-game} zone)
          (zone-choice-loop))
    "3" (let [zone (all-attack-zones reti)]
          (println "")
          (clojure.pprint/pprint zone)
          (println "")
          (println "This output shows all attack zones on one board, although each is calculated separately as it's own zone. Calculated per request of the HW.")
          (println "Output should be at the bottom of the browser, labelled Reti-Zone. I haven't learned how to clear yet.")
          (j/graph! "Reti Zone" (flatten (map :data (map #(reduce negation-graph-reducer-no-side
                                                                  {:game reti} %) zone)))
                    layout)
          (zone-choice-loop))
    "4" (System/exit 0)))

(defn -main [& args]
  (j/start-jutsu!)
  (zone-choice-loop))
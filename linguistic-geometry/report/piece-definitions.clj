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
  [x-dist y-dist & xy]
  (or
    (= x-dist 0)
    (= y-dist 0)))

(defn bishop
  [x-dist y-dist & xy]
  (= x-dist y-dist))

(defn pawn
  [x-dist y-dist x1 y1 x2 y2]
  (and
    (= x-dist 0)
    (= (- y1 y2) 1)))
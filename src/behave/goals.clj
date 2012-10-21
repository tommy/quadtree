(ns behave.goals
  (:use behave.util))

(defn step-towards
  [x position]
  (let [displacement (subtract position (:pos x))
        magnitude (length displacement)
        step (multiply {:x 1 :y 1} (divide displacement {:x magnitude :y magnitude}))]
    (move x step)))

(defn new-goal
  [x]
  (let [angle (* (rand) 2 Math/PI)
        unit {:x (Math/cos angle) :y (Math/sin angle)}
        distance (rand-int 50)
        new (add (:pos x) (multiply {:x distance :y distance} unit))]
  (assoc x :goal (within {:x [0 300] :y [0 600]} new))))

(defn distance-from-goal
  [x]
  (Math/abs (length (subtract (:pos x) (:goal x)))))

(defn at-goal
  [x]
  (< (distance-from-goal x) 5))

(defn seek-goal
  [x others]
  (if (at-goal x)
    x
  (step-towards x (:goal x))))

(ns behave.core
  (:use quil.core))

(defn within-values
  [bounds x]
  (-> x (max (first bounds)) (min (second bounds))))

(defn axis-wise
  [f]
  (fn [pos1 pos2]
    {:x (f (:x pos1) (:x pos2)) :y (f (:y pos1) (:y pos2))}))
(def add (axis-wise +))
(def subtract (axis-wise -))
(def multiply (axis-wise *))
(def divide (axis-wise /))
(def power (axis-wise #(Math/pow %1 %2)))
(def pos-min (axis-wise min))
(def within (axis-wise within-values))
(defn length [pos]
  (Math/sqrt (+ (Math/pow (:x pos) 2) (Math/pow (:y pos) 2))))
(defn move
  [x step]
  (assoc x :pos (add (:pos x) step)))

(def G 100)

(defn gravitate
  [x others]
      (let [distance (subtract {:x 200 :y 500} (:pos x) )
          step (divide distance {:x G :y G})]
      (move x step)))

(defn distance-inverse
  [x other]
  (power
   (subtract (:pos x) (:pos other))
   {:x -1 :y -1}))

(def rep 0.1)

(defn repulse
  [x other] 
  (pos-min {:x 10 :y 10}  (multiply {:x rep :y rep} (distance-inverse x other))))

(def attr -10)

(defn attract
  [x other]
  (multiply {:x attr :y attr} (distance-inverse x other)))

(defn same-color [x y] (= (:color x) (:color y)))

(defn seek-similar
  [x other]
  (if (same-color x other)
    (attract x other)
    {:x 0 :y 0}))

(defn fear-others
  [x other]
  (if (same-color x other)
    {:x 0 :y 0}
    (repulse x other)))

(defn not-me
  [x others]
  (filter #(not (= x %)) others))

(defn interact
  [f]
  (fn [x others]
    (let [total-force (reduce add (map #(f x @%) others))]
      (move x total-force))))

(def seek-personal-space (interact repulse))

(def socialize (interact attract))

(defn step-towards
  [x position]
  (let [displacement (subtract position (:pos x))
        magnitude (length displacement)
        step (multiply {:x 1 :y 1} (divide displacement {:x magnitude :y magnitude}))]
    (move x step)))



(defn random-position [x y] {:x (rand-int x) :y (rand-int y)})

(defn random-map-position []
  (random-position 300 600))

(defn new-goal
  [x]
  (let [angle (* (rand) 2 Math/PI)
        unit {:x (Math/cos angle) :y (Math/sin angle)}
        distance (rand-int 50)
        new (add (:pos x) (multiply {:x distance :y distance} unit))]
  (assoc x :goal (within {:x [0 300] :y [0 600]} new))))

(defn distance-from-goal
  [x]
  (abs (length (subtract (:pos x) (:goal x)))))

(defn at-goal
  [x]
  (< (distance-from-goal x) 5))

(defn seek-goal
  [x others]
  (if (at-goal x)
    x
  (step-towards x (:goal x))))

(defn with-probability
  [p f else-f]
  (if (< (rand) p)
    (f)
    (else-f)))

(defn wander
  [x others]
  (with-probability 0.01
    #(new-goal x)
    #(identity x)))

(defn mutate-language
  [lang]
  (with-probability 0.01
    (fn [] (map #(within-values [30 230] (- (+ (rand-int 3) %) 1)) lang))
    #(identity lang)))

(defn drift-language
  [x others]
  (assoc x :language (mutate-language (:language x))))

(defn random-language [] (rand-nth [[192 41 36] [83 122 119]]))
(defn gen-behaver
  []
  (atom {:pos (random-position 300 600)
         :goal (random-position 300 600)
         :language (random-language)
         :behaviors #{wander
                      seek-goal
                      drift-language}}))
(def behavers (repeatedly 10 gen-behaver))

(defn behave
  [b others]
  (let [behaviors (:behaviors @b)]
    (doseq [behavior behaviors]
      (swap! b behavior others))))

(defn setup []
  (smooth)                          ;;Turn on anti-aliasing
  (frame-rate 30)                    ;;Set framerate to 1 FPS
  (background 200))                 ;;Set the background colour to

(defn clear []
  (fill 236 208 120)
  (rect 0 0 (width) (height)))

(def *shown* nil)

(defn draw-behaver
  [behaver]
  (apply fill (:language @behaver))
  (apply stroke (:language @behaver))
  (if (= behaver *shown*)
    (fill 200))
  (let [pos (:pos @behaver)]
    (ellipse (:x pos) (:y pos)  10 10)
    (dotimes [m 10]
      (apply stroke (conj (:language @behaver) (- 50 (* 50 (/ m 10)))))
      (apply fill (conj (:language @behaver) (- 50 (* 50 (/ m 10)))))
      (let [r (+ 20 (* 10 m))]
        (ellipse (:x pos) (:y pos) r r)))))

(defn behave-all
  [bs]
  (doseq [b bs]
    (behave b (not-me b bs))))

(defn step-behavior
  [n bs]
  (dotimes [m n]
    (behave-all bs)))

(def rest 5)

(defn behave-forever
  [bs]
  (future
    (loop []
      (Thread/sleep rest)
      (behave-all bs)
      (recur))))

(defn show [x]
  (def *shown* x))

(defn draw []
  (clear)
  (stroke 0)
  (doseq [b behavers] (draw-behaver b)))

(defn run []
  (defsketch example                  ;;Define a new sketch named example
  :title "Oh so many grey circles"  ;;Set the title of the sketch
  :setup setup                      ;;Specify the setup fn
  :draw draw                        ;;Specify the draw fn
  :size [323 200]))

(def a (first behavers))

(defn timetest
  [n]
  (dotimes [m n]
    (time (step-behavior 1000 behavers))))

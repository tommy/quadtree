(ns behave.core
  (:use quil.core))

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
(defn length [pos]
  (Math/sqrt (+ (Math/pow (:x pos) 2) (Math/pow (:y pos) 2))))
(defn move
  [x step]
  (assoc x :pos (add (:pos x) step)))

(defn gravitate
  [center]
  (fn [x others]
    (let [distance (subtract center (:pos x) )
          step (divide distance {:x 10 :y 10})]
      (move x step))))

(defn wander
  [x others]
  (move x {:x (- 1 (rand-int 3)) :y (- 1 (rand-int 3))}))

(defn repulse
  [x other]
  (let [displacement (subtract (:pos x) (:pos other))
        force (power displacement {:x -1 :y -1})]
    (multiply {:x 5 :y 5} force)))

(defn not-me
  [x others]
  (filter #(not (= x %)) others))

(defn seek-personal-space
  [x others]
  (let [repulsions (map #(repulse x @%) others)
        total-force (reduce add repulsions)]
    (move x total-force)))
    
(defn random-position [] {:x (+ 100 (rand-int 200)) :y (+ 100 (rand-int 500))})
(defn gen-behaver
  []
  (atom {:pos (random-position) :behaviors #{wander seek-personal-space (gravitate {:x 200 :y 200})}}))
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

(def background-color 255)

(defn clear []
  (fill background-color)
  (rect 0 0 (width) (height)))

(def *shown* nil)

(defn draw-behaver
  [behaver]
  (fill 255)
  (if (= behaver *shown*)
    (fill 200))
  (let [pos (:pos @behaver)]
    (ellipse (:x pos) (:y pos) 10 10)))

(defn behave-all
  [bs]
  (doseq [b bs]
    (behave b (not-me b bs))))

(defn step-behavior
  [n bs]
  (dotimes [m n]
    (behave-all bs)))

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
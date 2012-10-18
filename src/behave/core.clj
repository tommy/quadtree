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

(def G 100)

(defn gravitate
  [x others]
      (let [distance (subtract {:x 200 :y 500} (:pos x) )
          step (divide distance {:x G :y G})]
      (move x step)))

(defn wander
  [x others]
  (move x {:x (- 1 (rand-int 3)) :y (- 1 (rand-int 3))}))

(defn distance-inverse
  [x other]
  (power
   (subtract (:pos x) (:pos other))
   {:x -1 :y -1}))

(def rep 15)

(defn repulse
  [x other] 
  (pos-min {:x 10 :y 10}  (multiply {:x rep :y rep} (distance-inverse x other))))

(def attr -10)

(defn attract
  [x other]
  (multiply {:x attr :y attr} (distance-inverse x other)))

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


  

(defn random-position [] {:x (+ 100 (rand-int 200)) :y (+ 100 (rand-int 500))})
(defn random-color [] (rand-nth [:red :green]))
(defn gen-behaver
  []
  (atom {:pos (random-position)
         :color (random-color)
         :behaviors #{wander seek-personal-space gravitate socialize}}))
(def behavers (repeatedly 20 gen-behaver))

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

(def rgb {:red '(192 41 36) :green '(83 119 122)})

(defn draw-behaver
  [behaver]
  (apply fill (rgb (:color @behaver)))
  (apply stroke (rgb (:color @behaver)))
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

(def rest 20)

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

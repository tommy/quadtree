(ns behave.core
  (:use quil.core))

(defn axis-wise
  [f]
  (fn [pos1 pos2]
    {:x (f (:x pos1) (:x pos2)) :y (f (:y pos1) (:y pos2))}))
(def add (axis-wise +))
(def subtract (axis-wise -))
(def divide (axis-wise /))
(defn gravitate
  [center]
  (fn [x]
    (let [distance (subtract center (:pos x))
          step (divide distance {:x 10 :y 10})]
    (assoc x :pos (add (:pos x) step)))))

(defn random-position [] {:x (+ 100 (rand-int 100)) :y (+ 100 (rand-int 100))})
(defn gen-behaver
  []
  (agent {:pos (random-position) :behaviors #{(gravitate {:x 100 :y 100})}}))
(def behavers (repeatedly 10 gen-behaver))

(defn behave
  [b]
  (let [behaviors (:behaviors @b)]
    (doseq [behavior behaviors]
      (send b behavior))))

(defn setup []
  (smooth)                          ;;Turn on anti-aliasing
  (frame-rate 30)                    ;;Set framerate to 1 FPS
  (background 200))                 ;;Set the background colour to

(def background-color 255)

(defn clear []
  (fill background-color)
  (rect 0 0 (width) (height)))

(defn draw-behaver
  [behaver]
  (let [pos (:pos @behaver)]
    (ellipse (:x pos) (:y pos) 10 10)))

(defn draw []
  (clear)
  (stroke 0)
  (doseq [b behavers]
    (behave b)
    (draw-behaver b)))


(defn run []
  (defsketch example                  ;;Define a new sketch named example
  :title "Oh so many grey circles"  ;;Set the title of the sketch
  :setup setup                      ;;Specify the setup fn
  :draw draw                        ;;Specify the draw fn
  :size [323 200]))

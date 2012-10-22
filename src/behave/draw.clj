(ns behave.draw
  (:use quil.core))

(defn setup []
  (smooth)                          ;;Turn on anti-aliasing
  (frame-rate 30)                    ;;Set framerate to 1 FPS
  (background 200))

(defn clear []
  (fill 236 208 120)
  (rect 0 0 (width) (height)))

(defn draw-behaver
  [behaver]
  (apply fill (:language @behaver))
  (apply stroke (:language @behaver))
  (let [pos (:pos @behaver)]
    (ellipse (:x pos) (:y pos)  10 10)
    (if (:player @behaver)
      (do
        (ellipse (:x pos) (:y pos) 20 20)))
    (dotimes [m 10]
      (apply stroke (conj (:language @behaver) (- 50 (* 50 (/ m 10)))))
      (apply fill (conj (:language @behaver) (- 50 (* 50 (/ m 10)))))
      (let [r (+ 20 (* 10 m))]
        (ellipse (:x pos) (:y pos) r r)))))

(def cursor-position (atom {:x 0 :y 0}))

(defn draw-behavers
  [behavers]
  (fn []
    (clear)
    (fill 0)
    (reset! cursor-position {:x (mouse-x) :y (mouse-y)})
    (text (str "cursor"
               (:x @cursor-position)
               " "
               (:y @cursor-position)) 100 100)
    (stroke 0)
    (doseq [b behavers] (draw-behaver b))))

(defn run [behavers]
  (defsketch example                ;;Define a new sketch named example
  :title "Oh so many grey circles"  ;;Set the title of the sketch
  :setup setup                      ;;Specify the setup fn
  :draw (draw-behavers behavers)    ;;Specify the draw fn
  :size [323 200]))
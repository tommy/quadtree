(ns behave.core
  (:use quil.core)
  (:use behave.util)
  (:use behave.goals)
  (:use behave.draw))

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



(defn random-position [x y] {:x (rand-int x) :y (rand-int y)})

(defn random-map-position []
  (random-position 300 600))


(defn wander
  [x others]
  (with-probability 0.01
    #(new-goal x)
    #(identity x)))


(defn gen-behaver
  []
  (atom {:pos (random-position 300 600)
         :goal (random-position 300 600)
         :language (random-language)
         :behaviors #{wander
                      seek-goal
                      drift-language
                      drift-language-towards-neighbors}}))
(def behavers (repeatedly 20 gen-behaver))

(defn behave
  [b others]
  (let [behaviors (:behaviors @b)]
    (doseq [behavior behaviors]
      (swap! b behavior others))))



(def *shown* nil)



(defn behave-all
  [bs]
  (doseq [b bs]
    (behave b (map #(identity @%) (not-me b bs)))))

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

(def a (first behavers))

(defn timetest
  [n]
  (dotimes [m n]
    (time (step-behavior 1000 behavers))))

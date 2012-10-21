(ns behave.core
  (:use quil.core)
  (:use behave.util)
  (:use behave.goals)
  (:use behave.draw))

(defn not-me
  [x others]
  (filter #(not (= x %)) others))

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

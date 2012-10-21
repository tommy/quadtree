(ns behave.core
  (:use quil.core)
  (:use behave.util)
  (:use behave.goals)
  (:use behave.draw)
  (:use behave.language))

(defn not-me
  [x others]
  (filter #(not (= x %)) others))

(defn random-position [x y] {:x (rand-int x) :y (rand-int y)})

(defn gen-behaver
  []
  (atom {:pos (random-position 300 600)
         :goal (random-position 300 600)
         :language (random-language)
         :player false
         :behaviors #{wander
                      seek-goal
                      drift-language
                      drift-language-towards-neighbors}}))

(defn follow-cursor
  [x others]
  (assoc x :pos @cursor-position))

(def player
  (atom
   {:pos (random-position 300 600)
    :player true
    :language [255 0 0]
    :behaviors #{follow-cursor
                 drift-language
                 drift-language-towards-neighbors}}))

(def behavers (conj (repeatedly 5 gen-behaver) player))

(defn behave
  [b others]
  (let [behaviors (:behaviors @b)]
    (doseq [behavior behaviors]
      (swap! b behavior others))))

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


(ns behave.util
  (:use [quad.core :only [query]]))

(defn position
  [x]
  {:pre [(map? x)]}
  [(:x (:pos x)) (:y (:pos x))])

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

(defn within-radius
  [x others r]
  (let [index (:index (meta others))]
    (query index position (position x) r)))

(defn with-probability
  [p f else-f]
  (if (< (rand) p)
    (f)
    (else-f)))

(defn distance-inverse
  [x other]
  (power
   (subtract (:pos x) (:pos other))
   {:x -1 :y -1}))


(ns behave.language
  (:use behave.util))

(defn average [x y] (/ (+ x y) 2))

(defn move-language-towards
  [x other]
  (assoc x :language (vec (map average (:language x) (:language other)))))

(defn drift-language-towards-neighbors
  [x others]
  (let [neighbors (within-radius x others 50)]
    (reduce move-language-towards x neighbors)))

(defn mutate-language
  [lang]
  (with-probability 1
    (fn [] (vec (map #(within-values [30 230] (- (+ (rand-int 3) %) 1)) lang)))
    #(identity lang)))

(defn drift-language
  [x others]
  (assoc x :language (mutate-language (:language x))))

(defn random-language [] (vec (repeatedly 3 #(rand-int 255))))

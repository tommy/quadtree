(ns quad.test
  (:use quad.core)
  (:require [cljts.geom :as g])
  (:require [cljts.analysis :as a])
  (:require [cljts.relation :as r])
  (:require [clojure.zip :as z]))

(def c g/c)
(def p (comp g/point g/c))

(def square
  (g/polygon
    (g/linear-ring
      [(c 500 500) (c 100 500) (c 100 100) (c 500 100) (c 500 500)])
    nil))

(def poly
  (g/polygon
    (g/linear-ring
      [(c 0 440) (c 50 340) (c 100 210) (c 150 100)
       (c 200 200) (c 250 380) (c 300 465)
       (c 250 510) (c 200 609) (c 150 700)
       (c 100 640) (c 50 550) (c 0 420)])
    nil))

(def n
  (node square))

(def myzipper
  (-> n
      quad-zip
      (z/edit split)
      z/down
      (z/edit split)
      z/root
      quad-zip
  ))

(def guy-pos :pos)

(def guys
  [
  {:name "david" :pos [0.25 0.25]}
  {:name "tommy" :pos [0.3 0.4]}
  {:name "stephanie" :pos [-0.2 -0.2]}
  {:name "josh" :pos [-0.4 0.2]}
  ])

(defn string
  [node]
  [(:boundary node)
   (:data node)
   (map string (:quads node))])

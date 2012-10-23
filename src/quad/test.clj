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

(ns quad.test
  (:use quad.core)
  (:require [incanter.core :as i])
  (:require [cljts.geom :as g])
  (:require [cljts.analysis :as a])
  (:require [cljts.relation :as r])
  (:require [clojure.zip :as z]))

(def c g/c)
(def p (comp g/point g/c))

(def square
  (g/polygon
    (g/linear-ring
      [(c -1 -1) (c 1 -1) (c 1 1) (c -1 1) (c -1 -1)])
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
  {:name "david" :pos (p 0.25 0.25)}
  {:name "tommy" :pos (p 0.3 0.4)}
  {:name "stephanie" :pos (p -0.2 -0.2)}
  {:name "josh" :pos (p -0.4 0.2)}
  ])

(defn string
  [node]
  [(:boundary node)
   (:data node)
   (map string (:quads node))])
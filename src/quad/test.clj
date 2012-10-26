(ns quad.test
  (:use quad.core)
  (:use [clojure.tools.logging :only [spy]])
  (:require [cljts.geom :as g])
  (:require [cljts.analysis :as a])
  (:require [cljts.relation :as r])
  (:require [clojure.zip :as z]))

(def c g/c)
(def p (comp g/point g/c))

(def square
  (g/polygon
    (g/linear-ring
      [(c 500 500) (c 0 500) (c 0 0) (c 500 0) (c 500 500)])
    nil))

(def poly
  (g/polygon
    (g/linear-ring
      [(c 0 440) (c 50 340) (c 100 210) (c 150 100)
       (c 200 200) (c 250 380) (c 300 465)
       (c 250 510) (c 200 609) (c 150 700)
       (c 100 640) (c 50 550) (c 0 440)])
    nil))

(def n
  (node square))

(defn guy-pos
  [e]
  (let [v (:pos e)]
    (apply p v)))

(def myquadtree
  (-> n
      quad-zip
      (z/edit split guy-pos)
      z/down
      (z/edit split guy-pos)
      z/root
      (with-meta {:position-fn guy-pos :capacity 3})
  ))

(def myzipper
  (quad-zip myquadtree))

(def guys
  [
  {:name "david" :pos [150 150]}
  {:name "tommy" :pos [30 40]}
  {:name "stephanie" :pos [200 200]}
  {:name "josh" :pos [400 200]}
  ])

(defn string
  [node]
  [(:boundary node)
   (:data node)
   (map string (:quads node))])

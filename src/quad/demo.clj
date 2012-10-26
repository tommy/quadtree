(ns quad.demo
  (:require [cljts.geom :as g])
  (:use quad.core)
  (:use quad.test)
  (:use quad.zipper)
  (:require [clojure.zip :as z])
  (:use [quil.core :as quil :only [defsketch begin-shape vertex end-shape
                                   mouse-x mouse-y]])
  (:import [com.vividsolutions.jts.geom
            GeometryFactory
            PrecisionModel
            PrecisionModel$Type
            Coordinate
            LinearRing
            Point
            Polygon
            Geometry]))

(defn atom?
  "True if x is an atom."
  [x]
  (= clojure.lang.Atom
    (class x)))

(defn atom-
  "Wraps r in a atom, unless r is already a atom."
  [r]
  (if (atom? r) r (atom r)))

(defn tovec
  ([^Coordinate c]
  [(.x c) (.y c)]))

(defn duplicate-inner
  [xs]
  (drop 1
    (drop-last 1
      (interleave xs xs))))

(defn draw-geom
  "Draw a geometry using the specified drawing mode.
  
  mode is one of :points, :lines, :triangles,
                 :triangle-fan, :triangle-strip,
                 :quads, :quad-strip"
  [geometry mode]
  (let [vs (duplicate-inner
              (map tovec (g/coordinates geometry)))]
    (do
      (begin-shape mode)
      (doseq [v vs]
        (apply vertex v))
      (end-shape))))
      
(comment
(def square
  (g/polygon
    (g/linear-ring
      [(g/c 20 20) (g/c 10 20)
       (g/c 50 50) (g/c 60 60)
       (g/c 10 10) (g/c 20 10)


       (g/c 20 20)
      ])
    nil))
)

(def dfs (partial tree-seq branch? children))
    
(defn boundary-seq
  [quadtree]
  {:pre [(not (atom? quadtree))]}
  (map :boundary
    (dfs
      quadtree)))

(defn object-seq
  [quadtree]
  {:pre [(not (atom? quadtree))]}
  (let [pos-fn 
          (comp
            (comp tovec (memfn getCoordinate))
            (:position-fn (meta quadtree)))]
    (map pos-fn
      (mapcat :data (dfs quadtree)))))

(defn setup
  [quadtree]
  ())

(defn draw
  [quadtree]
  (let [quadtree @quadtree]
    (doseq [b (boundary-seq quadtree)]
      (draw-geom b :lines))
    (quil/stroke-weight 5)
    (doseq [o (object-seq quadtree)]
      (apply quil/point o))
    (quil/stroke-weight 1)))

(defn mouse-clicked
  [quadtree]
  (let [x (mouse-x) y (mouse-y)
        e {:pos [x y]}
        f #(add %1 [e])]
    (swap! quadtree f)))


(defn run
  ([quadtree]
  
  (let [quadtree (atom- quadtree)]
    (defsketch quadtree-sketch
      :title "Quadtree"
      :setup (partial setup quadtree)
      :draw (partial draw quadtree)
      :mouse-clicked (partial mouse-clicked quadtree))))

  ([]
  (run myquadtree)))

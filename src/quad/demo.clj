(ns quad.demo
  (:require [cljts.geom :as g])
  (:use quad.core)
  (:use quad.zipper)
  (:require [clojure.zip :as z])
  (:use [quil.core :as quil :only [defsketch begin-shape vertex end-shape]])
  (:import [com.vividsolutions.jts.geom
            GeometryFactory
            PrecisionModel
            PrecisionModel$Type
            Coordinate
            LinearRing
            Point
            Polygon
            Geometry]))

(defn ref?
  "True if x is a ref."
  [x]
  (= clojure.lang.Ref
    (class x)))

(defn ref-
  "Wraps r in a ref, unless r is already a ref."
  [r]
  (if (ref? r) r (ref r)))

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
      
(def square
  (g/polygon
    (g/linear-ring
      [(g/c 20 20) (g/c 10 20)
       (g/c 50 50) (g/c 60 60)
       (g/c 10 10) (g/c 20 10)


       (g/c 20 20)
      ])
    nil))

(def dfs (partial tree-seq branch? children))
    
(defn boundary-seq
  [quadtree]
  {:pre [(not (ref? quadtree))]}
  (map :boundary
    (dfs
      quadtree)))

(defn object-seq
  [quadtree]
  {:pre [(not (ref? quadtree))]}
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
  ())


(defn run
  [quadtree]
  
  (let [quadtree (ref- quadtree)]
    (defsketch quadtree-sketch
      :title "Quadtree"
      :setup (partial setup quadtree)
      :draw (partial draw quadtree)
      :mouse-clicked (partial mouse-clicked quadtree))))

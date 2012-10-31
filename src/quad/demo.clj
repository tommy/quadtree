(ns quad.demo
  (:require [cljts.geom :as g])
  (:use lang)
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

(defn tovec
  ([^Coordinate c]
  [(.x c) (.y c)]))

(defn pos-fn
  [quadtree]
  (comp
    (comp tovec (memfn getCoordinate))
    (:position-fn (meta quadtree))))

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

(defn selected-object-seq
  [quadtree]
  {:pre [(not (atom? quadtree))]}
  (let [pos-fn
          (comp
            (comp tovec (memfn getCoordinate))
            (:position-fn (meta quadtree)))]
    (map pos-fn
      (query quadtree [(mouse-x) (mouse-y)] 50))))

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
    (quil/stroke 255 0 0)
    (doseq [o (selected-object-seq quadtree)]
      (apply quil/point o))
    (quil/stroke (quil/color 0))
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

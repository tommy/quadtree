(ns quad.jts
  "JTS/CljTS interop."
  (:use [cljts geom analysis relation])
  (:import (com.vividsolutions.jts.geom
             Point Coordinate Polygon)))

(defn point?
  [p]
  (= Point (class p)))

(defn coordinate?
  [c]
  (= Coordinate (class c)))

(def two-d? (comp (partial = 2) dimension))
(def rectangle? (memfn isRectangle))

(defn p
  "Create a JTS point object. Input can be any of:
  a JTS point object
  a JTS coordinate object
  a vector of 2 numbers
  x and y values."
  ([x]
  (cond
    (point? x) x
    (coordinate? x) (point x)
    (vector? x) (point (apply c x))))
  ([x y]
    (point (c x y))))

(defn- x
  "The x coordinate of the Point."
  [^Point p]
  (.getX p))

(defn- y
  "The y coordinate of the Point."
  [^Point p]
  (.getY p))


(defn bounding-corners
  "The four corners of this geometry's bounding box,
  returned as a set of JTS Points."
  [geometry]
  {:post [(= 4 (count %))]}
  (let [e (envelope geometry)]
    (set (map p (coordinates e)))))

(defn extrema
  "Get the minimum and maximum x and y values of a
  collection of points.
  
  Returns a vector:
  [minx miny maxx maxy]"
  [coll]
  {:pre [(every? point? coll)]}
  (let [xs (map x coll)
        ys (map y coll)]
    [(apply min xs)
     (apply min ys)
     (apply max xs)
     (apply max ys)]))

(defn rectangle
  "Shorthand for constructing a rectangle given two corners."
  [^Point p ^Point q]
  {:post [(rectangle? %)]}
  (let [[minx miny maxx maxy] (extrema [p q])]
    (polygon
      (linear-ring
        [(c minx miny)
         (c minx maxy)
         (c maxx maxy)
         (c maxx miny)
         (c minx miny)])
      nil)))


(defn circle-bounding-box
  "The bounding box for a circle defined by center and radius."
  [center radius]
  (let [center (p center)
        minx (- (x center) radius)
        miny (- (y center) radius)
        maxx (+ (x center) radius)
        maxy (+ (x center) radius)]
    (rectangle
      (p minx miny)
      (p maxx maxy))))
        

(defn quadrants
  "Divide a polygon into quadrants,
  centered on the polygon's centroid."
  [^Polygon p]
  {:post [(comp (partial = 4) count)]}
      ; TODO: why doesn't this return true for non-rectangles?
      ;(.equalsTopo (:boundary n)
      ;             (reduce a/union %))
  (let [center (centroid p)
        corners (bounding-corners p)]
    (map
      (comp
        (partial intersection p)
        (partial rectangle center))
      corners)))

(defn within-radius
  "Returns a predicate that tests if its argument
  falls within a circle defined by center and radius."
  [center radius]
  (fn [^Point q]
    (>= radius
      (distance (p center) q))))


















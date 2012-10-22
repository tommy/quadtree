(in-ns 'quad.core)

;; JTS interop
(defn- point?
  [p]
  (= Point (class p)))

(defn- coordinate?
  [c]
  (= Coordinate (class c)))

(defn- p
  "Create a JTS point object. Input can be any of:
  a JTS point object
  a JTS coordinate object
  a vector of 2 numbers
  x and y values."
  ([x]
  (cond
    (point? x) x
    (coordinate? x) (g/point x)
    (vector? x) (g/point (apply g/c x))))
  ([x y]
    (g/point (g/c x y))))

(defn- x
  [^Point p]
  (.getX p))

(defn- y
  [^Point p]
  (.getY p))



;; Quadtree datastructure operations

(defrecord Node [boundary quads data])

(defn node
  "Create an empty leaf node with the given boundary."
  [boundary]
  (Node. boundary [] #{}))

(defn- add-data
  "Add an object to the given node's data field."
  [node e]
  (update-in node [:data] conj e))

(defn- node?
  "True if the parameter represents a node."
  [n]
  (every?
    (partial contains? n)
    #{:boundary :quads :data}))

(def ^:private leaf? (comp empty? :quads))

(defn data
  "The data stored in this node and all child nodes."
  [node]
  (let [d (:data node)
        qs (:quads node)]
    (reduce into
      (set d)
      (map data qs))))

(defn get-position-fn
  [quadtree]
  {:post [(not (nil? %))]}
  (:position-fn (meta quadtree)))

;; Some helpful functions on JTS objects

(defn- bounding-corners
  "The four corners of this geometry's bounding box,
  returned as points."
  [geometry]
  {:post [(= 4 (count %))]}
  (let [e (g/envelope geometry)]
    (set (map g/point (g/coordinates e)))))

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
  [p q]
  {:pre [(= 0 (g/dimension p))
         (= 0 (g/dimension q))]
   :post [(.isRectangle %)]}
  (let [[minx miny maxx maxy] (extrema [p q])]
    (g/polygon
      (g/linear-ring
        [(g/c minx miny)
         (g/c minx maxy)
         (g/c maxx maxy)
         (g/c maxx miny)
         (g/c minx miny)])
      nil)))


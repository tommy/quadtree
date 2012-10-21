(ns quad.core
  (:require [incanter.core :as i])
  (:require [cljts.geom :as g])
  (:require [cljts.analysis :as a])
  (:require [cljts.relation :as r])
  (:require [clojure.zip :as z])
  (:import (com.vividsolutions.jts.geom
             Point)))

(defrecord Node [boundary quads data])

(defn node
  "Create an empty leaf node with the given boundary."
  [boundary]
  (Node. boundary [] #{}))

(defn- add-data
  "Add an object to the given node's data field."
  [node e]
  (update-in node [:data] conj e))

(defn node?
  "True if the parameter represents a node."
  [n]
  (every?
    (partial contains? n)
    #{:boundary :quads :data}))

(def leaf? (comp empty? :quads))

;; quadtree zipper

(def children :quads)

(defn- make-node
  [node children]
  (assoc node :quads children))

(defn quad-zip
  "Creates a zipper for the quadtree rooted at n."
  [n]
  (z/zipper
    (complement leaf?)
    children
    make-node
    n))

;; end quadtree zipper code


(defn data
  "The data stored in this node and all child nodes."
  [node]
  (let [d (:data node)
        qs (:quads node)]
    (reduce into
      (set d)
      (map data qs))))

(defn bounding-corners
  "The four corners of this geometry's bounding box,
  returned as points."
  [geometry]
  {:post [(= 4 (count %))]}
  (let [e (g/envelope geometry)]
    (set (map g/point (g/coordinates e)))))

(defn point?
  [p]
  (= Point (class p)))

(defn x
  [^Point p]
  (.getX p))

(defn y
  [^Point p]
  (.getY p))

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

(comment 
(defn- child-add
  [node position-fn coll]
  (let [children (:quads node)]
    (loop [n node [x & xs] coll]
      (let [container (first (filter (partial r/within? x) children))]
        (recur (add ))))))
)

(defn container
  "Returns the node that should contain the given object.

  That is, the first node (of the candidate-nodes) that
  the object lies within, or nil if it does not lie within
  any of the candidate-nodes."
  [position-fn object candidate-nodes]
  (first
    (filter
      (comp
        (partial r/within? (position-fn object))
        :boundary)
      candidate-nodes)))

(defn follow-pred
  "Descends a zipper, following the path of nodes for which
  pred is true."
  [zipper pred]
  {:pre [(pred (z/node zipper))]}
  (if (leaf? (z/node zipper))        ; if the node is a leaf
    zipper                           ; then we are done
    (loop [zipper (z/down zipper)]   ; otherwise, loop through the children
      (if (pred (z/node zipper))     ; if pred is true for this child,
        (follow-pred zipper pred)    ;   continue down
                                     ; if pred is false for this child,
        (if (nil? (z/right zipper))  ;   and this is the last child,
          (z/up zipper)              ;   return the parent
          (recur (z/right zipper))   ; otherwise, try the next sibling
      )))))



(defn- add-one
  [node position-fn e]
  (let [pos (position-fn e)
        pred (comp
               (partial r/within? pos) ; the condition on which we descend the
               :boundary)              ; tree is (within? pos boundary)
        loc (->
            (quad-zip node)
            (follow-pred pred))]
    (z/root
      (z/edit
        loc
        add-data e))))

(defn add
  [node position-fn coll]
  {:pre [(every? (comp (partial r/covers? (:boundary node)) position-fn) coll)]
   :post [(clojure.set/subset? (set coll) (data %))]}
  (reduce
    #(add-one %1 position-fn %2)
   node
   coll))

  

(defn split
  [n]
  {:pre [(node? n)
         (= 2 (g/dimension (:boundary n)))]
   :post [(node? %)
          (every? node? (:quads %))
          (.equalsTopo (:boundary n)
                       (reduce a/union (map :boundary (:quads %))))
          ]}
  (let [b (:boundary n)
        center (g/centroid b)
        corners (bounding-corners b)
        quads (map (comp node (partial rectangle center)) corners)
        data (:data n)
       ]
    (Node. b quads data)))
    
    

(defn quad
  ([position-fn coll] nil)
  
  ([position-fn coll capacity]
  ))

(ns quad.core
  (:use quad.zipper)
  (:require [cljts.geom :as g])
  (:require [cljts.analysis :as a])
  (:require [cljts.relation :as r])
  (:require clojure.set)
  (:require [clojure.zip :as z])
  (:import (com.vividsolutions.jts.geom
             Point Coordinate)))

(load "private")

;; functions passed to clojure.zip/zipper
(def children :quads)
(def branch? (complement leaf?))
(defn- make-node
  [node children]
  (assoc node :quads children))

(defn quad-zip
  "Creates a zipper for the quadtree rooted at n."
  [n]
  (z/zipper
    branch?
    children
    make-node
    n))
    
(defn within-node-predicate
  "Return a predicate that returns true if geom is contained
  within the node.
  
  That is, returns a function f such that:
  (f node) <=> (within? geom (:boundary node))"
  [geom]
  {:pre [(g/simple? geom)]}
  (comp (partial r/within? geom) :boundary))

(defn covered-by-node-predicate
  "Return a predicate that returns true if geom is covered by
  the node.

  That is, returns a function f such that:
  (f node) <=> (covers? (:boundary node) geom)"
  [geom]
  (fn [node]
    (r/covers? (:boundary node) geom)))

(defn count-leaves
  "Count the number of leaves in this quad tree."
  [root]
  (loop [zipper (quad-zip root)
         n      0]
    (if (z/end? zipper) n                 ; if we've reached the end,
                                          ; return our count
      (if (leaf? (z/node zipper))
        (recur (z/next zipper) (inc n))   ; if we're at a leaf, inc n
        (recur (z/next zipper) n)))))     ; otherwise just move on


;; inserting objects into quadtree

(defn- add-one
  "Add a single object to the quadtree rooted at node."
  ([node e]
  (let [pos-fn (get-position-fn node)]
    (add-one node pos-fn e)))

  ([node pos-fn e]
  {:pre [(r/covers? (:boundary node) (pos-fn e))]}
  (let [pos (pos-fn e)
        pred (covered-by-node-predicate pos)
        loc (->
            (quad-zip node)
            (follow-pred leaf? pred))]
    (z/root
      (z/edit
        loc
        add-data e)))))

(defn add
  "Add a collection of objects to the quadtree rooted at node."
  [node coll]
  {:post [(clojure.set/subset? (set coll) (data %))]}
  (reduce add-one node coll))


;; splitting nodes

(defn split
  [n]
  {:pre [(node? n)
         (= 2 (g/dimension (:boundary n)))]
   :post [(node? %)
          (every? node? (:quads %))
            ;(.equalsTopo (:boundary n)
            ;             (reduce a/union (map :boundary (:quads %))))
          ]}
  (let [b (:boundary n)
        center (g/centroid b)
        corners (bounding-corners b)
        quads (map
                (comp
                  node
                  (partial a/intersection b)
                  (partial rectangle center))
                corners)
        data (:data n)
       ]
    (Node. b quads data)))


(def to-jts (partial comp p))


;; querying a quadtree

(defn query-geom
  [quadtree position-fn geometry]
  {:pre [(= 2 (g/dimension geometry))]}
  (let [zipper (quad-zip quadtree)
        pred (covered-by-node-predicate geometry)
        candidates (data (z/node (follow-pred zipper leaf? pred)))
        ]
   (filter
     (comp (partial r/covers? geometry) position-fn)
     candidates)))


(defn query
  [quadtree position-fn center radius]
  (let [position-fn (to-jts position-fn)
        center (apply p center)
        minx (- (x center) radius)
        miny (- (y center) radius)
        maxx (+ (x center) radius)
        maxy (+ (x center) radius)
        bbox (rectangle (p minx miny)
                        (p maxx maxy))
        zipper (quad-zip quadtree)
        pred (covered-by-node-predicate bbox)
        candidates (data (z/node (follow-pred zipper leaf? pred)))
        ]
    (filter
      #(>= radius
          (a/distance center
            (position-fn %)))
      candidates)))

(defn split-all-leaves
  [n]
  ;{:post [(= (* 4 (count-leaves n))
  ;           (count-leaves %))]}
  (if (leaf? n)
    (split n)
    (assoc n :quads
      (map split-all-leaves (:quads n)))))
  

(defn- build-empty-quadtree
  [boundary depth]
  (loop [root (node boundary) depth depth]
    (if-not (zero? depth)
      (recur (split-all-leaves root) (dec depth))
      root)))

(defn quad
  "Construct a quadtree object.

  corner1     - a vector [x y] specifying one corner of the boundary
  corner2     - a vector [x y] specifying the opposite corner
  position-fn - a fn of one argument that returns the position [x y]
                of each member of coll
  depth       - the depth of the tree (3 or 4 is recommended)
  coll        - a collection of objects to be put in the quadtree."
  [corner1 corner2 position-fn depth coll]
  (let [position-fn (to-jts position-fn)
        boundary (rectangle
                   (apply p corner1)
                   (apply p corner2))
        empty-tree 
          (with-meta
            (build-empty-quadtree boundary depth)
            {:position-fn position-fn})]
    (add empty-tree coll)))

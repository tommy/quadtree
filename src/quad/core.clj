(ns quad.core
  (:use lang)
  (:use quad.zipper)
  (:use quad.jts)
  (:use [clojure.tools.logging :only [spy]])
  (:require [cljts.relation :as r])
  (:require clojure.set)
  (:require [clojure.zip :as z]))


;; Quadtree datastructure operations

(defrecord Node [boundary quads data])
(declare add)

(defn- node?
  "True if the parameter represents a node."
  [n]
  (every?
    (partial contains? n)
    #{:boundary :quads :data}))

(defn node
  "Create an empty leaf node with the given boundary."
  [boundary]
  (Node. boundary [] #{}))


;; splitting nodes

(defn split
  "Split a node into quadrants and add its data so that
  it descends into the correct child node."
  [n pos-fn & {:keys [capacity]}]
  {:post [(node? %)
          (every? node? (:quads %))]}
  (let [b (:boundary n)
        nodes (map node (quadrants b))
        data (:data n)]
    (-> (Node. b nodes #{})
        (with-meta {:position-fn pos-fn :capacity capacity})
        (add data))))

(defn check-capacity
  "Check if node has more than capacity elements of data.
  If so, split the node into quadrants."
  [capacity node pos-fn]
  {:pre [((complement nil?) capacity)
         ((complement nil?) node)]}
  (let [n (count (:data node))]
    (cond
      (<= capacity 0) node
      (<= n capacity) node
      :otherwise (split node pos-fn :capacity capacity))))

(defn- add-data
  "Add an object to the given node's data field."
  [node pos-fn e & {:keys [capacity] :or {capacity 0}}]
  {:pre [((complement nil?) capacity)]}
  (check-capacity capacity
    (update-in node [:data] conj e)
    pos-fn))

(def- leaf? (comp empty? :quads))

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
  {:post [(complement nil?)]}
  (:position-fn (meta quadtree)))

(defn get-capacity
  [quadtree]
  {:post [(complement nil?)]}
  (:capacity (meta quadtree)))


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
  [node pos-fn e & {:keys [capacity] :or {capacity 0}}]
  {:pre [(r/covers? (:boundary node) (pos-fn e))
         ((complement nil?) capacity)]}
  (let [pos (pos-fn e)
        pred (covered-by-node-predicate pos)
        loc (->
            (quad-zip node)
            (follow-pred pred))]
    (z/root
      (z/edit
        loc
        add-data pos-fn e :capacity capacity))))

(defn add
  "Add a collection of objects to the quadtree rooted at node."
  [node coll]
  {:post [(clojure.set/subset? (set coll) (data %))]}
  (let [pos-fn (get-position-fn node)
        capacity (spy (get-capacity node))]
    (reduce #(add-one %1 pos-fn %2 :capacity capacity) node coll)))


(def to-jts (partial comp p))

;; querying a quadtree

(defn query-geom
  [quadtree position-fn geometry]
  {:pre [(two-d? geometry)]}
  (let [zipper (quad-zip quadtree)
        pred (covered-by-node-predicate geometry)
        candidates (data (z/node (follow-pred zipper pred)))]
   (filter
     (comp (partial r/covers? geometry) position-fn)
     candidates)))


(defn query
  [quadtree position-fn center radius]
  (let [position-fn (to-jts position-fn)
        bbox (circle-bounding-box center radius)
        zipper (quad-zip quadtree)
        pred (covered-by-node-predicate bbox)
        candidates (data (z/node (follow-pred zipper pred)))
        ]
    (filter
      (comp within-radius position-fn)  ; have to extract position
      candidates)))                     ; before passing to JTS interop

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

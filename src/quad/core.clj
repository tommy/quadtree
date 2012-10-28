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
  [boundary & {:keys [position-fn
                      capacity
                      children
                      data]
               :or   {children []
                      data     #{}}}]
  (with-meta
    (Node. boundary children data)
    {:position-fn position-fn
     :capacity capacity}))

(defn get-position-fn
  "The position-fn of a quadtree is the fn that is used to
  get the position of each item in the quadtree. It should
  return JTS Point objects."
  [quadtree]
  {:post [(complement nil?)]}
  (:position-fn (meta quadtree)))

(defn get-capacity
  "The capacity of the quadtree is the number of data
  elements a node may contain before it should be divided."
  [quadtree]
  {:post [(complement nil?)]}
  (:capacity (meta quadtree)))



;; splitting nodes

(defn split
  "Split a node into quadrants and add its data so that
  it descends into the correct child node."
  [n]
  {:post [(node? %)
          (every? node? (:quads %))]}
  (let [pos-fn (get-position-fn n)
        capacity (get-capacity n)
        b (:boundary n)
        nodes (map
                #(node % :position-fn pos-fn :capacity capacity)
                (quadrants b))
        data (:data n)]
    (-> (node b :children nodes :position-fn pos-fn :capacity capacity)
        (add data))))

(defn check-capacity
  "Check if node has more than capacity elements of data.
  If so, split the node into quadrants."
  [node]
  {:pre [((complement nil?) (get-capacity node))
         ((complement nil?) node)]}
  (let [n (count (:data node))
        capacity (get-capacity node)]
    (cond
      (<= capacity 0) node
      (<= n capacity) node
      :otherwise (split node))))

(defn- add-data
  "Add an object to the given node's data field."
  [node e]
  {:pre [((complement nil?) (get-capacity node))]}
  (-> node
      (update-in [:data] conj e)
      check-capacity))

(def- leaf? (comp empty? :quads))

(defn data
  "The data stored in this node and all child nodes."
  [node]
  (let [d (:data node)
        qs (:quads node)]
    (reduce into
      (set d)
      (map data qs))))


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
  [node e]
  {:pre [(r/covers? (:boundary node) ((get-position-fn node) e))
         ((complement nil?) (get-capacity node))]}
  (let [pos-fn (get-position-fn node)
        pos (pos-fn e)
        pred (covered-by-node-predicate pos)
        loc (->
            (quad-zip node)
            (follow-pred pred))]
    (z/root
      (z/edit
        loc
        add-data e))))

(defn add
  "Add a collection of objects to the quadtree rooted at node."
  [node coll]
  {:post [(clojure.set/subset? (set coll) (data %))]}
  (reduce add-one node coll))


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

(defn quad
  "Construct a quadtree object.

  corner1     - a vector [x y] specifying one corner of the boundary
  corner2     - a vector [x y] specifying the opposite corner
  position-fn - a fn of one argument that returns the position [x y]
                of each member of coll
  depth       - the depth of the tree (3 or 4 is recommended)
  coll        - a collection of objects to be put in the quadtree."
  [corner1 corner2 position-fn capacity coll]
  (let [position-fn (to-jts position-fn)
        boundary (rectangle
                   (p corner1)
                   (p corner2))
        empty-tree 
          (with-meta
            (node boundary)
            {:position-fn position-fn
             :capacity capacity})]
    (add empty-tree coll)))

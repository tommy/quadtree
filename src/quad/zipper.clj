(ns quad.zipper
  (:require [clojure.zip :as z]))

(defn follow-pred
  "Descends a zipper, following the path of nodes for which
  pred is true. Returns the zipper at the last location
  for which pred is true."
  [zipper leaf? pred]
  {:pre [(pred (z/node zipper))]}
  (if (leaf? (z/node zipper))            ; if the node is a leaf
    zipper                               ; then we are done
    (loop [zipper (z/down zipper)]       ; otherwise, loop through the children
      (if (pred (z/node zipper))         ; if pred is true for this child,
        (follow-pred zipper leaf? pred)  ;   continue down
                                         ; if pred is false for this child,
        (if (nil? (z/right zipper))      ;   and this is the last child,
          (z/up zipper)                  ;   return the parent
          (recur (z/right zipper))       ; otherwise, try the next sibling
      )))))


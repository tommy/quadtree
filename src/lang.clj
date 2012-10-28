(ns lang
  "Generic Clojure utility methods that I find makes coding
  easier or more succint.")

(defn atom?
  "True if x is an atom."
  [x]
  (= clojure.lang.Atom
    (class x)))

(defn atom-
  "Wraps r in a atom, unless r is already a atom."
  [r]
  (if (atom? r) r (atom r)))

(defmacro def-
  "Same as def, yeilding a non-public def.
  Analagous to defn-."
  [name & decls]
  (list* `def ^:private name decls))

(defn copy-meta
  "Returns a copy of 'to' with the metadata of 'from'."
  [from to]
  (with-meta to (meta from)))

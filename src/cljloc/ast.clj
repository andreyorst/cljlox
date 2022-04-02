(ns cljloc.ast
  (:require [cljloc.protocols :refer [IStringable tostring]])
  (:import [cljloc.tokenizer Token]))

(defrecord Binary [left, ^Token operator, right])
(defrecord Unary [^Token operator, right])
(defrecord Grouping [expression])
(defrecord Literal [value])

(extend-type Binary
  IStringable
  (tostring [self]
    (format "(%s %s %s)"
            (tostring (:operator self))
            (tostring (:left self))
            (tostring (:right self)))))

(extend-type Unary
  IStringable
  (tostring [self]
    (format "(%s %s)"
            (tostring (:operator self))
            (tostring (:right self)))))

(extend-type Grouping
  IStringable
  (tostring [self]
    (format "(group %s)"
            (tostring (:expression self)))))

(extend-type Literal
  IStringable
  (tostring [self]
    (if-some [value (:value self)]
      (tostring value)
      "nil")))

(defn tostring-expr [expr]
  (tostring expr))

(defn print-ast [expr]
  (println (tostring-expr expr)))

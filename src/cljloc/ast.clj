(ns cljloc.ast
  (:require [cljloc.protocols :refer [IStringable tostring]])
  (:import [cljloc.tokenizer Token]))

(defrecord Binary [left, ^Token operator, right])
(defrecord Unary [^Token operator, right])
(defrecord Grouping [expression])
(defrecord Literal [value])
(defrecord Variable [^Token name])
(defrecord Assign [^Token name, value])

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

(extend-type Variable
  IStringable
  (tostring [self]
    (:name self)))

(defrecord Expression [expression])
(defrecord Print [expression])
(defrecord Var [^Token name initializer])

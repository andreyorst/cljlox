(ns cljloc.ast
  (:require [cljloc.protocols :refer [IStringable tostring]]
            [cljloc.tokenizer])
  (:import [cljloc.tokenizer Token])
  (:gen-class))

(defrecord Binary [left, ^Token operator, right])
(defrecord Unary [^Token operator, right])
(defrecord Grouping [expression])
(defrecord Literal [value])
(defrecord Logical [left, ^Token operator, right])
(defrecord Variable [^Token name])
(defrecord Assign [^Token name, value])
(defrecord Expression [expression])
(defrecord Print [expression])
(defrecord Var [^Token name initializer])
(defrecord Block [statements])
(defrecord If [condition, then, else])
(defrecord While [condition, body])
(defrecord For [initializer, body])
(defrecord Break [^Token break])

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

(ns cljloc.ast
  (:require [cljloc.protocols :refer [IStringable tostring]]
            [cljloc.tokenizer]
            [clojure.string :as str])
  (:import [cljloc.tokenizer Token])
  (:gen-class))

(defrecord Binary [left, ^Token operator, right])
(defrecord Call [callee, ^Token paren, arguments])
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

(extend-type Call
  IStringable
  (tostring [{:keys [callee arguments]}]
    (if (seq arguments)
      (format "(%s %s)" (tostring callee) (str/join " " (map tostring arguments)))
      (format "(%s)" (tostring callee)))))

(extend-type Unary
  IStringable
  (tostring [self]
    (format "(%s %s)"
            (tostring (:operator self))
            (tostring (:right self)))))

(extend-type Grouping
  IStringable
  (tostring [self]
    (format "(do %s)"
            (tostring (:expression self)))))

(extend-type Literal
  IStringable
  (tostring [self]
    (if-some [value (:value self)]
      (let [res (tostring value)]
        (if (and (number? value)
                 (re-find #"\.0$" res))
          (str/replace res #"\.0$" "")
          res))
      "nil")))

(extend-type Logical
  IStringable
  (tostring [self]
    (format "(%s %s %s)"
            (tostring (:operator self))
            (tostring (:left self))
            (tostring (:right self)))))

(extend-type Variable
  IStringable
  (tostring [self]
    (:name self)))

(extend-type Assign
  IStringable
  (tostring [self]
    (format "(set %s %s)"
            (tostring (:lexeme (:name self)))
            (tostring (:value self)))))

(extend-type Expression
  IStringable
  (tostring [self]
    (str self)))

(extend-type Print
  IStringable
  (tostring [self]
    (format "(print %s)" (tostring (:expression self)))))

(extend-type Var
  IStringable
  (tostring [{:keys [name initializer]}]
    (if initializer
      (format "(var %s %s)" (:lexeme name) (tostring initializer))
      (format "(var %s nil)" (:lexeme name)))))

(extend-type Block
  IStringable
  (tostring [{:keys [statements]}]
    (format "(do %s)" (str/join " " (map tostring statements)))))

(extend-type If
  IStringable
  (tostring [{:keys [condition then else]}]
    (if else
      (format "(if %s %s %s)" (tostring condition) (tostring then) (tostring else))
      (format "(if %s %s)" (tostring condition) (tostring then)))))

(extend-type While
  IStringable
  (tostring [{:keys [condition body]}]
    (format "(while %s %s)" (tostring condition) (tostring body))))

(extend-type Break
  IStringable
  (tostring [_]
    "(break)"))

(defn tostring-ast [ast]
  (tostring ast))

(ns cljloc.ast
  (:require [cljloc.protocols :refer [IStringable tostring]]
            [cljloc.tokenizer]
            [clojure.string :as str])
  (:import [cljloc.tokenizer Token])
  (:gen-class))

(defrecord Binary [left, ^Token operator, right]
  IStringable
  (tostring [{:keys [operator left right]}]
    (format "(%s %s %s)" (tostring operator) (tostring left) (tostring right))))

(defrecord Call [callee, ^Token paren, arguments]
  IStringable
  (tostring [_]
    (if (seq arguments)
      (format "(%s %s)" (tostring callee) (str/join " " (map tostring arguments)))
      (format "(%s)" (tostring callee)))))

(defrecord Unary [^Token operator, right]
  IStringable
  (tostring [_]
    (format "(%s %s)" (tostring operator) (tostring right))))

(defrecord Grouping [expression]
  IStringable
  (tostring [_]
    (format "(do %s)" (tostring expression))))

(defrecord Literal [value]
  IStringable
  (tostring [_]
    (if (some? value)
      (let [res (tostring value)]
        (if (and (number? value)
                 (re-find #"\.0$" res))
          (str/replace res #"\.0$" "")
          res))
      "nil")))

(defrecord Logical [left, ^Token operator, right]
  IStringable
  (tostring [_]
    (format "(%s %s %s)" (tostring operator) (tostring left) (tostring right))))

(defrecord Variable [^Token name]
  IStringable
  (tostring [_]
    (:lexeme name)))

(defrecord Assign [^Token name, value]
  IStringable
  (tostring [_]
    (format "(set %s %s)"
            (tostring (:lexeme name))
            (tostring value))))

(defrecord Expression [expression]
  IStringable
  (tostring [self]
    (str self)))

(defrecord Print [expression]
  IStringable
  (tostring [_]
    (format "(print %s)" (tostring expression))))

(defrecord Var [^Token name initializer]
  IStringable
  (tostring [_]
    (if initializer
      (format "(var %s %s)" (:lexeme name) (tostring initializer))
      (format "(var %s nil)" (:lexeme name)))))

(defrecord Block [statements]
  IStringable
  (tostring [_]
    (format "(do %s)" (str/join " " (map tostring statements)))))

(defrecord If [condition, then, else]
  IStringable
  (tostring [_]
    (if else
      (format "(if %s %s %s)" (tostring condition) (tostring then) (tostring else))
      (format "(if %s %s)" (tostring condition) (tostring then)))))

(defrecord While [condition, body]
  IStringable
  (tostring [_]
    (format "(while %s %s)" (tostring condition) (tostring body))))

(defrecord For [initializer, body])

(defrecord Break [^Token break]
  IStringable
  (tostring [_] "(break)"))

(defrecord LoxCallable [arity, function]
  IStringable
  (tostring [_] "#<native fn>"))

(defrecord Function [^Token name, params, body]
  IStringable
  (tostring [_]
    (format "(fn %s [%s] %s)"
            (tostring name)
            (str/join " " (map tostring params))
            (tostring body))))

(defrecord Return [^Token keyword, value]
  IStringable
  (tostring [_]
    (if (some? value)
      (format "(return %s)" (tostring value))
      (format "(return)"))))

(defrecord Get [object, ^Token name]
  IStringable
  (tostring [_]
    (format "%s.%s" (tostring object) (tostring name))))

(defrecord Set [object, ^Token name, val]
  IStringable
  (tostring [_]
    (format "(set %s.%s %s)" (tostring object) (tostring name) (tostring val))))

(defrecord This [^Token keyword]
  IStringable
  (tostring [_]
    "this"))

(defrecord Super [^Token keyword, method]
  IStringable
  (tostring [_]
    (format "super.%s" (tostring method))))

(defrecord LoxClassStatement [^Token name, ^Variable superclass, methods]
  IStringable
  (tostring [_]
    (format "(class %s %s)"
            (if superclass
              (format "(extends %s %s)" (tostring name) (tostring (:name superclass)))
              (tostring name))
            (->> methods
                 (map tostring)
                 (str/join " ")))))

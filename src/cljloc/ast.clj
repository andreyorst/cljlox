(ns cljloc.ast
  (:require [clojure.string :as str]))

(defmacro define-ast [& specs]
  (list* 'do
         (for [spec specs]
           (let [[name fields] (str/split spec #"\s+:\s+")
                 fields (->> (str/split fields #",?\s+")
                             (partition 2)
                             (map second)
                             (reduce (fn [fields name]
                                       (conj fields (symbol name)))
                                     []))]
             `(defrecord ~(symbol name) ~fields)))))


(define-ast
  "Binary   : Expr left, Token operator, Expr right"
  "Grouping : Expr expression"
  "Literal  : Object value"
  "Unary    : Token operator, Expr right")

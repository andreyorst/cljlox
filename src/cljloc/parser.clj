(ns cljloc.parser
  (:require [cljloc.ast :refer [->Binary ->Unary ->Grouping ->Literal] :as ast]
            [cljloc.tokenizer :refer [tokenize]]))

(declare expression)

(defn parse [tokens]
  (first (expression tokens 0)))

(defn consume [tokens current type message]
  (if (#{type} (:type (get tokens current)))
    (inc current)
    (throw (ex-info message {}))))

(defn previous [tokens current]
  (get tokens (dec current)))

(defn primary [tokens current]
  (let [token (get tokens current)
        current (inc current)]
    (cond
      (= :true (:type token)) [(->Literal true) current]
      (= :false (:type token)) [(->Literal false) current]
      (= :nil (:type token)) [(->Literal nil) current]
      (#{:number :string} (:type token)) [(->Literal (previous tokens current)) current]
      (= :left_paren (:type token))
      (let [[expr current] (expression tokens current)
            current (consume tokens current :right_paren "Expect ')' after expression.")]
        [(->Grouping expr) current]))))

(defn unary [tokens current]
  (if (#{:bang :minus} (:type (get tokens current)))
    (let [current (inc current)
          operator (previous tokens current)
          [right current] (unary tokens current)]
      [(->Unary operator right) current])
    (primary tokens current)))

(defn factor [tokens current]
  (loop [[expr current] (unary tokens current)]
    (if (#{:slash :star} (:type (get tokens current)))
      (let [current (inc current)
            operator (previous tokens current)
            [right current] (unary tokens current)]
        (recur [(->Binary expr operator right) current]))
      [expr current])))

(defn term [tokens current]
  (loop [[expr current] (factor tokens current)]
    (if (#{:minus :plus} (:type (get tokens current)))
      (let [current (inc current)
            operator (previous tokens current)
            [right current] (factor tokens current)]
        (recur [(->Binary expr operator right) current]))
      [expr current])))

(defn comparison [tokens current]
  (loop [[expr current] (term tokens current)]
    (if (#{:greater :greater_equal :less :less_equal} (:type (get tokens current)))
      (let [current (inc current)
            operator (previous tokens current)
            [right current] (term tokens current)]
        (recur [(->Binary expr operator right) current]))
      [expr current])))

(defn equality [tokens current]
  (loop [[expr current] (comparison tokens current)]
    (if (#{:bang_equal :equal_equal} (:type (get tokens current)))
      (let [current (inc current)
            operator (previous tokens current)
            [right current] (comparison tokens current)]
        (recur [(->Binary expr operator right) current]))
      [expr current])))

(defn expression [tokens current]
  (equality tokens current))

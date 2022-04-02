(ns cljloc.parser
  "A recursive descent parser that implements the following grammar:

   expression : equality ;
   equality   : comparison (('!=' | '==') comparison)* ;
   comparison : term (('>' | '>=' | '<' | '<=') term)* ;
   term       : factor (('-' | '+') factor)* ;
   factor     : unary (('/' | '*') unary)* ;
   unary      : ('!' | '-') unary
              | primary ;
   primary    : NUMBER | STRING | 'true' | 'false' | 'nil'
              | '(' expression ')' ;"
  (:require [cljloc.ast :refer [->Binary ->Unary ->Grouping ->Literal] :as ast]
            [clojure.tools.logging :as log])
  (:import [clojure.lang ExceptionInfo]))

(defn- consume [tokens n type message]
  (if (#{type} (:type (get tokens n)))
    (inc n)
    (throw (ex-info message {:tokens tokens
                             :n n}))))

(defn- current [tokens n]
  (if-some [token (get tokens n)]
    token
    (throw (ex-info "Unfinished expression" {:tokens tokens
                                             :n (dec n)}))))

(defn at-end? [tokens n]
  (= :eof (-> tokens (get n) :type)))

(defn- previous [tokens n]
  (current tokens (dec n)))

(declare expression)

(defn- primary [tokens n]
  (let [token (current tokens n)
        n (inc n)]
    (case (:type token)
      :true [(->Literal true) n]
      :false [(->Literal false) n]
      :nil [(->Literal nil) n]
      (:number | :string) [(->Literal (previous tokens n)) n]
      :left_paren
      (let [[expr n] (expression tokens n)
            n (consume tokens n :right_paren "Expect ')' after expression.")]
        [(->Grouping expr) n])
      :eof
      [:eof n]
      (throw (ex-info "Unsupported token" {:tokens tokens :n (dec n)})))))

(defn- unary [tokens n]
  (if (#{:bang :minus} (:type (current tokens n)))
    (let [n (inc n)
          operator (previous tokens n)
          [right n] (unary tokens n)]
      [(->Unary operator right) n])
    (primary tokens n)))

(defn- factor [tokens n]
  (loop [[expr n] (unary tokens n)]
    (if (and (not= expr :eof) (#{:slash :star} (:type (current tokens n))))
      (let [n (inc n)
            operator (previous tokens n)
            [right n] (unary tokens n)]
        (recur [(->Binary expr operator right) n]))
      [expr n])))

(defn- term [tokens n]
  (loop [[expr n] (factor tokens n)]
    (if (and (not= expr :eof) (#{:minus :plus} (:type (current tokens n))))
      (let [n (inc n)
            operator (previous tokens n)
            [right n] (factor tokens n)]
        (recur [(->Binary expr operator right) n]))
      [expr n])))

(defn- comparison [tokens n]
  (loop [[expr n] (term tokens n)]
    (if (and (not= expr :eof) (#{:greater :greater_equal :less :less_equal} (:type (current tokens n))))
      (let [n (inc n)
            operator (previous tokens n)
            [right n] (term tokens n)]
        (recur [(->Binary expr operator right) n]))
      [expr n])))

(defn- equality [tokens n]
  (loop [[expr n] (comparison tokens n)]
    (if (and (not= expr :eof) (#{:bang_equal :equal_equal} (:type (current tokens n))))
      (let [n (inc n)
            operator (previous tokens n)
            [right n] (comparison tokens n)]
        (recur [(->Binary expr operator right) n]))
      [expr n])))

(defn- expression [tokens n]
  (equality tokens n))

(defn syncronize [tokens n]
  (loop [n (inc n)]
    (when-not (at-end? tokens n)
      (cond (not= (:type (previous tokens n)) :semicolon)
            10
            (not (#{:class :fun :var :for :if :while :print :return} (:type (current tokens n))))
            20
            :else
            (recur (inc n))))))

(defn parse
  "Parse a sequence of `Token`s into a sequence of expressions."
  [tokens]
  (try
    (loop [exprs []
           n 0]
      (if (< n (dec (count tokens)))
        (let [[expr n] (expression tokens n)]
          (recur (conj exprs expr) n))
        exprs))
    (catch ExceptionInfo e
      (let [{:keys [tokens n]} (ex-data e)
            token (get tokens n)
            [line col] (:pos token)]
        (log/errorf "[%s:%s] %s at '%s'" line col (ex-message e) (str token))))))

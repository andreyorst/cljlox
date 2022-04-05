(ns cljloc.parser
  "A recursive descent parser.
  Main entry point is a `parse` function."
  (:require [cljloc.ast :refer [->Binary ->Unary ->Grouping ->Literal ->Print ->Expression ->Var ->Variable ->Assign ->Block]])
  (:import [clojure.lang ExceptionInfo]
           [cljloc.ast Variable]))

(defn- parse-error
  ([message] (parse-error message {}))
  ([message data]
   (throw (ex-info message (assoc data :type ::parse-error)))))

(defn- consume [tokens n type message]
  (let [token (get tokens n)]
    (if (#{type} (:type token))
      [token (inc n)]
      (parse-error message {:tokens tokens :n n}))))

(defn- current [tokens n]
  (if-some [token (get tokens n)]
    token
    (parse-error "Unfinished expression" {:tokens tokens :n (dec n)})))

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
      (:number | :string) [(->Literal (:literal (previous tokens n))) n]
      :left_paren
      (let [[expr n] (expression tokens n)
            [_ n] (consume tokens n :right_paren "Expect ')' after expression.")]
        [(->Grouping expr) n])
      :identifier [(->Variable token) n]
      :eof
      [:eof n]
      (parse-error "Unsupported token" {:tokens tokens :n (dec n)}))))

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

(defn- assignment [tokens n]
  (let [[expr n] (equality tokens n)]
    (if (= :equal (:type (get tokens n)))
      (let [equal-n n
            [value n] (assignment tokens (inc n))]
        (if (instance? Variable expr)
          [(->Assign (:name expr) value) n]
          (parse-error "Invalid assignment target." {:tokens tokens :n equal-n})))
      [expr n])))

(defn- expression [tokens n]
  (assignment tokens n))

(defn- synchronize [tokens n]
  (loop [n (inc n)]
    (if-not (at-end? tokens n)
      (cond (= (:type (previous tokens n)) :semicolon)
            n
            (#{:class :fun :var :for :if :while :print :return} (:type (current tokens n)))
            n
            :else
            (recur (inc n)))
      n)))

(defn print-statement [tokens n]
  (let [[expr n] (expression tokens n)]
    [(->Print expr) (second (consume tokens n :semicolon "Expect ';' after value."))]))

#_
(defn expression-statement [tokens n]
  (let [[expr n] (expression tokens n)]
    [(->Expression expr) (second (consume tokens n :semicolon "Expect ';' after value."))]))

(declare declaration)

(defn block [tokens n]
  (loop [statements []
         n n]
    (let [token (get tokens n)]
      (case (:type token)
        (:eof | :right_brace)
        [(->Block statements)
         (second (consume tokens n :right_brace "Expect '}' after block."))]
        (let [[statement n] (declaration tokens n)]
          (recur (conj statements statement) n))))))

(defn statement [tokens n]
  (let [token (current tokens n)
        n' (inc n)]
    (case (:type token)
      :print (print-statement tokens n')
      :identifier (expression tokens n)
      :left_brace (block tokens n')
      (expression tokens n))))

(defn var-declaration [tokens n]
  (let [[name n'] (consume tokens n :identifier "Expect variable name.")
        [value n'] (if (= :equal (:type (get tokens n')))
                     (expression tokens (inc n'))
                     [nil n'])]
    [(->Var name value) (second (consume tokens n' :semicolon "Expect ';' after value."))]))

(defn declaration [tokens n]
  (try
    (let [token (current tokens n)
          n' (inc n)]
      (if (= :var (:type token))
        (var-declaration tokens n')
        (statement tokens n)))
    (catch ExceptionInfo e
      (let [{type :type} (ex-data e)]
        (if (= type ::parse-error)
          [nil (synchronize tokens n)]
          (throw e))))))

(defn parse
  "Parse a sequence of `Token`s into a sequence of `AST` expressions."
  [tokens]
  (try
    (loop [exprs []
           n 0]
      (if (< n (dec (count tokens)))
        (let [[expr n] (declaration tokens n)]
          (recur (if expr (conj exprs expr) exprs) n))
        exprs))
    (catch ExceptionInfo e
      (let [{:keys [tokens n]} (ex-data e)
            token (get tokens n)
            [line col] (:pos token)]
        (binding [*out* *err*]
          (println "[%s:%s] %s at '%s'" line col (ex-message e) (str token)))))))

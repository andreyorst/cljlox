(ns cljloc.parser
  "A recursive descent parser.
  Main entry point is a `parse` function."
  (:require [cljloc.ast :as ast]
            [cljloc.macros :refer [with-out-err]])
  (:import [cljloc.ast
            Binary Unary Grouping Literal Print Expression Var Variable
            Assign Block If Logical While Break Call Function Return
            LoxClassStatement Get Set This Super]
           [clojure.lang ExceptionInfo]))

(defn- parse-error [message data]
  (throw (ex-info message (assoc data :type ::parse-error))))

(defn- consume [tokens n type message]
  (let [token (get tokens n)]
    (if (#{type} (:type token))
      [token (inc n)]
      (parse-error message {:tokens tokens :n n}))))

(defn- current [tokens n]
  (if-some [token (get tokens n)]
    token
    (parse-error "Unfinished expression" {:tokens tokens :n (dec n)})))

(defn- at-end? [tokens n]
  (= :eof (-> tokens (get n) :type)))

(defn- previous [tokens n]
  (current tokens (dec n)))

(declare expression)
(declare fn-declaration)

(defn- primary [tokens n]
  (let [token (current tokens n)
        n (inc n)]
    (case (:type token)
      :true [(Literal. true) n]
      :false [(Literal. false) n]
      :nil [(Literal. nil) n]
      (:number | :string) [(Literal. (:literal (previous tokens n))) n]
      :left_paren
      (let [[expr n] (expression tokens n)
            [_ n] (consume tokens n :right_paren "Expect ')' after expression.")]
        [(Grouping. expr) n])
      :identifier [(Variable. token) n]
      :semicolon [nil n]
      :fun (fn-declaration tokens n "function")
      :this [(This. token) n]
      :super (let [[_ n] (consume tokens n :dot "Expect '.' after 'super'.")
                   [method n] (consume tokens n :identifier "Expect superclass method name.")]
               [(Super. token method) n])
      :eof
      [:eof n]
      (parse-error "Unsupported token" {:tokens tokens :n (dec n)}))))

(defn- finish-call [callee tokens n]
  (let [[args n]
        (if (not= :right_paren (:type (current tokens n)))
          (loop [[expr n] (expression tokens n)
                 exprs []]
            (if (= :comma (:type (current tokens n)))
              (recur (expression tokens (inc n))
                     (conj exprs expr))
              [(conj exprs expr) n]))
          [[] n])
        [paren n] (consume tokens n :right_paren "Expect ')' after expression.")]
    (when (> (count args) 255)
      (parse-error "Can't have more than 255 arguments." {:tokens tokens :n (dec n)}))
    [(Call. callee paren args) n]))

(defn- call [tokens n]
  (loop [[expr n] (primary tokens n)]
    (case (:type (current tokens n))
      :left_paren (recur (finish-call expr tokens (inc n)))
      :dot (let [[name n] (consume tokens (inc n) :identifier "Expect property name after '.'.")]
             (recur [(Get. expr name) n]))
      [expr n])))

(defn- unary [tokens n]
  (if (#{:bang :minus} (:type (current tokens n)))
    (let [n (inc n)
          operator (previous tokens n)
          [right n] (unary tokens n)]
      [(Unary. operator right) n])
    (call tokens n)))

(defn- factor [tokens n]
  (loop [[expr n] (unary tokens n)]
    (if (and (not= expr :eof) (#{:slash :star} (:type (current tokens n))))
      (let [n (inc n)
            operator (previous tokens n)
            [right n] (unary tokens n)]
        (recur [(Binary. expr operator right) n]))
      [expr n])))

(defn- term [tokens n]
  (loop [[expr n] (factor tokens n)]
    (if (and (not= expr :eof) (#{:minus :plus} (:type (current tokens n))))
      (let [n (inc n)
            operator (previous tokens n)
            [right n] (factor tokens n)]
        (recur [(Binary. expr operator right) n]))
      [expr n])))

(defn- comparison [tokens n]
  (loop [[expr n] (term tokens n)]
    (if (and (not= expr :eof) (#{:greater :greater_equal :less :less_equal} (:type (current tokens n))))
      (let [n (inc n)
            operator (previous tokens n)
            [right n] (term tokens n)]
        (recur [(Binary. expr operator right) n]))
      [expr n])))

(defn- equality [tokens n]
  (loop [[expr n] (comparison tokens n)]
    (if (and (not= expr :eof) (#{:bang_equal :equal_equal} (:type (current tokens n))))
      (let [n (inc n)
            operator (previous tokens n)
            [right n] (comparison tokens n)]
        (recur [(Binary. expr operator right) n]))
      [expr n])))

(defn- and-expr [tokens n]
  (loop [[expr n] (equality tokens n)]
    (let [token (current tokens n)]
      (if (and (not= (:type token) :eof)
               (= :and (:type token)))
        (let [operator token
              [right n] (equality tokens (inc n))]
          (recur [(Logical. expr operator right) n]))
        [expr n]))))

(defn- or-expr [tokens n]
  (loop [[expr n] (and-expr tokens n)]
    (let [token (current tokens n)]
      (if (and (not= (:type token) :eof)
               (= :or (:type token)))
        (let [operator token
              [right n] (and-expr tokens (inc n))]
          (recur [(Logical. expr operator right) n]))
        [expr n]))))

(defn- assignment [tokens n]
  (let [[expr n] (or-expr tokens n)]
    (if (= :equal (:type (get tokens n)))
          (let [equal-n n
                [value n] (assignment tokens (inc n))]
            (cond (instance? Variable expr)
                  [(Assign. (:name expr) value) n]
                  (instance? Get expr)
                  [(Set. (:object expr) (:name expr) value) n]
                  :else
                  (parse-error "Invalid assignment target." {:tokens tokens :n equal-n})))
          [expr n])))

(defn- expression [tokens n]
  (assignment tokens n))

(defn- synchronize [tokens n]
  (loop [n (inc n)]
    (if-not (at-end? tokens n)
      (cond (= (:type (previous tokens n)) :semicolon)
            n
            (#{:class :fun :var :for :if :while :print :return :break} (:type (current tokens n)))
            n
            :else
            (recur (inc n)))
      n)))

(defn- print-statement [tokens n]
  (let [[expr n] (expression tokens n)]
    [(Print. expr) (second (consume tokens n :semicolon "Expect ';' after value."))]))

(defn- expression-statement [tokens n]
  (let [[expr n] (expression tokens n)]
    [(Expression. expr) (second (consume tokens n :semicolon "Expect ';' after value."))]))

(declare declaration)
(declare statement)
(declare var-declaration)

(defn- block [tokens n]
  (loop [statements []
         n n]
    (let [token (get tokens n)]
      (case (:type token)
        (:eof | :right_brace)
        [statements (second (consume tokens n :right_brace "Expect '}' after block."))]
        (let [[statement n] (declaration tokens n)]
          (if statement
            (recur (conj statements statement) n)
            (recur statements n)))))))

(defn- if-statement [tokens n]
  (let [[_ n] (consume tokens n :left_paren "Expect '(' after 'if'.")
        [condition n] (expression tokens n)
        [_ n] (consume tokens n :right_paren "Expect ')' after if condition.")
        [then n] (statement tokens n)]
    (if (= :else (:type (current tokens n)))
      (let [[else n] (statement tokens (inc n))]
        [(If. condition then else) n])
      [(If. condition then nil) n])))

(defn- while-statement [tokens n]
  (let [[_ n] (consume tokens n :left_paren "Expect '(' after 'while'.")
        [condition n] (expression tokens n)
        [_ n] (consume tokens n :right_paren "Expect ')' after while condition.")
        [body n] (statement tokens n)]
    [(While. condition body) n]))

(defn- for-statement [tokens n]
  (let [[_ n] (consume tokens n :left_paren "Expect '(' after 'for'.")
        [initializer n] (case (:type (current tokens n))
                          :semicolon [nil (inc n)]
                          :var (var-declaration tokens (inc n))
                          (expression-statement tokens (inc n)))
        [condition n] (if (not= :semicolon (:type (current tokens n)))
                        (expression tokens n)
                        [(Literal. true) n])
        [_ n] (consume tokens n :semicolon "Expect ';' after loop condition.")
        [increment n] (if (not= :right_paren (:type (current tokens n)))
                        (statement tokens n)
                        [nil n])
        [_ n] (consume tokens n :right_paren "Expect ')' after for clauses.")
        [body n] (statement tokens n)
        body (While. condition
                     (if increment
                       (Block. [body increment])
                       body))]
    (if initializer
      [(Block. [initializer body]) n]
      [body n])))

(defn- break-statement [tokens n]
  [(Break. (current tokens n)) (second (consume tokens (inc n) :semicolon "Expect ';' after break."))])

(defn- return-statement [tokens n]
  (let [token (current tokens n)
        n (inc n)]
    (if (= :semicolon (:type (current tokens n)))
      [(Return. token nil) (inc n)]
      (let [[value n] (statement tokens n)
            [_ n] (consume tokens n :semicolon "Expect ';' after loop condition.")]
        [(Return. token value) n]))))

(defn- statement [tokens n]
  (let [token (current tokens n)
        n' (inc n)]
    (case (:type token)
      :if (if-statement tokens n')
      :for (for-statement tokens n')
      :while (while-statement tokens n')
      :return (return-statement tokens n)
      :print (print-statement tokens n')
      :identifier (expression tokens n)
      :left_brace (let [[expressions n] (block tokens n')]
                    [(Block. expressions) n])
      :break (break-statement tokens n)
      (expression tokens n))))

(defn- var-declaration [tokens n]
  (let [[name n'] (consume tokens n :identifier "Expect variable name.")
        [value n'] (if (= :equal (:type (get tokens n')))
                     (expression tokens (inc n'))
                     [nil n'])]
    [(Var. name value) (second (consume tokens n' :semicolon "Expect ';' after value."))]))

(defn- fn-declaration [tokens n kind]
  (let [fun-token-n (dec n)
        [name n] (try (consume tokens n :identifier (format "Expected %s name." kind))
                      (catch ExceptionInfo e
                        (let [{:keys [tokens n]} (ex-data e)]
                          (if (= :left_paren (:type (get tokens n)))
                            [nil n]
                            (throw e)))))
        [_ n] (consume tokens n :left_paren (format "Expected '(' after %s name." kind))
        [args n]
        (if (not= :right_paren (:type (current tokens n)))
          (loop [[expr n] (expression tokens n)
                 exprs []]
            (if (= :comma (:type (current tokens n)))
              (recur (expression tokens (inc n))
                     (conj exprs expr))
              [(conj exprs expr) n]))
          [[] n])
        [_ n] (consume tokens n :right_paren "Expect ')' after parameters.")
        [_ n] (consume tokens n :left_brace (format "Expect '{' before %s body." kind))
        [body n] (block tokens n)]
    (when (> (count args) 255)
      (parse-error "Can't have more than 255 arguments." {:tokens tokens :n fun-token-n}))
    [(Function. name args body) n]))

(defn- class-declaration [tokens n]
  (let [[name n] (consume tokens n :identifier "Expected class name.")
        [superclass n] (if (= :less (:type (current tokens n)))
                         (consume tokens (inc n) :identifier "Expect superclass name.")
                         [nil n])
        superclass (when superclass
                     (Variable. superclass))
        [_ n] (consume tokens n :left_brace "Expect '{' before class body.")
        [methods n]
        (loop [methods []
               n n]
          (let [token (current tokens n)]
            (if (#{:eof :right_brace} (:type token))
              [methods n]
              (let [[method n] (fn-declaration tokens n "method")]
                (recur (conj methods method) n)))))
        [_ n] (consume tokens n :right_brace "Expect '}' after class body.")]
    [(LoxClassStatement. name superclass methods) n]))

(defn- declaration [tokens n]
  (try
    (let [token (current tokens n)
          n' (inc n)]
      (case (:type token)
        :class (class-declaration tokens n')
        :fun (fn-declaration tokens n' "function")
        :var (var-declaration tokens n')
        (statement tokens n)))
    (catch ExceptionInfo e
      (let [{type :type} (ex-data e)]
        (if (= type ::parse-error)
          (let [{:keys [tokens n]} (ex-data e)
                token (get tokens n)
                [line col] (:pos token)]
            (binding [*out* *err*]
              (println (format "[%s:%s] %s at '%s'" line col (ex-message e) (str token))))
            [nil (synchronize tokens n)])
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
      (with-out-err
        (let [data (ex-data e)]
          (case (:type data)
            ::parse-error
            (let [{:keys [tokens n]} (ex-data e)
                  token (get tokens n)
                  [line col] (:pos token)]
              (println (format "[%s:%s] %s at '%s'" line col (ex-message e) (str token))))
            (println (format "Uncaught error: %s" (ex-message e)))))))))

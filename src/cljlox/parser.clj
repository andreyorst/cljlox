(ns cljlox.parser
  "A recursive descent parser.
  Main entry point is the `parse` function."
  (:require
   [cljlox.ast :as ast]
   [cljlox.macros :refer [with-out-err]])
  (:import
   (cljlox.ast Assign Binary Block Break Call Expression Function
               Get Grouping If Literal Logical LoxClassStatement
               Print Return Set Super This Unary Var Variable While)
   (clojure.lang ExceptionInfo)))

(declare expression)
(declare fn-declaration)
(declare declaration)
(declare statement)
(declare var-declaration)

(defn- parse-error [message data]
  (throw (ex-info message (assoc data :type ::parse-error))))

(defn- consume-token
  "Check the `n`th token `type` and returns the pair of the `token` and
  the next `n`.  If the token type doesn't match, raises an error with
  a given message."
  [tokens n type message]
  (let [token (get tokens n)]
    (if (#{type} (:type token))
      [token (inc n)]
      (parse-error message {:tokens tokens :n n}))))

(defn- consume
  "Same as `consume-token` but doesn't return a token."
  [tokens n type message]
  (nth (consume-token tokens n type message) 1))

(defn- current
  "Return the current token from the stream of tokens.  If there's no
  token to return raises an error."
  [tokens n]
  (if-some [token (get tokens n)]
    token
    (parse-error "Unfinished expression" {:tokens tokens :n (dec n)})))

(defn- at-end?
  "Check if current token is an EOF token."
  [tokens n]
  (= :eof (-> tokens (get n) :type)))

(defn- previous
  "Returns the previous token."
  [tokens n]
  (current tokens (dec n)))

;;; Grammar implementation

(defn- primary [tokens n]
  (let [token (current tokens n)
        n (inc n)]
    (case (:type token)
      (:true :false) [(Literal. (= (:type token) :true)) n]
      :nil [(Literal. nil) n]
      (:number :string) [(Literal. (:literal (previous tokens n))) n]
      :left_paren (let [[expr n] (expression tokens n)]
                    [(Grouping. expr)
                     (consume tokens n :right_paren "Expect ')' after expression.")])
      :identifier [(Variable. token) n]
      :semicolon [nil n]
      :fun (fn-declaration tokens n "function")
      :this [(This. token) n]
      :super (let [n (consume tokens n :dot "Expect '.' after 'super'.")
                   [method n] (consume-token tokens n :identifier "Expect superclass method name.")]
               [(Super. token method) n])
      :eof (parse-error "Unfinished expression" {:tokens tokens :n (dec n)})
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
        [paren n] (consume-token tokens n :right_paren "Expect ')' after expression.")]
    (when (> (count args) 255)
      (parse-error "Can't have more than 255 arguments." {:tokens tokens :n (dec n)}))
    [(Call. callee paren args) n]))

(defn- call [tokens n]
  (loop [[expr n] (primary tokens n)]
    (case (:type (current tokens n))
      :left_paren (recur (finish-call expr tokens (inc n)))
      :dot (let [[name n] (consume-token tokens (inc n) :identifier "Expect property name after '.'.")]
             (recur [(Get. expr name) n]))
      [expr n])))

(defn- unary [tokens n]
  (case (:type (current tokens n))
    (:bang :minus) (let [n (inc n)
                         operator (previous tokens n)
                         [right n] (unary tokens n)]
                     [(Unary. operator right) n])
    (call tokens n)))

(defn- factor [tokens n]
  (loop [[expr n] (unary tokens n)]
    (case (:type (current tokens n))
      (:slash :star)
      (let [n (inc n)
            operator (previous tokens n)
            [right n] (unary tokens n)]
        (recur [(Binary. expr operator right) n]))
      [expr n])))

(defn- term [tokens n]
  (loop [[expr n] (factor tokens n)]
    (case (:type (current tokens n))
      (:minus :plus)
      (let [n (inc n)
            operator (previous tokens n)
            [right n] (factor tokens n)]
        (recur [(Binary. expr operator right) n]))
      [expr n])))

(defn- comparison [tokens n]
  (loop [[expr n] (term tokens n)]
    (case (:type (current tokens n))
      (:greater :greater_equal :less :less_equal)
      (let [n (inc n)
            operator (previous tokens n)
            [right n] (term tokens n)]
        (recur [(Binary. expr operator right) n]))
      [expr n])))

(defn- equality [tokens n]
  (loop [[expr n] (comparison tokens n)]
    (case (:type (current tokens n))
      (:bang_equal :equal_equal)
      (let [n (inc n)
            operator (previous tokens n)
            [right n] (comparison tokens n)]
        (recur [(Binary. expr operator right) n]))
      [expr n])))

(defn- and-expr [tokens n]
  (loop [[expr n] (equality tokens n)]
    (let [token (current tokens n)]
      (if (and (not (at-end? tokens n))
               (= :and (:type token)))
        (let [operator token
              [right n] (equality tokens (inc n))]
          (recur [(Logical. expr operator right) n]))
        [expr n]))))

(defn- or-expr [tokens n]
  (loop [[expr n] (and-expr tokens n)]
    (let [token (current tokens n)]
      (if (and (not (at-end? tokens n))
               (= :or (:type token)))
        (let [operator token
              [right n] (and-expr tokens (inc n))]
          (recur [(Logical. expr operator right) n]))
        [expr n]))))

(defn- assignment [tokens n]
  (let [[expr n] (or-expr tokens n)]
    (case (:type (get tokens n))
      :equal (let [equal-n n
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
      (if (or (= :semicolon (:type (previous tokens n)))
              (#{:class :fun :var :for :if :while :print :return :break}
               (:type (current tokens n))))
        n
        (recur (inc n)))
      n)))

(defn- print-statement [tokens n]
  (let [[expr n] (expression tokens n)]
    [(Print. expr)
     (consume tokens n :semicolon "Expect ';' after value.")]))

(defn- expression-statement [tokens n]
  (let [[expr n] (expression tokens n)]
    [(Expression. expr)
     (consume tokens n :semicolon "Expect ';' after value.")]))

(defn- block [tokens n]
  (loop [statements []
         n n]
    (let [token (get tokens n)]
      (case (:type token)
        (:eof :right_brace)
        [statements
         (consume tokens n :right_brace "Expect '}' after block.")]
        (let [[statement n] (declaration tokens n)]
          (if statement
            (recur (conj statements statement) n)
            (recur statements n)))))))

(defn- if-statement [tokens n]
  (let [[condition n] (expression tokens (consume tokens n :left_paren "Expect '(' after 'if'."))
        [then n] (statement tokens (consume tokens n :right_paren "Expect ')' after if condition."))]
    (if (= :else (:type (current tokens n)))
      (let [[else n] (statement tokens (inc n))]
        [(If. condition then else) n])
      [(If. condition then nil) n])))

(defn- while-statement [tokens n]
  (let [[condition n] (expression tokens (consume tokens n :left_paren "Expect '(' after 'while'."))
        [body n] (statement tokens (consume tokens n :right_paren "Expect ')' after while condition."))]
    [(While. condition body) n]))

(defn- for-statement [tokens n]
  (let [n (consume tokens n :left_paren "Expect '(' after 'for'.")
        [initializer n] (case (:type (current tokens n))
                          :semicolon [nil (inc n)]
                          :var (var-declaration tokens (inc n))
                          (expression-statement tokens (inc n)))
        [condition n] (if (not= :semicolon (:type (current tokens n)))
                        (expression tokens n)
                        [(Literal. true) n])
        n (consume tokens n :semicolon "Expect ';' after loop condition.")
        [increment n] (if (not= :right_paren (:type (current tokens n)))
                        (statement tokens n)
                        [nil n])
        [body n] (statement tokens (consume tokens n :right_paren "Expect ')' after for clauses."))
        body (While. condition
                     (if increment
                       (Block. [body increment])
                       body))]
    (if initializer
      [(Block. [initializer body]) n]
      [body n])))

(defn- break-statement [tokens n]
  [(Break. (current tokens n))
   (consume tokens (inc n) :semicolon "Expect ';' after break.")])

(defn- return-statement [tokens n]
  (let [token (current tokens n)
        n (inc n)]
    (if (= :semicolon (:type (current tokens n)))
      [(Return. token nil) (inc n)]
      (let [[value n] (statement tokens n)]
        [(Return. token value)
         (consume tokens n :semicolon "Expect ';' after loop condition.")]))))

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
  (let [[name n'] (consume-token tokens n :identifier "Expect variable name.")
        [value n'] (if (= :equal (:type (get tokens n')))
                     (expression tokens (inc n'))
                     [nil n'])]
    [(Var. name value)
     (consume tokens n' :semicolon "Expect ';' after value.")]))

(defn- fn-declaration [tokens n kind]
  (let [fun-token-n (dec n)
        [name n] (try (consume-token tokens n :identifier (format "Expected %s name." kind))
                      (catch ExceptionInfo e
                        (let [{:keys [tokens n]} (ex-data e)]
                          (if (= :left_paren (:type (get tokens n)))
                            [nil n]
                            (throw e)))))
        n (consume tokens n :left_paren (format "Expected '(' after %s name." kind))
        [args n] (if (not= :right_paren (:type (current tokens n)))
                   (loop [[expr n] (expression tokens n)
                          exprs []]
                     (if (= :comma (:type (current tokens n)))
                       (recur (expression tokens (inc n))
                              (conj exprs expr))
                       [(conj exprs expr) n]))
                   [[] n])
        n (consume tokens n :right_paren "Expect ')' after parameters.")
        n (consume tokens n :left_brace (format "Expect '{' before %s body." kind))
        [body n] (block tokens n)]
    (when (> (count args) 255)
      (parse-error "Can't have more than 255 arguments." {:tokens tokens :n fun-token-n}))
    [(Function. name args body) n]))

(defn- class-declaration [tokens n]
  (let [[name n] (consume-token tokens n :identifier "Expected class name.")
        [superclass n] (if (= :less (:type (current tokens n)))
                         (consume-token tokens (inc n) :identifier "Expect superclass name.")
                         [nil n])
        superclass (when superclass
                     (Variable. superclass))
        n (consume tokens n :left_brace "Expect '{' before class body.")
        [methods n] (loop [methods []
                           n n]
                      (let [token (current tokens n)]
                        (case (:type token)
                          (:eof :right_brace) [methods n]
                          (let [[method n] (fn-declaration tokens n "method")]
                            (recur (conj methods method) n)))))]
    [(LoxClassStatement. name superclass methods)
     (consume tokens n :right_brace "Expect '}' after class body.")]))

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

(ns cljloc.evaluator
  "Evaluate AST."
  (:require [cljloc.ast :as ast]
            [cljloc.protocols :refer [ICallable call IStringable tostring]]
            [cljloc.resolver :refer [*locals]]
            [cljloc.macros :refer [with-out-err]])
  (:import [cljloc.ast
            Binary Unary Grouping Print Var Variable Assign Literal
            Block If Logical While Break Call LoxCallable Function
            Return]
           [clojure.lang ExceptionInfo]))

(def globals {"clock" (LoxCallable. 0 (fn [] (/ (System/currentTimeMillis) 1000.0)))})
(def *global-env (atom {:values globals :enclosing nil}))

(defn- make-env [parent]
  (atom {:enclosing parent
         :values {}}))

(defn- runtime-error
  ([msg]
   (runtime-error msg {}))
  ([msg data]
   {:pre [(map? data)]}
   (throw (ex-info msg (assoc data :type ::runtime-error)))))

(extend-type LoxCallable
  ICallable
  (call [{:keys [arity function]} arguments token env]
    (if (= arity (count arguments))
      (apply function arguments)
      (runtime-error
       (format "Expected %s arguments but got %s." arity (count arguments))
       {:token token}))))

(extend-protocol ICallable
  Object
  (call [self & rest] (runtime-error (format "Can only call functions and classes. Tried to call %s" (class self))))
  nil
  (call [self & rest] (runtime-error "Can only call functions and classes. Tried to call nil.")))

(defn- truth? [val]
  (if (some? val)
    (if (boolean? val) val
        true)
    false))

(defn- check-number-op!
  ([op val]
   (when-not (double? val)
     (runtime-error "Operand must be a number" {:token op})))
  ([op val1 val2]
   (when-not (and (double? val1) (double? val2))
     (runtime-error "Operands must be numbers" {:token op}))))

(defprotocol IInterpretable
  (evaluate [self env]))

(extend-type Literal
  IInterpretable
  (evaluate [{value :value} _env]
    value))

(extend-type Grouping
  IInterpretable
  (evaluate [{expr :expression} env]
    (evaluate expr env)))

(extend-type Unary
  IInterpretable
  (evaluate [{:keys [operator right]} env]
    (let [right (evaluate right env)]
      (case (:type operator)
        :minus (do (check-number-op! operator right)
                   (- right))
        :bang (not (truth? right))
        (runtime-error "Unsupported unary operator" {:token operator})))))

(extend-type Binary
  IInterpretable
  (evaluate [{:keys [left operator right]} env]
    (let [left (evaluate left env)
          right (evaluate right env)]
      (if (= :plus (:type operator))
        (let [op (cond (and (double? left) (double? right)) +
                       (and (string? left) (string? right)) str
                       :else (runtime-error "Operands must be two numbers or two strings."
                                            {:token operator :left left :right right}))]
          (op left right))
        (let [op (case (:type operator)
                   :minus -
                   :slash (if (zero? right)
                            (runtime-error "Division by zero" {:token operator})
                            /)
                   :star *
                   :less <
                   :less_equal <=
                   :greater >
                   :greater_equal >=
                   :bang_equal not=
                   :equal_equal =
                   (runtime-error "Unsupported operator" {:token operator}))]
          (check-number-op! operator left right)
          (op left right))))))

(extend-type Print
  IInterpretable
  (evaluate [{:keys [expression]} env]
    (println (tostring (evaluate expression env)))
    nil))

(extend-type Var
  IInterpretable
  (evaluate [{:keys [name initializer]} env]
    (swap! env
           assoc-in
           [:values (:lexeme name)]
           (when (some? initializer)
             (evaluate initializer env)))
    nil))

(defn- ancestor
  "Recursively walks environments upwards looking for a variable."
  [env distance]
  (loop [i 0 env env]
    (if (< i distance)
      (recur (inc i) (:enclosing @env))
      env)))

(defn- lookup-variable [{:keys [name] :as expr} env]
  (get-in @(if-let [distance (get @*locals expr)]
             (ancestor env distance)
             *global-env)
          [:values (:lexeme name)]))

(extend-type Variable
  IInterpretable
  (evaluate [expr env]
    (lookup-variable expr env)))

(extend-type Assign
  IInterpretable
  (evaluate [{:keys [name value] :as expr} env]
    (let [val (evaluate value env)
          env (if-let [distance (get @*locals expr)]
                (ancestor env distance)
                *global-env)]
      (if (contains? (:values @env) (:lexeme name))
        (swap! env assoc-in [:values (:lexeme name)] val)
        (runtime-error (format "Undefined variable '%s'." (:lexeme name)) {:token name}))
      nil)))

(extend-type Break
  IInterpretable
  (evaluate [{:keys [break]} env]
    (throw (ex-info "break" {:type :break :token break}))))

(extend-type Block
  IInterpretable
  (evaluate [{:keys [statements]} env]
    (let [env' (make-env env)]
      (reduce (fn [_ statement] (evaluate statement env')) nil statements))))

(extend-type If
  IInterpretable
  (evaluate [{:keys [condition then else]} env]
    (let [test (evaluate condition env)]
      (if (truth? test)
        (evaluate then env)
        (when else
          (evaluate else env))))))

(extend-type Logical
  IInterpretable
  (evaluate [{:keys [left operator right]} env]
    (let [left (evaluate left env)]
      (case (:type operator)
        :or (if (truth? left)
              left
              (evaluate right env))
        :and (if (not (truth? left))
               left
               (evaluate right env))
        (runtime-error "Unsupported logical operator" {:token operator})))))

(extend-type While
  IInterpretable
  (evaluate [{:keys [condition body]} env]
    (try
      (while (truth? (evaluate condition env))
        (evaluate body env))
      (catch ExceptionInfo e
        (if (= :break (:type (ex-data e)))
          nil
          (throw e))))))

(extend-type Call
  IInterpretable
  (evaluate [{:keys [callee arguments]} env]
    (let [function (evaluate callee env)
          args (map #(evaluate % env) arguments)]
      (call function args (:name callee) env))))

(extend-type Return
  IInterpretable
  (evaluate [{:keys [keyword value] :as self} env]
    (throw (ex-info "return" {:type :return,
                              :value (when value (evaluate value env))
                              :token keyword}))))

(defrecord LoxFunction [^Function declaration, closure]
  ICallable
  (call [{{:keys [params body]} :declaration} args _ _]
    (try
      (let [env (make-env closure)]
        (doseq [[arg val] (map vector params args)]
          (swap! env assoc-in [:values (:lexeme (:name arg))] val))
        (evaluate body env))
      (catch ExceptionInfo e
        (let [data (ex-data e)]
          (if (= :return (:type data))
            (:value data)
            (throw e))))))
  IStringable
  (tostring [self]
    (format "#<function: %s>" (or (->> self :declaration :name :lexeme) "anonymous"))))

(extend-type Function
  IInterpretable
  (evaluate [{:keys [name] :as self} env]
    (let [f (LoxFunction. self env)]
      (when name
        (swap! env assoc-in [:values (:lexeme name)] f))
      f)))

(defn interpret
  ([ast] (interpret ast "stdin"))
  ([ast file]
   (try
     (-> ast (evaluate *global-env) tostring)
     (catch ExceptionInfo e
       (with-out-err
         (let [data (ex-data e)]
           (case (:type data)
             ::runtime-error
             (let [{{[line col] :pos} :token} data]
               (println (format "%s [%s:%s] Runtime error: %s"
                                file line col (ex-message e))))
             :break
             (let [{{[line col] :pos} :token} data]
               (println (format "%s [%s:%s] Runtime error: break outside of a loop."
                                file line col)))
             :return
             (let [{{[line col] :pos} :token} data]
               (println (format "%s [%s:%s] Runtime error: return outside of a function."
                                file line col)))
             (throw (ex-info "Eval error" {} e)))))))))

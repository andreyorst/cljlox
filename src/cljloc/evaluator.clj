(ns cljloc.evaluator
  "Evaluate AST."
  (:require [clojure.tools.logging :as log]
            [clojure.string :as str])
  (:import [cljloc.ast Binary Unary Grouping Print Var Variable Assign Literal Block]
           [clojure.lang ExceptionInfo]))

(defn make-env
  ([] (make-env nil))
  ([parent]
   (atom {:enclosing parent
          :values {}})))

(defonce *global-env (make-env))

(defn- runtime-error
  ([msg]
   (runtime-error msg {}))
  ([msg data]
   {:pre [(map? data)]}
   (throw (ex-info msg (assoc data :type ::runtime-error)))))

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

(defn- tostring [obj]
  (cond (double? obj) (-> obj str (str/replace #"\.0$" ""))
        (nil? obj) "nil"
        :else (str obj)))

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

(defn set-variable [env var value]
  (assoc-in env [:values var] value))

(extend-type Var
  IInterpretable
  (evaluate [{:keys [name initializer]} env]
    (swap! env
           set-variable
           (:lexeme name)
           (when (some? initializer)
             (evaluate initializer env)))
    nil))

(defn get-variable [env var]
  (let [env @env
        values (:values env)
        name (:lexeme var)]
    (if (contains? values name)
      (get values name)
      (if-some [enclosing (:enclosing env)]
        (recur enclosing var)
        (runtime-error (format "Undefined variable '%s'." name) {:token var})))))

(extend-type Variable
  IInterpretable
  (evaluate [{:keys [name]} env]
    (get-variable env name)))

(extend-type Assign
  IInterpretable
  (evaluate [{:keys [name value]} env]
    (let [val (evaluate value env)]
      (if (contains? @env (:lexeme name))
        (swap! env assoc (:lexeme name) val)
        (runtime-error (format "Undefined variable '%s'." (:lexeme name)) {:token name})))
    val))

(extend-type Block
  IInterpretable
  (evaluate [{:keys [statements]} env]
    (let [env' (make-env env)]
      (doseq [statement statements]
        (evaluate statement env')))))

(defn interpret
  ([ast] (interpret ast "stdin"))
  ([ast file]
   (try
     (-> ast (evaluate *global-env) tostring println)
     (catch ExceptionInfo e
       (let [data (ex-data e)]
         (if (= ::runtime-error (:type data))
           (let [{{[line col] :pos} :token} data]
             (binding [*out* *err*]
               (println (format "%s [%s:%s] Runtime error: %s\n"
                                file line col (ex-message e)))))
           (throw (ex-info "Eval error" {} e))))))))

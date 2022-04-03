(ns cljloc.evaluator
  "Evaluate AST."
  (:require [clojure.tools.logging :as log]
            [clojure.string :as str])
  (:import [cljloc.ast Binary Unary Grouping Print Var Variable Assign Literal]
           [clojure.lang ExceptionInfo]))

(defonce *env (atom {}))

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
  (evaluate [self]))

(extend-type Literal
  IInterpretable
  (evaluate [{value :value}]
    value))

(extend-type Grouping
  IInterpretable
  (evaluate [{expr :expression}]
    (evaluate expr)))

(extend-type Unary
  IInterpretable
  (evaluate [{:keys [operator right]}]
    (let [right (evaluate right)]
      (case (:type operator)
        :minus (do (check-number-op! operator right)
                   (- right))
        :bang (not (truth? right))
        (runtime-error "Unsupported unary operator" {:token operator})))))

(extend-type Binary
  IInterpretable
  (evaluate [{:keys [left operator right]}]
    (let [left (evaluate left)
          right (evaluate right)]
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
  (evaluate [{:keys [expression]}]
    (println (tostring (evaluate expression)))
    nil))

(extend-type Var
  IInterpretable
  (evaluate [{:keys [name initializer]}]
    (swap! *env
           assoc
           (:lexeme name)
           (when (some? initializer)
             (evaluate initializer)))
    nil))

(extend-type Variable
  IInterpretable
  (evaluate [{:keys [name]}]
    (let [val (get @*env (:lexeme name) ::not-found)]
      (if (= val ::not-found)
        (runtime-error (format "Undefined variable '%s'." (:lexeme name)) {:token name})
        val))))

(extend-type Assign
  IInterpretable
  (evaluate [{:keys [name value]}]
    (let [val (evaluate value)]
      (if (contains? @*env (:lexeme name))
        (swap! *env assoc (:lexeme name) val)
        (runtime-error (format "Undefined variable '%s'." (:lexeme name)) {:token name})))
    val))

(defn interpret
  ([ast] (interpret ast "stdin"))
  ([ast file]
   (try
     (-> ast evaluate tostring println)
     (catch ExceptionInfo e
       (let [data (ex-data e)]
         (if (= ::runtime-error (:type data))
           (let [{{[line col] :pos} :token} data]
             (binding [*out* *err*]
               (println (format "%s [%s:%s] Runtime error: %s\n"
                                file line col (ex-message e)))))
           (throw (ex-info "Eval error" {} e))))))))

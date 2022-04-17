(ns cljloc.evaluator
  "Evaluate AST."
  (:require [cljloc.ast :as ast]
            [cljloc.tokenizer]
            [cljloc.protocols :refer [ICallable call IStringable tostring]]
            [cljloc.macros :refer [with-out-err]])
  (:import [cljloc.tokenizer Token]
           [cljloc.ast
            Binary Unary Grouping Print Var Variable Assign Literal
            Block If Logical While Break Call LoxCallable Function
            Return LoxClassStatement Get Set This Super]
           [clojure.lang ExceptionInfo]))

(def globals {"clock" (LoxCallable. 0 (fn [] (/ (System/currentTimeMillis) 1000.0)))})
(def *global-env (atom {:values globals :enclosing nil :global true}))

(defn- make-env [parent]
  (atom {:enclosing parent
         :values {}}))

(defn- env-def! [env var val]
  (doto env
    (swap! assoc-in
           [:values (if (instance? Token var)
                      (:lexeme var)
                      var)]
           val)))

(defn- env-get [env var]
  (get-in @env [:values (if (instance? Token var) (:lexeme var) var)]))

(defn- env-contains? [env var]
  (contains? (:values @env) (if (instance? Token var) (:lexeme var) var)))

(defn- runtime-error
  ([msg]
   (runtime-error msg {}))
  ([msg data]
   {:pre [(map? data)]}
   (throw (ex-info msg (assoc data :type ::runtime-error)))))

(extend-type LoxCallable
  ICallable
  (call [{:keys [arity function]} arguments token _ _]
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
  (evaluate [self env locals]))

(extend-type Literal
  IInterpretable
  (evaluate [{value :value} _ _]
    value))

(extend-type Grouping
  IInterpretable
  (evaluate [{expr :expression} env locals]
    (evaluate expr env locals)))

(extend-type Unary
  IInterpretable
  (evaluate [{:keys [operator right]} env locals]
    (let [right (evaluate right env locals)]
      (case (:type operator)
        :minus (do (check-number-op! operator right)
                   (- right))
        :bang (not (truth? right))
        (runtime-error "Unsupported unary operator" {:token operator})))))

(extend-type Binary
  IInterpretable
  (evaluate [{:keys [left operator right]} env locals]
    (let [left (evaluate left env locals)
          right (evaluate right env locals)]
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
  (evaluate [{:keys [expression]} env locals]
    (println (tostring (evaluate expression env locals)))
    nil))

(extend-type Var
  IInterpretable
  (evaluate [{:keys [name initializer]} env locals]
    (env-def! env name (when (some? initializer)
                         (evaluate initializer env locals)))
    nil))

(defn- ancestor
  "Recursively walks environments upwards looking for a variable."
  [env distance]
  (loop [i 0 env env]
    (if (< i distance)
      (recur (inc i) (:enclosing @env))
      env)))

(defn- env-get-at [env distance name]
  (let [env (ancestor env distance)]
    (env-get env name)))

(defn- lookup-variable [expr name env locals]
  (env-get (if-let [distance (get locals expr)]
             (ancestor env distance)
             *global-env)
           name))

(extend-type Variable
  IInterpretable
  (evaluate [expr env locals]
    (lookup-variable expr (:name expr) env locals)))

(extend-type Assign
  IInterpretable
  (evaluate [{:keys [name value] :as expr} env locals]
    (let [val (evaluate value env locals)
          env (if-let [distance (get locals expr)]
                (ancestor env distance)
                *global-env)]
      (if (env-contains? env name)
        (env-def! env name val)
        (runtime-error (format "Undefined variable '%s'." (:lexeme name)) {:token name}))
      nil)))

(extend-type Break
  IInterpretable
  (evaluate [{:keys [break]} env _]
    (throw (ex-info "break" {:type :break :token break}))))

(extend-type Block
  IInterpretable
  (evaluate [{:keys [statements]} env locals]
    (let [env' (make-env env)]
      (reduce (fn [_ statement] (evaluate statement env' locals)) nil statements))))

(extend-type If
  IInterpretable
  (evaluate [{:keys [condition then else]} env locals]
    (let [test (evaluate condition env locals)]
      (if (truth? test)
        (evaluate then env locals)
        (when else
          (evaluate else env locals))))))

(extend-type Logical
  IInterpretable
  (evaluate [{:keys [left operator right]} env locals]
    (let [left (evaluate left env locals)]
      (case (:type operator)
        :or (if (truth? left)
              left
              (evaluate right env locals))
        :and (if (not (truth? left))
               left
               (evaluate right env locals))
        (runtime-error "Unsupported logical operator" {:token operator})))))

(extend-type While
  IInterpretable
  (evaluate [{:keys [condition body]} env locals]
    (try
      (while (truth? (evaluate condition env locals))
        (evaluate body env locals))
      (catch ExceptionInfo e
        (if (= :break (:type (ex-data e)))
          nil
          (throw e))))))

(extend-type Call
  IInterpretable
  (evaluate [{:keys [callee arguments]} env locals]
    (let [function (evaluate callee env locals)
          args (map #(evaluate % env locals) arguments)]
      (call function args (:name callee) env locals))))

(extend-type Return
  IInterpretable
  (evaluate [{:keys [keyword value] :as self} env locals]
    (throw (ex-info "return" {:type :return,
                              :value (when value (evaluate value env locals))
                              :token keyword}))))

(defprotocol IBind
  (bind [self, instance, env]))

(defn- evaluate-exprs [exprs env locals]
  (reduce (fn [_ expr] (evaluate expr env locals)) nil exprs))

(defrecord LoxFunction [^Function declaration, arity, closure, initializer?]
  ICallable
  (call [{{:keys [name params body]} :declaration} args _ _ locals]
    (when (not= arity (count args))
      (runtime-error
       (format "Expected %s arguments but got %s." arity (count args))
       {:token name}))
    (try
      (let [env (make-env closure)]
        (doseq [[arg val] (map vector params args)]
          (env-def! env (:name arg) val))
        (let [res (evaluate-exprs body env locals)]
          (if initializer?
            (env-get closure "this")
            res)))
      (catch ExceptionInfo e
        (let [data (ex-data e)]
          (if (= :return (:type data))
            (if initializer?
              (env-get closure "this")
              (:value data))
            (throw e))))))
  IBind
  (bind [self instance env]
    (let [closure (make-env env)]
      (env-def! closure "this" instance)
      (LoxFunction. (:declaration self) (:arity self) closure initializer?)))
  IStringable
  (tostring [_]
    (format "#<function: %s>" (or (->> declaration :name :lexeme) "anonymous"))))

(extend-type Function
  IInterpretable
  (evaluate [{:keys [name params] :as self} env locals]
    (let [f (LoxFunction. self (count params) env false)]
      (when name
        (env-def! env name f))
      f)))

(extend-type This
  IInterpretable
  (evaluate [self env locals]
    (lookup-variable self (:keyword self) env locals)))

(defn- get-class-method [{:keys [methods] :as self} name]
  (cond (contains? methods (:lexeme name))
        (let [method (get methods (:lexeme name))]
          (bind method self (:closure method)))
        (and (:superclass (:class self))
             (contains? (:methods (:superclass (:class self))) (:lexeme name)))
        (let [method (get (:methods (:superclass (:class self))) (:lexeme name))]
          (bind method self (:closure method)))
        :else
        (runtime-error (format "Undefined property '%s'." (:lexeme name)) {:token name})))

(defn- get-prop [{:keys [fields methods] :as self} name]
  (let [fields @fields]
    (if (contains? fields (:lexeme name))
      (get fields (:lexeme name))
      (get-class-method self name))))

(defrecord LoxInstance [class fields methods]
  IStringable
  (tostring [_]
    (format "#<instance: %s>" (:name class))))

(extend-type Get
  IInterpretable
  (evaluate [{:keys [object name]} env locals]
    (let [obj (evaluate object env locals)]
      (when-not (instance? LoxInstance obj)
        (runtime-error "Only instances have properties." {:token name}))
      (get-prop obj name))))

(extend-type Set
  IInterpretable
  (evaluate [{:keys [object name val]} env locals]
    (let [obj (evaluate object env locals)]
      (when-not (instance? LoxInstance obj)
        (runtime-error "Only instances have fields." {:token name}))
      (swap! (:fields obj) assoc (:lexeme name) (evaluate val env locals)))))

(defrecord LoxClass [^String name, superclass, arity, methods]
  IStringable
  (tostring [_]
    (format "#<class: %s>" name))
  ICallable
  (call [self args _ env locals]
    (let [c (LoxInstance. self (atom {}) methods)]
      (when (contains? methods "init")
        (call (bind (get methods "init") c env) args nil env locals))
      c)))

(extend-type LoxClassStatement
  IInterpretable
  (evaluate [{:keys [name superclass methods]} env locals]
    (let [superclass (when superclass
                       (let [sc (evaluate superclass env locals)]
                         (when-not (instance? LoxClass sc)
                           (runtime-error "Superclass must be a class." {:token (:name superclass)}))
                         sc))
          _ (env-def! env name nil)
          env (if superclass
                (env-def! (make-env env) "super" superclass)
                env)
          methods (reduce (fn [methods method]
                            (assoc methods (:lexeme (:name method))
                                   (LoxFunction. method (count (:params method)) env
                                                 (= "init" (:lexeme (:name method))))))
                          {} methods)
          c (LoxClass. (:lexeme name) superclass 0 methods)
          env (if superclass
                (:enclosing @env)
                env)]
      (env-def! env name c)
      c)))

(extend-type Super
  IInterpretable
  (evaluate [{:keys [keyword method] :as expr} env locals]
    (let [distance (get locals expr)
          superclass (env-get-at env distance "super")
          obj (env-get-at env (dec distance) "this")
          method (get-class-method superclass method)]
      (bind method obj env))))

(defn interpret
  ([ast locals] (interpret ast locals "stdin"))
  ([ast locals file]
   (try
     (-> ast (evaluate *global-env locals) tostring)
     (catch ExceptionInfo e
       (with-out-err
         (let [data (ex-data e)]
           (case (:type data)
             ::runtime-error
             (let [{{[line col] :pos} :token} data]
               (println (format "%s [%s:%s] Runtime error: %s"
                                file line col (ex-message e))))
             (throw (ex-info "Eval error" {} e)))))))))

(ns cljlox.resolver
  "Resolver for Lox AST.

  Resolves each variable definition and usage with it's corresponding
  scope.  Stores the scope number in the `locals` which is returned as
  an artifacto of calling the resolver."
  (:require [cljlox.ast :as ast]
            [cljlox.protocols :refer [Resolver lox-resolve]])
  (:import [cljlox.ast
            Expression Binary Unary Grouping Print Var Variable Assign Literal
            Block If Logical While Break Call Function Return
            LoxClassStatement Get Set This Super]))

;; TODO: Extend the resolver to associate a unique index for each local
;;       variable declared in a scope. When resolving a variable access,
;;       look up both the scope the variable is in and its index and
;;       store that. In the interpreter, use that to quickly access a
;;       variable by its index instead of using a map.

(defn- resolve-error
  ([msg]
   (resolve-error msg {}))
  ([msg data]
   {:pre [(map? data)]}
   (throw (ex-info msg (assoc data :type ::error)))))

(defn- begin-scope [[scope-stack locals]]
  [(conj scope-stack {}) locals])

(defn- end-scope [[scope-stack locals]]
  (doseq [[var status] (peek scope-stack)]
    (when (and (not= status :used)
               (not (re-find #"^_" var))
               (not (= "this" var))
               (not (= "super" var)))
      (resolve-error (format "Unused local variable: '%s'. Start variable name with underscore if variable is unused." var))))
  [(pop scope-stack) locals])

(defn- scope-get [scope-stack token]
  (get-in scope-stack [(dec (count scope-stack)) (:lexeme token token)]))

(defn- scope-set [scope-stack token val]
  (assoc-in scope-stack [(dec (count scope-stack)) (:lexeme token token)] val))

(defn- resolve-local [expr name [scope-stack locals]]
  (if (seq scope-stack)
    (loop [i (dec (count scope-stack))]
      (if (>= i 0)
        (if (contains? (get scope-stack i) (:lexeme name))
          [(assoc-in scope-stack [i (:lexeme name)] :used)
           (assoc locals expr (- (count scope-stack) 1 i))]
          (recur (dec i)))
        [scope-stack locals]))
    [scope-stack locals]))

(defn- declare-var [name [scope-stack locals]]
  (when (scope-get scope-stack name)
    (resolve-error "Already a variable with this name in this scope." {:token name}))
  [(if (seq scope-stack)
     (scope-set scope-stack name false)
     scope-stack)
   locals])

(defn- define-var [name [scope-stack locals]]
  [(if (seq scope-stack)
     (scope-set scope-stack name true)
     scope-stack)
   locals])

(defn- resolve-exprs [expressions stack]
  (reduce (fn [stack expr]
            (lox-resolve expr stack))
          stack
          expressions))

(defn- resolve-function
  ([expr stack] (resolve-function expr :function stack))
  ([{:keys [params body]} type [scope-stack locals]]
   (let [enclosing-function (:function locals)
         stack [scope-stack (assoc locals :function type)]
         [scope-stack locals]
         (->> params
              (reduce (fn [stack param]
                        (->> stack
                             (declare-var (:name param))
                             (define-var (:name param))))
                      (begin-scope stack))
              (resolve-exprs body)
              end-scope)]
     [scope-stack (assoc locals :function enclosing-function)])))

(extend-type Block
  Resolver
  (lox-resolve [{:keys [statements]} stack]
    (-> statements
        (resolve-exprs (begin-scope stack))
        end-scope)))

(extend-type Var
  Resolver
  (lox-resolve [{:keys [name initializer]} stack]
    (cond->> (declare-var name stack)
      (some? initializer) (lox-resolve initializer)
      true (define-var name))))

(extend-type Variable
  Resolver
  (lox-resolve [{:keys [name] :as expr} [scope-stack :as stack]]
    (when (and (seq scope-stack)
               (false? (scope-get scope-stack name)))
      (resolve-error "Can't read local variable in its own initializer." {:token name}))
    (resolve-local expr name stack)))

(extend-type Assign
  Resolver
  (lox-resolve [{:keys [name value] :as expr} stack]
    (resolve-local expr name (lox-resolve value stack))))

(extend-type Function
  Resolver
  (lox-resolve [{:keys [params name body] :as expr} stack]
    (cond->> stack
      (some? name) (declare-var name)
      (some? name) (define-var name)
      true (resolve-function expr))))

(extend-type If
  Resolver
  (lox-resolve [{:keys [condition then else]} stack]
    (cond->> stack
      true (lox-resolve condition)
      true (lox-resolve then)
      (some? else) (lox-resolve else))))

(extend-type Print
  Resolver
  (lox-resolve [{:keys [expression]} stack]
    (lox-resolve expression stack)))

(extend-type Return
  Resolver
  (lox-resolve [{:keys [keyword value]} [_ locals :as stack]]
    (if (:function locals)
      (if value
        (if (= :initializer (:function locals))
          (resolve-error "Can't return a value from an initializer." {:token keyword})
          (lox-resolve value stack))
        stack)
      (resolve-error "Can't return from top-level code." {:token keyword}))))

(extend-type While
  Resolver
  (lox-resolve [{:keys [condition body]} [scope-stack locals] ]
    (let [enclosing-loop (:loop locals)
          [scope-stack locals]
          (->> [scope-stack (assoc locals :loop true)]
               (lox-resolve condition)
               (lox-resolve body))]
      [scope-stack (assoc locals :loop enclosing-loop)])))

(extend-type Binary
  Resolver
  (lox-resolve [{:keys [left right]} stack]
    (->> stack
         (lox-resolve left)
         (lox-resolve right))))

(extend-type Call
  Resolver
  (lox-resolve [{:keys [callee arguments]} stack]
    (reduce (fn [stack arg] (lox-resolve arg stack))
            (lox-resolve callee stack) arguments)))

(extend-type Grouping
  Resolver
  (lox-resolve [{:keys [expression]} stack]
    (lox-resolve expression stack)))

(extend-type Literal
  Resolver
  (lox-resolve [_ stack]
    stack))

(extend-type Break
  Resolver
  (lox-resolve [{:keys [break]} [scope-stack locals] ]
    (if (:loop locals)
      [scope-stack locals]
      (resolve-error "break outside of a loop." {:token break}))))

(extend-type Logical
  Resolver
  (lox-resolve [{:keys [left right]} stack ]
    (->> stack
         (lox-resolve left)
         (lox-resolve right))))

(extend-type Unary
  Resolver
  (lox-resolve [{:keys [right]} stack ]
    (lox-resolve right stack)))

(extend-type LoxClassStatement ;; ok
  Resolver
  (lox-resolve [{:keys [name superclass methods]} [scope-stack locals] ]
    (when (and superclass (= (:lexeme (:name superclass)) (:lexeme name)))
      (resolve-error "A class can't inherit from itself." {:token (:name superclass)}))
    (let [enclosing-class (:class locals)
          stack (->> [scope-stack (assoc locals :class true)]
                     (declare-var name)
                     (define-var name))
          stack (if superclass
                  (let [[scope-stack locals] stack]
                    (lox-resolve superclass [scope-stack (assoc locals :class :subclass)]))
                  stack)
          stack (if superclass
                  (let [[scope-stack locals] (begin-scope stack)]
                    [(scope-set scope-stack "super" true) locals])
                  stack)
          stack (let [[scope-stack locals] (begin-scope stack)]
                  [(scope-set scope-stack "this" true) locals])
          stack (->> methods
                     (reduce (fn [stack method]
                               (resolve-function
                                method
                                (if (= "init" (:lexeme (:name method))) :initializer :method)
                                stack))
                             stack)
                     end-scope)
          [scope-stack locals] (if superclass (end-scope stack) stack)]
      [scope-stack (assoc locals :class enclosing-class)])))

(extend-type Get
  Resolver
  (lox-resolve [{:keys [object]} stack ]
    (lox-resolve object stack)))

(extend-type Set
  Resolver
  (lox-resolve [{:keys [object val]} stack ]
    (->> stack
         (lox-resolve val)
         (lox-resolve object))))

(extend-type This
  Resolver
  (lox-resolve [{:keys [keyword] :as expr} [_ locals :as stack]]
    (if (:class locals)
      (resolve-local expr keyword stack)
      (resolve-error "Can't use 'this' outside of a class." {:token keyword}))))

(extend-type Super
  Resolver
  (lox-resolve [{:keys [keyword] :as expr} [_ locals :as stack]]
    (when-not (:class locals)
      (resolve-error "Can't use 'super' outside of a class." {:token keyword}))
    (when-not (= :subclass (:class locals))
      (resolve-error  "Can't use 'super' in a class with no superclass." {:token keyword}))
    (resolve-local expr keyword stack)))

(extend-type Expression
  Resolver
  (lox-resolve [{:keys [expression]} stack]
    (lox-resolve expression stack)))

(defn resolve-expr [expr]
  (let [[_ locals] (lox-resolve expr [[] {}])]
    (dissoc locals :loop :class :function)))

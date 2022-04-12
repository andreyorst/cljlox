(ns cljloc.resolver
  "Resolver for Lox AST.

  Resolves each variable definition and usage with it's corresponding scope.
  Stores the scope number in the `*locals` atom.
  This atom is later used by evaluator to refer to locals."
  (:require [cljloc.ast :as ast]
            [cljloc.protocols :refer [Resolver lox-resolve]])
  (:import [cljloc.ast
            Binary Unary Grouping Print Var Variable Assign Literal
            Block If Logical While Break Call Function Return]))

;; TODO: Extend the resolver to report an error if a local variable is never used.
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

(defn begin-scope [[scope-stack locals]]
  [(conj scope-stack {}) locals])

(defn end-scope [[scope-stack locals]]
  [(pop scope-stack) locals])

(defn scope-get [scope-stack token]
  (get-in scope-stack [(dec (count scope-stack)) (:lexeme token)]))

(defn scope-set [scope-stack token val]
  (assoc-in scope-stack [(dec (count scope-stack)) (:lexeme token)] val))

(extend-type Block
  Resolver
  (lox-resolve [{:keys [statements]} stack]
    (->> statements
         (reduce (fn [stack statement]
                   (lox-resolve statement stack))
                 (begin-scope stack))
         end-scope)))

(defn- declare-var [name [scope-stack locals]]
  (when (get-in scope-stack [(dec (count scope-stack)) (:lexeme name)])
    (resolve-error "Already a variable with this name in this scope." {:token name}))
  [(if (seq scope-stack)
     (scope-set scope-stack name false)
     scope-stack) locals])

(defn- define-var [name [scope-stack locals]]
  [(if (seq scope-stack)
     (scope-set scope-stack name true)
     scope-stack)
   locals])

(extend-type Var
  Resolver
  (lox-resolve [{:keys [name initializer]} stack]
    (cond->> (declare-var name stack)
      (some? initializer) (lox-resolve initializer)
      true (define-var name))))

(defn- resolve-local [expr name [scope-stack locals]]
  (when (seq scope-stack)
    (loop [i (dec (count scope-stack))]
      (if (contains? (get scope-stack i) (:lexeme name))
        [scope-stack (assoc locals expr (- (count scope-stack) 1 i))]
        (if (> i 0)
          (recur (dec i))
          [scope-stack locals])))))

(extend-type Variable
  Resolver
  (lox-resolve [{:keys [name] :as expr} [scope-stack locals]]
    (when (and (seq scope-stack)
               (false? (get-in scope-stack [(dec (count scope-stack)) (:lexeme name)])))
      (resolve-error "Can't read local variable in its own initializer." {:token name}))
    (resolve-local expr name [scope-stack locals])))

(extend-type Assign
  Resolver
  (lox-resolve [{:keys [name value] :as expr} stack]
    (resolve-local expr name (lox-resolve value stack))))

(defn- resolve-function [{:keys [params body]} stack]
  (->> params
       (reduce (fn [stack param]
                 (->> stack
                      (declare-var (:name param))
                      (define-var (:name param))))
               (begin-scope stack))
       (lox-resolve body)
       end-scope))

(extend-type Function
  Resolver
  (lox-resolve [{:keys [params name body] :as expr} stack]
    (->> stack
         (declare-var name)
         (define-var name)
         (resolve-function expr))))

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
  (lox-resolve [{:keys [value]} stack]
    (if value
      (lox-resolve value stack)
      stack)))

(extend-type While
  Resolver
  (lox-resolve [{:keys [condition body]} stack]
    (->> stack
         (lox-resolve condition)
         (lox-resolve body))))

(extend-type Binary
  Resolver
  (lox-resolve [{:keys [left right]} stack]
    (->> stack
         (lox-resolve left)
         (lox-resolve right))))

(extend-type Call
  Resolver
  (lox-resolve [{:keys [callee arguments]} [scope-stack locals]]
    (reduce (fn [[scope-stack locals] arg]
              (lox-resolve arg [scope-stack locals]))
            (lox-resolve callee [scope-stack locals])
            arguments)))

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
  (lox-resolve [_ stack]
    stack))

(extend-type Logical
  Resolver
  (lox-resolve [{:keys [left right]} stack]
    (->> stack
         (lox-resolve left)
         (lox-resolve right))))

(extend-type Unary
  Resolver
  (lox-resolve [{:keys [right]} stack]
    (lox-resolve right stack)))

(defn resolve-expr [expr]
  (let [[_ locals] (lox-resolve expr [[] {}])]
    locals))

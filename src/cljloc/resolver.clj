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

;; TODO: Think about removing state
(def *locals (atom {}))

(defn register-expr-scope [expr i]
  (swap! *locals assoc expr i))

(defn- resolve-error
  ([msg]
   (resolve-error msg {}))
  ([msg data]
   {:pre [(map? data)]}
   (throw (ex-info msg (assoc data :type ::error)))))

(defn begin-scope [scope-stack]
  (conj scope-stack {}))

(defn end-scope [scope-stack]
  (pop scope-stack))

(defn scope-get [scope-stack token]
  (get-in scope-stack [(dec (count scope-stack)) (:lexeme token)]))

(defn scope-set [scope-stack token val]
  (assoc-in scope-stack [(dec (count scope-stack)) (:lexeme token)] val))

(extend-type Block
  Resolver
  (lox-resolve [{:keys [statements]} scope-stack]
    (->> statements
         (reduce (fn [scope-stack statement]
                   (lox-resolve statement scope-stack))
                 (begin-scope scope-stack))
         end-scope)))

(defn- declare-var [name scope-stack]
  (when (get-in scope-stack [(dec (count scope-stack)) (:lexeme name)])
    (resolve-error "Already a variable with this name in this scope." {:token name}))
  (if (seq scope-stack)
    (scope-set scope-stack name false)
    scope-stack))

(defn- define-var [name scope-stack]
  (if (seq scope-stack)
    (scope-set scope-stack name true)
    scope-stack))

(extend-type Var
  Resolver
  (lox-resolve [{:keys [name initializer]} scope-stack]
    (cond->> (declare-var name scope-stack)
      (some? initializer) (lox-resolve initializer)
      true (define-var name))))

(defn- resolve-local [expr name scope-stack]
  (when (seq scope-stack)
    (loop [i (dec (count scope-stack))]
      (if (contains? (get scope-stack i) (:lexeme name))
        (register-expr-scope expr (- (count scope-stack) 1 i))
        (when (> i 0)
          (recur (dec i)))))))

(extend-type Variable
  Resolver
  (lox-resolve [{:keys [name] :as expr} scope-stack]
    (when (and (seq scope-stack)
               (false? (get-in scope-stack [(dec (count scope-stack)) (:lexeme name)])))
      (resolve-error "Can't read local variable in its own initializer." {:token name}))
    (resolve-local expr name scope-stack)
    scope-stack))

(extend-type Assign
  Resolver
  (lox-resolve [{:keys [name value] :as expr} scope-stack]
    (let [scope-stack (lox-resolve value scope-stack)]
      (resolve-local expr name scope-stack)
      scope-stack)))

(defn- resolve-function [{:keys [params body]} scope-stack]
  (->> params
       (reduce (fn [scope-stack param]
                 (->> scope-stack
                      (declare-var (:name param))
                      (define-var (:name param))))
               (begin-scope scope-stack))
       (lox-resolve body)
       end-scope))

(extend-type Function
  Resolver
  (lox-resolve [{:keys [params name body] :as expr} scope-stack]
    (->> scope-stack
         (declare-var name)
         (define-var name)
         (resolve-function expr))))

(extend-type If
  Resolver
  (lox-resolve [{:keys [condition then else]} scope-stack]
    (cond->> scope-stack
      true (lox-resolve condition)
      true (lox-resolve then)
      (some? else) (lox-resolve else))))

(extend-type Print
  Resolver
  (lox-resolve [{:keys [expression]} scope-stack]
    (lox-resolve expression scope-stack)))

(extend-type Return
  Resolver
  (lox-resolve [{:keys [value]} scope-stack]
    (if value
      (lox-resolve value scope-stack)
      scope-stack)))

(extend-type While
  Resolver
  (lox-resolve [{:keys [condition body]} scope-stack]
    (->> scope-stack
         (lox-resolve condition)
         (lox-resolve body))))

(extend-type Binary
  Resolver
  (lox-resolve [{:keys [left right]} scope-stack]
    (->> scope-stack
         (lox-resolve left)
         (lox-resolve right))))

(extend-type Call
  Resolver
  (lox-resolve [{:keys [callee arguments]} scope-stack]
    (reduce (fn [scope-stack arg]
              (lox-resolve arg scope-stack))
            (lox-resolve callee scope-stack)
            arguments)))

(extend-type Grouping
  Resolver
  (lox-resolve [{:keys [expression]} scope-stack]
    (lox-resolve expression scope-stack)))

(extend-type Literal
  Resolver
  (lox-resolve [_ scope-stack]
    scope-stack))

(extend-type Break
  Resolver
  (lox-resolve [_ scope-stack]
    scope-stack))

(extend-type Logical
  Resolver
  (lox-resolve [{:keys [left right]} scope-stack]
    (->> scope-stack
         (lox-resolve left)
         (lox-resolve right))))

(extend-type Unary
  Resolver
  (lox-resolve [{:keys [ right]} scope-stack]
    (lox-resolve right scope-stack)))

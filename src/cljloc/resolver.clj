(ns cljloc.resolver
  "Resolver for Lox AST.

  Resolves each variable definition and usage with it's corresponding scope.
  Stores the scope number in the `*locals` atom.
  This atom is later used by evaluator to refer to locals."
  (:require [cljloc.ast :as ast]
            [cljloc.protocols :refer [Resolver lox-resolve]])
  (:import [cljloc.ast
            Binary Unary Grouping Print Var Variable Assign Literal
            Block If Logical While Break Call Function Return
            LoxClassStatement Get Set This]))

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
  (doseq [[var status] (peek scope-stack)]
    (when (and (not= status :used)
               (not (re-find #"^_" var))
               (not (= "this" var)))
      (resolve-error (format "Unused local variable: '%s'. Start variable name with underscore if variable is unused." var))))
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
        [(assoc-in scope-stack [i (:lexeme name)] :used)
         (assoc locals expr (- (count scope-stack) 1 i))]
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
              (lox-resolve body)
              end-scope)]
     [scope-stack (assoc locals :function enclosing-function)])))

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
  (lox-resolve [{:keys [keyword value]} [scope-stack locals]]
    (if (:function locals)
      (if value
        (if (= :initializer (:function locals))
          (resolve-error "Can't return a value from an initializer." {:token keyword})
          (lox-resolve value [scope-stack locals]))
        [scope-stack locals])

      (resolve-error "Can't return from top-level code." {:token keyword}))))

(extend-type While
  Resolver
  (lox-resolve [{:keys [condition body]} [scope-stack locals]]
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
  (lox-resolve [{:keys [break]} [scope-stack locals]]
    (if (:loop locals)
      [scope-stack locals]
      (resolve-error "break outside of a loop." {:token break}))))

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

(extend-type LoxClassStatement
  Resolver
  (lox-resolve [{:keys [name methods]} [scope-stack locals]]
    (let [enclosing-class (:class locals)
          stack
          (->> [scope-stack (assoc locals :class true)]
               (declare-var name)
               (define-var name)
               begin-scope
               ((fn [[scope-stack locals]]
                  [(assoc-in scope-stack [(dec (count scope-stack)) "this"] true) locals])))
          [scope-stack locals]
          (->> methods
               (reduce (fn [stack method]
                         (resolve-function method (if (= "init" (:lexeme (:name method))) :initializer :method) stack))
                       stack)
               end-scope)]
      [scope-stack (assoc locals :class enclosing-class)])))

(extend-type Get
  Resolver
  (lox-resolve [{:keys [object name]} stack]
    (lox-resolve object stack)))

(extend-type Set
  Resolver
  (lox-resolve [{:keys [object val]} stack]
    (->> stack
         (lox-resolve val)
         (lox-resolve object))))

(extend-type This
  Resolver
  (lox-resolve [{:keys [keyword] :as expr} [scope-stack locals]]
    (if (:class locals)
      (resolve-local expr keyword [scope-stack locals])
      (resolve-error "Can't use 'this' outside of a class." {:token keyword}))))

(defn resolve-expr [expr]
  (let [[_ locals] (lox-resolve expr [[] {}])]
    locals))

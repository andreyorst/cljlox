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

(defn- resolve-error
  ([msg]
   (resolve-error msg {}))
  ([msg data]
   {:pre [(map? data)]}
   (throw (ex-info msg (assoc data :type ::error)))))

(defn- begin-scope [state]
  (update state :scopes conj {}))

(defn- end-scope [{:keys [scopes] :as state}]
  (doseq [[var status] (peek scopes)]
    (when (and (not= status :used)
               (not (re-find #"^_" var))
               (not (= "this" var))
               (not (= "super" var)))
      (resolve-error (format "Unused local variable: '%s'. Start variable name with underscore if variable is unused." var))))
  (update state :scopes pop))

(defn- scope-get [scopes token]
  (get-in scopes [(dec (count scopes)) (:lexeme token token)]))

(defn- scope-set [scopes token val]
  (assoc-in scopes [(dec (count scopes)) (:lexeme token token)] val))

(defn- resolve-local [expr name {:keys [scopes] :as state}]
  (if (seq scopes)
    (loop [i (dec (count scopes))]
      (if (>= i 0)
        (if (contains? (get scopes i) (:lexeme name))
          (-> state
              (update :scopes assoc-in [i (:lexeme name)] :used)
              (update :locals assoc expr (- (count scopes) 1 i)))
          (recur (dec i)))
        state))
    state))

(defn- declare-var [name {:keys [scopes] :as state}]
  (when (scope-get scopes name)
    (resolve-error "Already a variable with this name in this scope." {:token name}))
  (if (seq scopes)
    (update state :scopes scope-set name false)
    state))

(defn- define-var [name {:keys [scopes] :as state}]
  (if (seq scopes)
    (update state :scopes scope-set name true)
    state))

(defn- resolve-exprs [expressions state]
  (reduce (fn [state expr]
            (lox-resolve expr state))
          state
          expressions))

(defn- resolve-function
  ([expr state] (resolve-function expr :function state))
  ([{:keys [params body]} type state]
   (let [enclosing-function (:function state)
         state (->> params
                    (reduce (fn [state param]
                              (->> state
                                   (declare-var (:name param))
                                   (define-var (:name param))))
                            (-> state
                                (assoc :function type)
                                begin-scope))
                    (resolve-exprs body)
                    end-scope)]
     (assoc state :function enclosing-function))))

(extend-type Block
  Resolver
  (lox-resolve [{:keys [statements]} state]
    (-> statements
        (resolve-exprs (begin-scope state))
        end-scope)))

(extend-type Var
  Resolver
  (lox-resolve [{:keys [name initializer]} state]
    (cond->> (declare-var name state)
      (some? initializer) (lox-resolve initializer)
      true (define-var name))))

(extend-type Variable
  Resolver
  (lox-resolve [{:keys [name] :as expr} {:keys [scopes] :as state}]
    (when (and (seq scopes)
               (false? (scope-get scopes name)))
      (resolve-error "Can't read local variable in its own initializer." {:token name}))
    (resolve-local expr name state)))

(extend-type Assign
  Resolver
  (lox-resolve [{:keys [name value] :as expr} state]
    (resolve-local expr name (lox-resolve value state))))

(extend-type Function
  Resolver
  (lox-resolve [{:keys [params name body] :as expr} state]
    (cond->> state
      (some? name) (declare-var name)
      (some? name) (define-var name)
      true (resolve-function expr))))

(extend-type If
  Resolver
  (lox-resolve [{:keys [condition then else]} state]
    (cond->> state
      true (lox-resolve condition)
      true (lox-resolve then)
      (some? else) (lox-resolve else))))

(extend-type Print
  Resolver
  (lox-resolve [{:keys [expression]} state]
    (lox-resolve expression state)))

(extend-type Return
  Resolver
  (lox-resolve [{:keys [keyword value]} state]
    (if (:function state)
      (if value
        (if (= :initializer (:function state))
          (resolve-error "Can't return a value from an initializer." {:token keyword})
          (lox-resolve value state))
        state)
      (resolve-error "Can't return from top-level code." {:token keyword}))))

(extend-type While
  Resolver
  (lox-resolve [{:keys [condition body]} state]
    (let [enclosing-loop (:loop state)
          state (->> (assoc state :loop true)
                     (lox-resolve condition)
                     (lox-resolve body))]
      (assoc state :loop enclosing-loop))))

(extend-type Binary
  Resolver
  (lox-resolve [{:keys [left right]} state]
    (->> state
         (lox-resolve left)
         (lox-resolve right))))

(extend-type Call
  Resolver
  (lox-resolve [{:keys [callee arguments]} state]
    (reduce (fn [state arg] (lox-resolve arg state))
            (lox-resolve callee state) arguments)))

(extend-type Grouping
  Resolver
  (lox-resolve [{:keys [expression]} state]
    (lox-resolve expression state)))

(extend-type Literal
  Resolver
  (lox-resolve [_ state]
    state))

(extend-type Break
  Resolver
  (lox-resolve [{:keys [break]} state]
    (if (:loop state)
      state
      (resolve-error "break outside of a loop." {:token break}))))

(extend-type Logical
  Resolver
  (lox-resolve [{:keys [left right]} state]
    (->> state
         (lox-resolve left)
         (lox-resolve right))))

(extend-type Unary
  Resolver
  (lox-resolve [{:keys [right]} state]
    (lox-resolve right state)))

(extend-type LoxClassStatement
  Resolver
  (lox-resolve [{:keys [name superclass methods]} state]
    (when (and superclass (= (:lexeme (:name superclass)) (:lexeme name)))
      (resolve-error "A class can't inherit from itself." {:token (:name superclass)}))
    (let [enclosing-class (:class state)
          state (->> (assoc state :class true)
                     (declare-var name)
                     (define-var name))
          state (if superclass
                  (-> superclass
                      (lox-resolve (assoc state :class :subclass))
                      begin-scope
                      (update :scopes scope-set "super" true))
                  state)
          state (->> methods
                     (reduce (fn [state method]
                               (resolve-function
                                method
                                (if (= "init" (:lexeme (:name method))) :initializer :method)
                                state))
                             (update (begin-scope state) :scopes scope-set "this" true))
                     end-scope)]
      (-> (if superclass (end-scope state) state)
          (assoc :class enclosing-class)))))

(extend-type Get
  Resolver
  (lox-resolve [{:keys [object]} state]
    (lox-resolve object state)))

(extend-type Set
  Resolver
  (lox-resolve [{:keys [object val]} state]
    (->> state
         (lox-resolve val)
         (lox-resolve object))))

(extend-type This
  Resolver
  (lox-resolve [{:keys [keyword] :as expr} state]
    (if (:class state)
      (resolve-local expr keyword state)
      (resolve-error "Can't use 'this' outside of a class." {:token keyword}))))

(extend-type Super
  Resolver
  (lox-resolve [{:keys [keyword] :as expr} state]
    (when-not (:class state)
      (resolve-error "Can't use 'super' outside of a class." {:token keyword}))
    (when-not (= :subclass (:class state))
      (resolve-error  "Can't use 'super' in a class with no superclass." {:token keyword}))
    (resolve-local expr keyword state)))

(extend-type Expression
  Resolver
  (lox-resolve [{:keys [expression]} state]
    (lox-resolve expression state)))

(defn resolve-expr [expr]
  (->> {:scopes [] :locals {}}
       (lox-resolve expr)
       :locals))

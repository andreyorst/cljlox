(ns cljloc.evaluator
  (:import [cljloc.ast Unary Binary Literal Grouping]))

(defn runtime-error
  ([msg]
   (runtime-error msg {}))
  ([msg data]
   (throw (ex-info msg (assoc data :type :runtime-error)))))

(defn truth? [val]
  (if (some? val)
    (if (boolean? val) val
        true)
    false))

(defn check-number-op! [op val]
  (when-not (double? val)
    (runtime-error "Operand must be a number" {:operator op})))

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
        (runtime-error "Unsupported unary operator" {:operator operator})))))

(extend-type Binary
  IInterpretable
  (evaluate [{:keys [left operator right]}]
    (let [left (evaluate left)
          right (evaluate right)]
      ((case (:type operator)
         :minus -
         :slash /
         :star *
         :plus (cond (and (double? left) (double? right)) +
                     (and (string? left) (string? right)) str
                     :else (runtime-error "Operands must be of same type"
                                          {:operator operator :left left :right right}))

         :less <
         :less_equal <=
         :greater >
         :greater_equal >=
         :bang_equal not=
         :equal_equal =
         (runtime-error "Unsupported operator" {:operator operator}))
       left right))))

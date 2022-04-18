(ns cljlox.protocols
  (:require
   [clojure.string :as str]))

(defprotocol IStringable
  (tostring [self]))

(extend-protocol IStringable
  Object
  (tostring [self]
    (cond (double? self) (-> self str (str/replace #"\.0$" ""))
          (nil? self) "nil"
          :else (str self)))
  nil
  (tostring [_] "nil"))

(defprotocol ICallable
  (call [self arguments token env locals]))

(defprotocol Resolver
  (lox-resolve [this [scope-stack locals]]))

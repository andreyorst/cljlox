(ns cljloc.protocols)

(defprotocol IStringable
  (tostring [self]))

(extend-protocol IStringable
  Object
  (tostring [self] (str self))
  nil
  (tostring [self] "nil"))

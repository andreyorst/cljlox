(ns cljloc.macros)

(defmacro with-out-err [& body]
  `(binding [*out* *err*]
     ~@body))

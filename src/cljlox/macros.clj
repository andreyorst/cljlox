(ns cljlox.macros)

(defmacro with-out-err [& body]
  `(binding [*out* *err*]
     ~@body))

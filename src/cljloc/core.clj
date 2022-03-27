(ns cljloc.core
  (:require [cljloc.tokenizer :refer [tokenize]]))

(defn run [source]
  (let [{:keys [errors tokens]} (tokenize source)]
    ))

(defn run-file [file]
  ;; (run (String. (.getBytes (slurp file)) (Charset/defaultCharset)))
  (run (slurp file))
  )

(defn run-prompt []
  (print "> ")
  (flush)
  (when-some [line (read-line)]
    (run line)
    (recur)))

(defn -main [& args]
  (let [arglen (count args)]
    (cond (> arglen 1)
          (println "usage: cljlox [script]")
          (= arglen 1)
          (run-file (first args))
          :else
          (run-prompt))))

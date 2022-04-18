(ns cljlox.core
  (:gen-class)
  (:require
   [cljlox.runner :refer [run-file run-prompt]]))

(defn -main [& args]
  (try
    (let [arglen (count args)]
      (cond (> arglen 1)
            (println "usage: cljlox [script file]")
            (= arglen 1)
            (run-file (first args))
            :else
            (run-prompt)))
    (catch Exception e
      (println (format "Fatal error: %s" (ex-message e))))))

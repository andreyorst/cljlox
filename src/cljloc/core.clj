(ns cljloc.core
  (:require [cljloc.tokenizer :refer [tokenize]]
            [cljloc.parser :refer [parse]]
            [clojure.tools.logging :as log]))

(defn run
  ([source]
   (run source nil))
  ([source file]
   (let [{:keys [errors tokens]} (tokenize source)]
     (if (seq errors)
       (doseq [error errors]
         (if file
           (log/errorf "%s %s" file (str error))
           (log/errorf "%s" (str error))))
       (parse tokens)))))

(defn run-file [file]
  (run (slurp file) file))

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

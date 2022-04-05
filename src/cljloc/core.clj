(ns cljloc.core
  (:require [cljloc.tokenizer :refer [tokenize]]
            [cljloc.parser :refer [parse]]
            [cljloc.evaluator :refer [interpret]]))

(defn- run
  ([source]
   (run source nil))
  ([source file]
   (let [{:keys [errors tokens]} (tokenize source)]
     (if (seq errors)
       (let [fmt (if file (str file " %s") "%s")]
         (doseq [error errors]
           (binding [*out* *err*]
             (println (format fmt (str error))))))
       (doseq [ast (parse tokens)]
         (interpret ast))))))

(defn- run-file [file]
  (run (slurp file) file))

(defn- run-prompt []
  (println "Welcome to CljLoc.")
  (loop []
    (print "cljloc> ")
    (flush)
    (when-some [line (read-line)]
      (run line)
      (recur))))

(defn -main [& args]
  (let [arglen (count args)]
    (cond (> arglen 1)
          (println "usage: cljlox [script]")
          (= arglen 1)
          (run-file (first args))
          :else
          (run-prompt))))

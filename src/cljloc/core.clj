(ns cljloc.core
  (:require [cljloc.tokenizer :refer [tokenize]]
            [cljloc.parser :refer [parse]]
            [cljloc.evaluator :refer [interpret]]
            [cljloc.protocols :refer [lox-resolve]]
            [cljloc.resolver :as resolver]
            [cljloc.macros :refer [with-out-err]])
  (:import [clojure.lang ExceptionInfo])
  (:gen-class))

(defn run
  ([source]
   (run source nil))
  ([source file]
   (let [fmt (if file (str file " %s") "%s")
         {:keys [errors tokens]} (tokenize source)]
     (if (seq errors)
       (with-out-err
         (doseq [error errors]
           (println (format fmt (str error)))))
       (let [expressions (parse tokens)
             locals (reduce (fn [locals expr]
                              (merge locals (second (lox-resolve expr [[] {}]))))
                            {} expressions)]
         (reduce (fn [_ expr]
                   (when (seq expr)
                     (interpret expr locals)))
                 nil expressions))))))

(defn run-file [file]
  (println (run (slurp file) file)))

(defn run-prompt []
  (println "Welcome to CljLoc.")
  (loop []
    (print "cljloc> ")
    (flush)
    (when-some [line (read-line)]
      (println (run line))
      (recur))))

(defn -main [& args]
  (try
    (let [arglen (count args)]
      (cond (> arglen 1)
            (println "usage: cljlox [script]")
            (= arglen 1)
            (run-file (first args))
            :else
            (run-prompt)))
    (catch ExceptionInfo e
      (with-out-err
        (let [fmt (if (= (count args) 1)
                    (str (first args) " %s")
                    "%s")]
          (case (:type (ex-data e))
            ::resolver/error
            (let [[line col] (->> e ex-data :token :pos)]
              (println (format (format fmt "[%s %s] resolve error: %s")
                               line col
                               (ex-message e))))
            (println (format "Fatal error: %s" (ex-message e)))))))
    (catch Exception e
      (println (format "Fatal error: %s" (.getMessage e))))))

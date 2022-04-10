(ns cljloc.core
  (:require [cljloc.tokenizer :refer [tokenize]]
            [cljloc.parser :refer [parse]]
            [cljloc.evaluator :refer [interpret]]
            [cljloc.protocols :refer [lox-resolve]]
            [cljloc.resolver :as resolver :refer [*locals]]
            [cljloc.macros :refer [with-out-err]])
  (:import [clojure.lang ExceptionInfo])
  (:gen-class))

(defn- run
  ([source]
   (run source nil))
  ([source file]
   (let [fmt (if file (str file " %s") "%s")]
     (try
       (let [{:keys [errors tokens]} (tokenize source)]
         (if (seq errors)
           (doseq [error errors]
             (with-out-err (println (format fmt (str error)))))
           (let [expressions (parse tokens)]
             (reset! *locals {})
             (doseq [expr expressions]
               (lox-resolve expr []))
             (reduce (fn [_ expr]
                       (when (seq expr)
                         (interpret expr)))
                     nil expressions))))
       (catch ExceptionInfo e
         (case (:type (ex-data e))
           ::resolver/error
           (let [[line col] (->> e ex-data :token :pos)]
             (with-out-err
               (println (format (format fmt "[%s %s] resolve error: %s")
                                line col
                                (ex-message e)))))
           (throw e)))))))

(defn- run-file [file]
  (println (run (slurp file) file)))

(defn- run-prompt []
  (println "Welcome to CljLoc.")
  (loop []
    (print "cljloc> ")
    (flush)
    (when-some [line (read-line)]
      (println (run line))
      (recur))))

(defn -main [& args]
  (let [arglen (count args)]
    (cond (> arglen 1)
          (println "usage: cljlox [script]")
          (= arglen 1)
          (run-file (first args))
          :else
          (run-prompt))))

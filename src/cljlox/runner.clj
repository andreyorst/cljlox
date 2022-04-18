(ns cljlox.runner
  (:require
   [cljlox.evaluator :refer [interpret]]
   [cljlox.macros :refer [with-out-err]]
   [cljlox.parser :refer [parse]]
   [cljlox.resolver :as resolver :refer [resolve-expr]]
   [cljlox.tokenizer :refer [tokenize]])
  (:import
   (clojure.lang ExceptionInfo)))

(defn- log-resolver-error [fmt [line col] message]
  (println
   (format (format fmt "[%s %s] resolve error: %s")
           line col
           message)))

(defn- run
  ([source] (run source nil))
  ([source file]
   (let [fmt (if file (str file " %s") "%s")]
     (try
       (let [{:keys [errors tokens]} (tokenize source)]
         (if (seq errors)
           (with-out-err
             (doseq [error errors]
               (println (format fmt (str error)))))
           (let [expressions (parse tokens)
                 locals (reduce (fn [locals expr]
                                  (merge locals (resolve-expr expr)))
                                {} expressions)]
             (reduce (fn [_ expr]
                       (when (seq expr)
                         (interpret expr locals)))
                     nil expressions))))
       (catch ExceptionInfo e
         (with-out-err
           (case (:type (ex-data e))
             ::resolver/error
             (log-resolver-error fmt (-> e ex-data :token :pos) (ex-message e))
             (throw e))))))))

(defn run-source [source]
  (run source))

(defn run-file [file]
  (println (run (slurp file) file)))

(defn run-prompt []
  (println "Welcome to Cljlox.")
  (loop []
    (print "cljlox> ")
    (flush)
    (when-some [line (read-line)]
      (println (run line))
      (recur))))

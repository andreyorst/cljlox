(ns cljloc.core
  (:import [java.io BufferedReader IOException InputStreamReader]
           [java.nio.charset Charset]
           [java.nio.file Files Paths]
           [java.util List]))

(defn error
  ([pos msg]
   (error pos nil msg))
  ([line where msg]
   (->> (format "[line %d] Error%s: %s"
                line
                (if where (str " " where) "")
                msg)
        println)))

(defrecord Token [type lexeme literal line]
  Object
  (toString [_]
    (format "%s %s %s" type lexeme literal)))

(defn make-token
  ([type lexeme pos] (make-token type lexeme nil pos))
  ([type lexeme literal pos]
   (Token. type lexeme literal pos)))

(defn at-end? [current source]
  (>= current (count source)))

(def single-token-type
  {\( :left_paren
   \) :right_paren
   \{ :left_brace
   \} :right_brace
   \, :comma
   \. :dot
   \- :minus
   \+ :plus
   \; :semicolon
   \* :star
   \= :equal
   \< :less
   \> :greater
   \/ :slash
   \! :bang})

(def double-token-type
  {"==" :equal_equal
   "!=" :bang_equal
   "<=" :less_eqal
   ">=" :greater_equal
   "//" :comment})

(def keywords
  {"and"    :and
   "class"  :class
   "else"   :else
   "false"  :false
   "for"    :for
   "fun"    :fun
   "if"     :if
   "nil"    :nil
   "or"     :or
   "print"  :print
   "return" :return
   "super"  :super
   "this"   :this
   "true"   :true
   "var"    :var
   "while"  :while})

(defn digit? [c]
  (boolean (re-find #"[0-9]" (str c))))

(defn alpha? [c]
  (boolean (re-find #"[a-zA-Z_]" (str c))))

(defn alpha-numeric? [c]
  (or (alpha? c) (digit? c)))

(defn tokenize [source]
  (let [at-end? (complement (partial > (count source)))]
    (loop [current 0
           line 1
           tokens []
           error? false]
      (if (at-end? current)
        [tokens error?]
        (let [c (nth source current)
              current (inc current)]
          (case c
            (\( \) \{ \} \, \. \- \+ \; \*)
            (recur current line
                   (conj tokens (make-token (single-token-type c) (str c) line))
                   error?)
            (\= \! \< \>)
            (if-let [type (and (not (at-end? current))
                               (double-token-type (str c (nth source current))))]
              (let [lexem (str c (nth source current))
                    current (inc current)]
                (recur current line
                       (conj tokens (make-token type lexem line))
                       error?))
              (recur current line
                     (conj tokens (make-token (single-token-type c) (str c) line))
                     error?))
            \/ (if (and (not (at-end? current))
                        (double-token-type (str c (nth source current))))
                 (let [current (loop [current current]
                                 (if (or (at-end? current)
                                         (= (nth source current) \newline))
                                   (inc current)
                                   (recur (inc current))))]
                   (recur current (inc line) tokens error?))
                 (recur current line
                        (conj tokens (make-token (single-token-type c) (str c) line))
                        error?))
            (\space \return \tab)
            (recur current line tokens error?)
            \newline
            (recur current (inc line) tokens error?)
            \" (let [[ok? pos line]
                     (loop [pos current
                            line line]
                       (cond (at-end? pos)
                             (do (error line "Unterminated string.")
                                 [false pos line])
                             (= (nth source pos) \")
                             [true pos line]
                             :else
                             (recur (inc pos)
                                    (if (= (nth source pos) \newline) (inc line) line))))]
                 (if ok?
                   (recur (inc pos) line
                          (conj tokens (make-token :string (subs source current pos) line))
                          error?)
                   (recur (inc pos) line tokens true)))
            (cond (digit? c)
                  (let [pos (loop [pos current]
                              (if-let [c (and (not (at-end? pos))
                                              (nth source pos))]
                                (if (or (digit? c) (= \. c))
                                  (recur (inc pos))
                                  pos)
                                pos))]
                    (recur (inc pos) line
                           (conj tokens (make-token :number (Double/parseDouble (subs source (dec current) pos)) line))
                           error?))
                  (alpha? c)
                  (let [pos (loop [pos current]
                              (if-let [c (and (not (at-end? pos))
                                              (nth source pos))]
                                (if (alpha-numeric? c)
                                  (recur (inc pos))
                                  pos)
                                pos))
                        word (subs source (dec current) pos)]
                    (recur (inc pos) line
                           (conj tokens (make-token (keywords word :identifier) word line))
                           error?))
                  :else
                  (do (error line "Unexpected character.")
                      (recur current line tokens true)))))))))

(defn run [source]
  (let [tokens (tokenize source)]
    ))

(defn run-file [file]
  ;; (run (String. (.getBytes (slurp file)) (Charset/defaultCharset)))
  (run (slurp file))
  (when @had-error
    (System/exit 65)))

(defn run-prompt []
  (print "> ")
  (flush)
  (when-some [line (read-line)]
    (run line)
    (vreset! had-error false)
    (recur)))

(defn -main [& args]
  (let [arglen (count args)]
    (cond (> arglen 1)
          (println "usage: cljlox [script]")
          (= arglen 1)
          (run-file (first args))
          :else
          (run-prompt))))

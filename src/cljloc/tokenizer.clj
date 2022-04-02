(ns cljloc.tokenizer
  "Tokenizer."
  (:require [clojure.string :as str]
            [cljloc.protocols :refer [IStringable]]))

(defrecord TokenizerError [line col source message]
  Object
  (toString [_]
    (format "[line %d:%d] Error: %s\n%s\n%s" line col message
            source
            (str (str/join (repeat col " ")) "^"))))

(defn- compose-error
  [source [line col] msg]
  (TokenizerError. line col (first (drop (dec line) (str/split-lines source))) msg))

(defn- tokenizer-error [source _ current col line tokens errors]
  [current (inc col) line tokens
   (conj errors (compose-error source [line col] "Unexpected character."))])

(defrecord Token [type lexeme literal pos]
  Object
  (toString [_]
    (format "%s %s %s" type lexeme literal))
  IStringable
  (tostring [_]
    (str lexeme)))

(defn- make-token
  ([type lexeme pos]
   (make-token type lexeme nil pos))
  ([type lexeme literal pos]
   (Token. type lexeme literal pos)))

(defn- at-end? [current source]
  (>= current (count source)))

(def single-token-type
  "Single character tokens types."
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
  "Double character tokens types."
  {"==" :equal_equal
   "!=" :bang_equal
   "<=" :less_eqal
   ">=" :greater_equal
   "//" :comment})

(def keywords
  "Language keywords."
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

(defn- digit? [c]
  (boolean (re-find #"[0-9]" (str c))))

(defn- alpha? [c]
  (boolean (re-find #"[a-zA-Z_]" (str c))))

(defn- alpha-numeric? [c]
  (or (alpha? c) (digit? c)))

(defn- single-token [_ c current col line tokens errors]
  [current (inc col) line
   (conj tokens (make-token (single-token-type c) (str c) [line col]))
   errors])

(defn- double-token [source c current col line tokens errors]
  (if-let [type (and (not (at-end? current source))
                     (double-token-type (str c (nth source current))))]
    (let [lexem (str c (nth source current))
          current (inc current)]
      [current (+ 2 col) line
       (conj tokens (make-token type lexem [line col]))
       errors])
    [current (inc col) line
     (conj tokens (make-token (single-token-type c) (str c) [line col]))
     errors]))

(defn- comment-token [source c current col line tokens errors]
  (if (and (not (at-end? current source))
           (double-token-type (str c (nth source current))))
    (let [current (loop [current current]
                    (if (or (at-end? current source)
                            (= (nth source current) \newline))
                      (inc current)
                      (recur (inc current))))]
      [current 0 (inc line) tokens errors])
    [current (inc col) line
     (conj tokens (make-token (single-token-type c) (str c) [line col]))
     errors]))

(defn- space [_ c current col line tokens errors]
  (if (= c \newline)
    [current 0 (inc line) tokens errors]
    [current (inc col) line tokens errors]))

(defn- string-token [source _ current col line tokens errors]
  (let [[error pos col line]
        (loop [pos current
               col* (inc col)
               line* line
               escaped? false]
          (if-let [c (and (not (at-end? pos source)) (nth source pos))]
            (cond (or (= \\ c) (and (= \" c) escaped?))
                  (recur (inc pos) (inc col*) line* (= \\ c))
                  (= c \")
                  [nil pos col* line*]
                  :else
                  (if (= \newline c)
                    (recur (inc pos) 0 (inc line*) false)
                    (recur (inc pos) (inc col*) line* false)))
            [(compose-error source [line (dec col)] "Unterminated string.") pos col* line*]))]
    (if (some? error)
      [(inc pos) (+ col 2) line tokens (conj errors error)]
      [(inc pos) (+ col 2) line
       (conj tokens (make-token :string (subs source current pos) [line col]))
       errors])))

(defn- number-token [source _ current col line tokens errors]
  (let [[pos col]
        (loop [pos current
               col col]
          (if-let [c (and (not (at-end? pos source))
                          (nth source pos))]
            (if (or (digit? c) (= \. c))
              (recur (inc pos) (inc col))
              [pos col])
            [pos col]))]
    [pos col line
     (conj tokens (make-token :number (Double/parseDouble (subs source (dec current) pos)) [line col]))
     errors]))

(defn- identifier-token [source _ current col line tokens errors]
  (let [[pos col*]
        (loop [pos current
               col* col]
          (if-let [c (and (not (at-end? pos source))
                          (nth source pos))]
            (if (alpha-numeric? c)
              (recur (inc pos) (inc col*))
              [pos col*])
            [pos col*]))
        word (subs source (dec current) pos)]
    [pos (inc col*) line
     (conj tokens (make-token (keywords word :identifier) word [line col]))
     errors]))

(defn tokenize
  "Accepts source code as a string, and returns a hashmap with `errors`
  and `tokens`."
  ([source]
   (loop [[current col line tokens errors] [0 0 1 [] []]]
     (if (at-end? current source)
       {:errors errors
        :tokens tokens}
       (recur (tokenize source (nth source current)
                        (inc current) col line
                        tokens errors)))))
  ([source c current col line tokens errors]
   (let [tokenizer
         (case c
           (\( \) \{ \} \, \. \- \+ \; \*) single-token
           (\= \! \< \> \\) double-token
           \/ comment-token
           (\space \return \tab \newline) space
           \" string-token
           (cond (digit? c) number-token
                 (alpha? c) identifier-token
                 :else tokenizer-error))]
     (tokenizer source c current col line tokens errors))))

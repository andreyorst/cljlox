(ns cljloc.ast-test
  (:require [cljloc.ast :as ast]
            [cljloc.tokenizer]
            [cljloc.protocols :refer [tostring]]
            [clojure.test :refer [deftest is testing]])
  (:import [cljloc.tokenizer Token]))

(def var-a (Token. :variable "a" nil [0 0]))
(def one (Token. :literal "1" 1 [0 0]))
(def two (Token. :literal "2" 2 [0 0]))

(deftest ast-to-string-test
  (testing "common forms"
    (is (= "(- 1 2)"
           (tostring (ast/->Binary one "-" two))))
    (is (= "(- 2)"
           (tostring (ast/->Unary "-" two))))
    (is (= "(do 1)"
           (tostring (ast/->Grouping one))))
    (is (= "nil"
           (tostring (ast/->Literal nil))))
    (is (= "1"
           (tostring (ast/->Literal 1.0))))
    (is (= "1"
           (tostring (ast/->Literal 1))))
    (is (= "1.2"
           (tostring (ast/->Literal (Token. :literal "1.2" 1.2 [0 0])))))
    (is (= "1"
           (tostring (ast/->Literal (Token. :literal "1" 1.0 [0 0])))))
    (is (= "(< 1 2)"
           (tostring (ast/->Logical one "<" two))))
    (is (= "a"
           (tostring (ast/->Variable var-a))))
    (is (= "(set a 1)"
           (tostring (ast/->Assign var-a one))))
    (is (= "(print 1)"
           (tostring (ast/->Print one))))
    (is (= "(var a nil)"
           (tostring (ast/->Var var-a nil))))
    (is (= "(var a 1)"
           (tostring (ast/->Var var-a one))))
    (is (= "(do 1 2)"
           (tostring (ast/->Block [one two]))))
    (is (= "(if true 1)"
           (tostring (ast/->If (Token. :literal "true" true [0 0]) one nil))))
    (is (= "(if true 1 2)"
           (tostring (ast/->If (Token. :literal "true" true [0 0]) one two))))
    (is (= "(while true 1)"
           (tostring (ast/->While (Token. :literal "true" true [0 0]) one))))
    (is (= "(break)"
           (tostring (ast/->Break (Token. :break "break" nil [0 0])))))
    (is (= "#<native fn>"
           (tostring (ast/->LoxCallable nil nil))))
    (is (= "(fn a [a] a)"
           (tostring (ast/->Function var-a [var-a] var-a))))
    (is (= "(return)"
           (tostring (ast/->Return var-a nil))))
    (is (= "(return a)"
           (tostring (ast/->Return var-a var-a))))
    (is (= "(a)"
           (tostring (ast/->Call var-a nil []))))
    (is (= "(a a)"
           (tostring (ast/->Call var-a nil [var-a]))))))

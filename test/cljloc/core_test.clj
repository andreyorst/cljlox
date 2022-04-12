(ns cljloc.core-test
  (:require [cljloc.core :refer [run run-file run-prompt -main]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [cljloc.evaluator :as evaluator]
            [clojure.string :as str])
  (:import [clojure.lang ExceptionInfo]))

(use-fixtures :each
  (fn [t]
    (reset! evaluator/*global-env {:values evaluator/globals :enclosing nil})
    (t)))

(deftest comment-test
  (testing "comments are ignored"
    (is (= "0" (run "var x = 0; // x = 1;\nx")))))

(deftest variable-definition-test
  (testing "defining a global variable"
    (is (= "nil" (run "var x;"))))
  (testing "defining a global variable with assignment"
    (is (= "nil" (run "var x = nil;"))))
  (testing "defining a global variable with assignment of another variable"
    (is (= "nil" (run "var x = nil; var y = x;"))))
  (testing "defining a global variable with assignment of another itself"
    (is (= "nil" (run "var x = x;"))))
  (testing "defining a local variable"
    (is (= "nil" (run "{var x;}"))))
  (testing "defining a local variable with assignment"
    (is (= "nil" (run "{var x = nil;}"))))
  (testing "defining a local variable with assignment of another variable"
    (is (= "nil" (run "{var x = nil; var y = x;}"))))
  (testing "defining a local variable with assignment of another itself"
    (is (thrown? ExceptionInfo (run "{var x = x;}")))))

(deftest assignment-test
  (testing "assigning an unknown global"
    (is (nil? (run "a = 1;"))))
  (testing "assigning a known global"
    (is (= "nil" (run "var a = 1;"))))
  (testing "assigning to a string"
    (is (nil? (run "\"a\" = 1;")))))

(deftest math-precedence-test
  (testing "precedence is the same without explicit grouping"
    (is (= (run "1 + 2 * 3 / 4")
           (run "1 + (2 * 3) / 4")
           (run "1 + ((2 * 3) / 4)")
           "2.5")))
  (testing "explicit grouping affects precedence"
    (is (not= (run "1 + 2 * 3 / 4")
              (run "(1 + 2) * 3 / 4")))
    (is (= "2.25" (run "(1 + 2) * 3 / 4")))))

(deftest expression-test
  (testing "literals"
    (is (= "1" (run "1")))
    (is (= "false" (run "false")))
    (is (= "true" (run "true")))
    (is (= "nil" (run "nil")))
    (is (= "bar" (run "\"bar\""))))
  (testing "unary expressions"
    (is (= "-2" (run "-2")))
    (is (nil? (run "-nil")))
    (is (= "false" (run "!1")))
    (is (= "false" (run "!true")))
    (is (= "true" (run "!nil"))))
  (testing "various binary expressions"
    (is (= "2" (run "1 + 1")))
    (is (= "1" (run "1 * 1")))
    (is (= "1" (run "1 / 1")))
    (is (= "0" (run "1 - 1")))
    (is (= "foobar" (run "\"foo\" + \"bar\"")))
    (is (= "true" (run "1 < 2")))
    (is (= "true" (run "2 <= 2")))
    (is (= "false" (run "1 > 2")))
    (is (= "true" (run "2 >= 2")))
    (is (= "true" (run "2 == 2")))
    (is (= "true" (run "1 != 2"))))
  (testing "failing expressions"
    (is (nil? (run "1 + \"1\"")))
    (is (nil? (run "\"1\" + 1")))
    (is (nil? (run "\"1\" - \"2\"")))
    (is (nil? (run "\"1\" - 2")))
    (is (nil? (run "\"1\" * 2")))
    (is (nil? (run "\"1\" / 2")))
    (is (nil? (run "1 / 0")))))

(deftest condition-test
  (testing "only one branch is executed"
    (is (= "1" (run "var x = 0; if (true) { x = 1; } else { x = 2; } x")))
    (is (= "2" (run "var x = 0; if (false) { x = 1; } else { x = 2; } x")))
    (is (= "0" (run "var x = 0; if (false) { x = 1; } x")))))

(deftest function-test
  (testing "named functions defined at global scope"
    (is (= "#<function: f>" (run "fun f () { return 42; }"))))
  (testing "anonymous functions is returned at global scope"
    (is (= "#<function: anonymous>" (run "fun () { return 42; }"))))
  (testing "immediatelly calling a named functions"
    (is (= "42" (run "(fun f () { return 42; })()"))))
  (testing "immediatelly calling an anonymous function"
    (is (= "42" (run "(fun () { return 42; })()"))))
  (testing "immediatelly calling an anonymous function with arguments"
    (is (= "foobarbaz" (run "(fun (x, y, z) { return x + y + z; })(\"foo\", \"bar\", \"baz\")"))))
  (testing "arity mismatch"
    (is (nil? (run "fun f (x, y) { return x + y; } f(1)"))))
  (testing "return outside of the function"
    (is (nil? (run "return;")))
    (is (nil? (run "while (true) return;"))))
  (testing "function with more than 255 arguments"
    (is (nil? (run (str "fun f (" (->> (range 256) (map #(str "x" %)) (str/join ", ")) ") {}"))))
    (is (nil? (run (str "f(" (->> (range 256) (clojure.string/join ", ")) ")"))))))

(deftest closure-test
  (testing "returning anonymous function that closes over a function argument"
    (is (= "42" (run "fun f (x) {return fun (y) {return x + y;};} f(10)(32)"))))
  (testing "returning anonymous function that closes over a local variable"
    (is (= "27" (run "var f; {var x = 17; f = fun (y) {return x + y;};} f(10)"))))
  (testing "mutable scope bug test"
    (is (= "1" (run "var x = 1; var y; {fun f() { y = x; } var x = 2; f()} y")))))

(deftest iteration-test
  (testing "empty while loop"
    (is (= "nil" (run "while (false) {}"))))
  (testing "while loop with counter"
    (is (= "10" (run "var x = 0; var i = 10; while (i > 0) { i = i - 1; x = x + 1;} x"))))
  (testing "for loop"
    (is (= "10" (run "var x = 0; for (var i = 0; i < 10; i = i + 1) { x = x + 1; } x")))
    (is (= "10" (run "var x = 0; for (var i = 0; i < 10;) { i = i + 1; x = i; } x")))
    (is (= "10" (run "var x = 0; for (;x < 10;) { x = x + 1; } x")))
    (is (= "0" (run "var x = 0; for (;; x = x + 1) {break;} x")))
    (is (= "0" (run "var x = 0; for (;;) { break; } x"))))

  (testing "break from the loop"
    (is (= "5" (run "var x = 0; for (var i = 0; i < 10;) { i = i + 1; x = i; if (i == 5) break; } x"))))
  (testing "break outside of the loop"
    (is (nil? (run "break;")))))

(deftest logical-test
  (testing "logical operators short-circuit"
    (is (= "0" (run "var x = 0; fun f() { x = 1;} false and f(); x")))
    (is (= "0" (run "var x = 0; fun f() { x = 1;} true and false and f(); x")))
    (is (= "0" (run "var x = 0; fun f() { x = 1;} true or f(); x")))
    (is (= "0" (run "var x = 0; fun f() { x = 1;} false or true or f(); x")))))

(deftest builtins-test
  (testing "calling clock builtin"
    (is (= "true" (run "clock() > 0"))))
  (testing "calling clock builtin with wrong amount of arguments"
    (is (nil? (run "clock(1, 2) > 0")))))

(deftest calling-non-function-objects-test
  (testing "calling nil"
    (is (nil? (run "nil()"))))
  (testing "calling numbers"
    (is (nil? (run "1()")))))

(deftest print-test
  (is (= "10\n" (with-out-str (run "print 10;")))))

(deftest run-file-test
  (testing "running files"
    (is (= "21\n" (with-out-str (run-file "test/data/fib.lox")))))
  (testing "running files via main"
    (is (= "21\n" (with-out-str (-main "test/data/fib.lox"))))))

(deftest run-prompt-test
  (testing "running prompt"
    (let [vals ["42"]
          i (volatile! 0)]
      (with-redefs [read-line (fn [] (let [line (get vals @i)] (vswap! i inc) line))]
        (is (= "Welcome to CljLoc.\ncljloc> 42\ncljloc> " (with-out-str (-main)))))))
  (testing "running prompt with resolution error"
    (let [vals [(slurp "test/data/resolve-error.lox")]
          i (volatile! 0)]
      (with-redefs [read-line (fn [] (let [line (get vals @i)] (vswap! i inc) line))]
        (is (= "Welcome to CljLoc.\ncljloc> [2 10] resolve error: Can't read local variable in its own initializer.\n"
               (with-out-str (binding [*err* *out*](-main)))))))))

(deftest main-test
  (testing "main expects only one file"
    (is (nil? (-main 1 2))))
  (testing "main logs resolution errors"
    (is (= "test/data/resolve-error.lox [2 10] resolve error: Can't read local variable in its own initializer.\n"
           (with-out-str (binding [*err* *out*] (-main "test/data/resolve-error.lox")))))))

(deftest tokenization-error-test
  (testing "invalid token"
    (is (nil? (run "1 ~ 1")))))

(deftest parse-error-test
  (testing "unfinished expression"
    (is (nil? (run "1 +"))))
  (testing "unfinished statement"
    (is (nil? (run "return 1")))
    (is (nil? (run "print 1")))))

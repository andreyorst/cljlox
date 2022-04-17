(ns cljlox.core-test
  (:require [cljlox.core :refer [run run-file run-prompt -main]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [cljlox.evaluator :as evaluator]
            [clojure.string :as str])
  (:import [clojure.lang ExceptionInfo]))

(use-fixtures :each
  (fn [t]
    (reset! evaluator/*global-env {:values evaluator/globals :enclosing nil})
    (let [res (t)]
      (reset! evaluator/*global-env {:values evaluator/globals :enclosing nil})
      res)))

(defn- clean-run [source]
  (reset! evaluator/*global-env {:values evaluator/globals :enclosing nil})
  (run source))

(defn- clean-run-file [source]
  (reset! evaluator/*global-env {:values evaluator/globals :enclosing nil})
  (run-file source))

(defn- clean-run-prompt []
  (reset! evaluator/*global-env {:values evaluator/globals :enclosing nil})
  (run-prompt))

(deftest comment-test
  (testing "comments are ignored"
    (is (= "0" (clean-run "var x = 0; // x = 1;\nx")))))

(deftest variable-definition-test
  (testing "defining a global variable"
    (is (= "nil" (clean-run "var x;"))))
  (testing "defining a global variable with assignment"
    (is (= "nil" (clean-run "var x = nil;"))))
  (testing "defining a global variable with assignment of another variable"
    (is (= "nil" (clean-run "var x = nil; var y = x;"))))
  (testing "defining a global variable with assignment of another itself"
    (is (= "nil" (clean-run "var x = x;"))))
  (testing "defining a local variable"
    (is (= "nil" (clean-run "{var _x;}"))))
  (testing "defining a local variable with assignment"
    (is (= "nil" (clean-run "{var _x = nil;}"))))
  (testing "defining a local variable with assignment of another variable"
    (is (= "nil" (clean-run "{var x = nil; var _y = x;}"))))
  (testing "defining a local variable with assignment of another itself"
    (is (thrown? ExceptionInfo (clean-run "{var x = x;}")))))

(deftest assignment-test
  (testing "assigning an unknown global"
    (is (nil? (clean-run "a = 1;"))))
  (testing "assigning a known global"
    (is (= "nil" (clean-run "var a = 1;"))))
  (testing "assigning to a string"
    (is (nil? (clean-run "\"a\" = 1;")))))

(deftest math-precedence-test
  (testing "precedence is the same without explicit grouping"
    (is (= (clean-run "1 + 2 * 3 / 4")
           (clean-run "1 + (2 * 3) / 4")
           (clean-run "1 + ((2 * 3) / 4)")
           "2.5")))
  (testing "explicit grouping affects precedence"
    (is (not= (clean-run "1 + 2 * 3 / 4")
              (clean-run "(1 + 2) * 3 / 4")))
    (is (= "2.25" (clean-run "(1 + 2) * 3 / 4")))))

(deftest expression-test
  (testing "literals"
    (is (= "1" (clean-run "1")))
    (is (= "false" (clean-run "false")))
    (is (= "true" (clean-run "true")))
    (is (= "nil" (clean-run "nil")))
    (is (= "bar" (clean-run "\"bar\""))))
  (testing "unary expressions"
    (is (= "-2" (clean-run "-2")))
    (is (nil? (clean-run "-nil")))
    (is (= "false" (clean-run "!1")))
    (is (= "false" (clean-run "!true")))
    (is (= "true" (clean-run "!nil"))))
  (testing "various binary expressions"
    (is (= "2" (clean-run "1 + 1")))
    (is (= "1" (clean-run "1 * 1")))
    (is (= "1" (clean-run "1 / 1")))
    (is (= "0" (clean-run "1 - 1")))
    (is (= "foobar" (clean-run "\"foo\" + \"bar\"")))
    (is (= "true" (clean-run "1 < 2")))
    (is (= "true" (clean-run "2 <= 2")))
    (is (= "false" (clean-run "1 > 2")))
    (is (= "true" (clean-run "2 >= 2")))
    (is (= "true" (clean-run "2 == 2")))
    (is (= "true" (clean-run "1 != 2"))))
  (testing "failing expressions"
    (is (nil? (clean-run "1 + \"1\"")))
    (is (nil? (clean-run "\"1\" + 1")))
    (is (nil? (clean-run "\"1\" - \"2\"")))
    (is (nil? (clean-run "\"1\" - 2")))
    (is (nil? (clean-run "\"1\" * 2")))
    (is (nil? (clean-run "\"1\" / 2")))
    (is (nil? (clean-run "1 / 0")))))

(deftest condition-test
  (testing "only one branch is executed"
    (is (= "1" (clean-run "var x = 0; if (true) { x = 1; } else { x = 2; } x")))
    (is (= "2" (clean-run "var x = 0; if (false) { x = 1; } else { x = 2; } x")))
    (is (= "0" (clean-run "var x = 0; if (false) { x = 1; } x")))))

(deftest function-test
  (testing "named functions defined at global scope"
    (is (= "#<function: f>" (clean-run "fun f () { return; }"))))
  (testing "anonymous functions is returned at global scope"
    (is (= "#<function: anonymous>" (clean-run "fun () { return 42; }"))))
  (testing "immediatelly calling a named functions"
    (is (= "42" (clean-run "(fun f () { return 42; })()"))))
  (testing "immediatelly calling an anonymous function"
    (is (= "42" (clean-run "(fun () { return 42; })()"))))
  (testing "immediatelly calling an anonymous function with arguments"
    (is (= "foobarbaz" (clean-run "(fun (x, y, z) { return x + y + z; })(\"foo\", \"bar\", \"baz\")"))))
  (testing "arity mismatch"
    (is (nil? (clean-run "fun f (x, y) { return x + y; } f(1)"))))
  (testing "return outside of the function"
    (is (thrown? ExceptionInfo (clean-run "return;")))
    (is (thrown? ExceptionInfo (clean-run "while (true) return;"))))
  (testing "function with more than 255 arguments"
    (is (nil? (clean-run (str "fun f (" (->> (range 256) (map #(str "x" %)) (str/join ", ")) ") {}"))))
    (is (nil? (clean-run (str "f(" (->> (range 256) (clojure.string/join ", ")) ")"))))))

(deftest closure-test
  (testing "returning anonymous function that closes over a function argument"
    (is (= "42" (clean-run "fun f (x) {return fun (y) {return x + y;};} f(10)(32)"))))
  (testing "returning anonymous function that closes over a local variable"
    (is (= "27" (clean-run "var f; {var x = 17; f = fun (y) {return x + y;};} f(10)"))))
  (testing "mutable scope bug test"
    (is (= "1" (clean-run "var x = 1; var y; {fun f() { y = x; } var x = 2; x; f()} y")))))

(deftest iteration-test
  (testing "empty while loop"
    (is (= "nil" (clean-run "while (false) {}"))))
  (testing "while loop with counter"
    (is (= "10" (clean-run "var x = 0; var i = 10; while (i > 0) { i = i - 1; x = x + 1;} x"))))
  (testing "for loop"
    (is (= "10" (clean-run "var x = 0; for (var i = 0; i < 10; i = i + 1) { x = x + 1; } x")))
    (is (= "10" (clean-run "var x = 0; for (var i = 0; i < 10;) { i = i + 1; x = i; } x")))
    (is (= "10" (clean-run "var x = 0; for (;x < 10;) { x = x + 1; } x")))
    (is (= "0" (clean-run "var x = 0; for (;; x = x + 1) {break;} x")))
    (is (= "0" (clean-run "var x = 0; for (;;) { break; } x"))))

  (testing "break from the loop"
    (is (= "5" (clean-run "var x = 0; for (var i = 0; i < 10;) { i = i + 1; x = i; if (i == 5) break; } x"))))
  (testing "break outside of the loop"
    (is (thrown? ExceptionInfo (clean-run "break;")))))

(deftest logical-test
  (testing "logical operators short-circuit"
    (is (= "0" (clean-run "var x = 0; fun f() { x = 1;} false and f(); x")))
    (is (= "0" (clean-run "var x = 0; fun f() { x = 1;} true and false and f(); x")))
    (is (= "0" (clean-run "var x = 0; fun f() { x = 1;} true or f(); x")))
    (is (= "0" (clean-run "var x = 0; fun f() { x = 1;} false or true or f(); x")))))

(deftest builtins-test
  (testing "calling clock builtin"
    (is (= "true" (clean-run "clock() > 0"))))
  (testing "calling clock builtin with wrong amount of arguments"
    (is (nil? (clean-run "clock(1, 2) > 0")))))

(deftest calling-non-function-objects-test
  (testing "calling nil"
    (is (nil? (clean-run "nil()"))))
  (testing "calling numbers"
    (is (nil? (clean-run "1()")))))

(deftest print-test
  (is (= "10\n" (with-out-str (clean-run "print 10;")))))

(deftest run-file-test
  (testing "running files"
    (is (= "21\n" (with-out-str (clean-run-file "test/data/fib.lox")))))
  (testing "running files via main"
    (is (= "21\n" (with-out-str (-main "test/data/fib.lox"))))))

(deftest run-prompt-test
  (testing "running prompt"
    (let [vals ["42"]
          i (volatile! 0)]
      (with-redefs [read-line (fn [] (let [line (get vals @i)] (vswap! i inc) line))]
        (is (= "Welcome to Cljlox.\ncljlox> 42\ncljlox> " (with-out-str (-main)))))))
  (testing "running prompt with resolution error"
    (let [vals [(slurp "test/data/resolve-error.lox")]
          i (volatile! 0)]
      (with-redefs [read-line (fn [] (let [line (get vals @i)] (vswap! i inc) line))]
        (is (= "Welcome to Cljlox.\ncljlox> [2 10] resolve error: Can't read local variable in its own initializer.\n"
               (with-out-str (binding [*err* *out*](-main)))))))))

(deftest main-test
  (testing "main expects only one file"
    (is (nil? (-main 1 2))))
  (testing "main logs resolution errors"
    (is (= "test/data/resolve-error.lox [2 10] resolve error: Can't read local variable in its own initializer.\n"
           (with-out-str (binding [*err* *out*] (-main "test/data/resolve-error.lox")))))))

(deftest tokenization-error-test
  (testing "invalid token"
    (is (nil? (clean-run "1 ~ 1")))))

(deftest parse-error-test
  (testing "unfinished expression"
    (is (nil? (clean-run "1 +"))))
  (testing "unfinished statement"
    (is (nil? (clean-run "return 1")))
    (is (nil? (clean-run "print 1"))))
  (testing "unfinished expression"
    (is (nil? (clean-run "print 1")))
    (is (nil? (clean-run "print 1 print 2;"))))
  (testing "unfinished block"
    (is (nil? (clean-run "print 1")))
    (is (nil? (clean-run "{ print 1;")))
    (is (nil? (clean-run "{"))))
  (testing "broken for"
    (is (nil? (clean-run "for () {}")))
    (is (nil? (clean-run "for (;) {}")))
    (is (nil? (clean-run "for (1 + 1;) { 2 }")))
    (is (nil? (clean-run "for (1 + 1) { 2 }")))))

(deftest class-test
  (testing "definition"
    (is (= "#<class: A>" (clean-run "class A {}")))
    (is (= "#<class: B>" (clean-run "class A {} class B < A {}"))))
  (testing "class initialization"
    (is (= "nil" (clean-run "class A {} var a = A();")))
    (is (= "nil" (clean-run "class A { init() {} } var a = A();")))
    (is (= "nil" (clean-run "class A {} class B < A { init() {} } var b = B();")))
    (is (= "nil" (clean-run "class A { init() {} } class B < A { } var b = B();")))
    (is (= "nil" (clean-run "class A { init() {} } class B < A { init() {} } var b = B();"))))
  (testing "member access"
    (is (= "10" (clean-run "class A { } var a = A(); a.x = 10; a.x")))
    (is (= "10" (clean-run "class A { init() { this.x = 10 } } var a = A(); a.x")))
    (is (= "42" (clean-run "class A { init(x) { this.x = x; } } var a = A(10); a.init(42); a.x"))))
  (testing "method access"
    (is (= "27" (clean-run "class A { init() { this.x = 27; } get_x() { return this.x; } } var a = A(); a.get_x()"))))
  (testing "inherited method access"
    (is (= "27" (clean-run "class A { init() { this.x = 27; } get_x() { return this.x; } } class B < A {} var b = B(); b.get_x()"))))
  (testing "direct superclass method access"
    (is (= "80" (clean-run "class A { init() { this.x = 40; } get_x() { return this.x; } }
                            class B < A { init() { this.x = 2; super.init(); } get_x() { return this.x + super.get_x(); } }
                            var b = B(); b.get_x()")))))

(deftest resolve-error-test
  (testing "unused variable"
    (is (thrown? ExceptionInfo (clean-run "{var x;}"))))
  (testing "shadowing"
    (is (thrown? ExceptionInfo (clean-run "{var x = 1; var x = 2; print x;}"))))
  (testing "init returns value"
    (is (thrown? ExceptionInfo (clean-run "class A { init() { return 42; } }"))))
  (testing "can't inherit from itself"
    (is (thrown? ExceptionInfo (clean-run "class A < A {}"))))
  (testing "invalid use of super and this"
    (is (thrown? ExceptionInfo (clean-run "print this;")))
    (is (thrown? ExceptionInfo (clean-run "print super.x;")))
    (is (thrown? ExceptionInfo (clean-run "class A { init() { super.x(); } }")))))

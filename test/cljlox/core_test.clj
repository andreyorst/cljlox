(ns cljlox.core-test
  (:require
   [cljlox.core :refer [-main]]
   [clojure.test :refer [deftest is testing]]))

(deftest main-test
  (testing "main expects only one file"
    (is (nil? (-main 1 2))))
  (testing "main logs resolution errors"
    (is (= "test/data/resolve-error.lox [2 10] resolve error: Can't read local variable in its own initializer.\nnil\n"
           (with-out-str (binding [*err* *out*] (-main "test/data/resolve-error.lox"))))))
  (testing "running files via main"
    (is (= "21\n" (with-out-str (-main "test/data/fib.lox")))))
  (testing "running prompt"
    (let [vals ["42"]
          i (volatile! 0)]
      (with-redefs [read-line (fn [] (let [line (get vals @i)] (vswap! i inc) line))]
        (is (= "Welcome to Cljlox.\ncljlox> 42\ncljlox> " (with-out-str (-main)))))))
  (testing "running prompt with resolution error"
    (let [vals [(slurp "test/data/resolve-error.lox")]
          i (volatile! 0)]
      (with-redefs [read-line (fn [] (let [line (get vals @i)] (vswap! i inc) line))]
        (is (= "Welcome to Cljlox.\ncljlox> [2 10] resolve error: Can't read local variable in its own initializer.\nnil\ncljlox> "
               (with-out-str (binding [*err* *out*] (-main))))))))
  (testing "running non-existing file"
    (is (= "Fatal error: test/data/fi.lox (No such file or directory)\n"
           (with-out-str (-main "test/data/fi.lox"))))))

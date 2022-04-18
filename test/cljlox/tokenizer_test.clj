(ns cljlox.tokenizer-test
  (:require
   [cljlox.tokenizer :refer [map->Token tokenize]]
   [clojure.test :refer [deftest is testing]]))

(deftest tokenize-test
  (testing "basic tokenization"
    (is (= {:errors [],
            :tokens
            [(map->Token {:type :number, :lexeme "1", :literal 1.0, :pos [1 1]})
             (map->Token {:type :eof, :lexeme "EOF", :literal nil, :pos [1 2]})]}
           (tokenize "1")))
    (is (= {:errors [],
            :tokens
            [(map->Token {:type :number, :lexeme "1", :literal 1.0, :pos [1 1]})
             (map->Token {:type :plus, :lexeme "+", :literal nil, :pos [1 3]})
             (map->Token {:type :number, :lexeme "2", :literal 2.0, :pos [1 5]})
             (map->Token {:type :eof, :lexeme "EOF", :literal nil, :pos [1 6]})]}
           (tokenize "1 + 2")))
    (is (= {:errors [],
            :tokens
            [(map->Token {:type :number, :lexeme "1", :literal 1.0, :pos [1 1]})
             (map->Token {:type :slash, :lexeme "/", :literal nil, :pos [1 2]})
             (map->Token {:type :number, :lexeme "2", :literal 2.0, :pos [1 3]})
             (map->Token {:type :eof, :lexeme "EOF", :literal nil, :pos [1 4]})]}
           (tokenize "1/2")))))

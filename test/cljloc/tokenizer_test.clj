(ns cljloc.tokenizer-test
  (:require [clojure.test :refer [deftest is testing]]
            [cljloc.tokenizer :refer [tokenize map->Token]]))

(deftest tokenize-test
  (testing "basic tokenization"
    (is (= {:errors [],
            :tokens
            [(map->Token {:type :number, :lexeme 1.0, :literal nil, :pos [1 0]})]}
           (tokenize "1")))
    (is (= {:errors [],
            :tokens
            [(map->Token {:type :number, :lexeme 1.0, :literal nil, :pos [1 0]})
             (map->Token {:type :plus, :lexeme "+", :literal nil, :pos [1 2]})
             (map->Token {:type :number, :lexeme 2.0, :literal nil, :pos [1 4]})]}
           (tokenize "1 + 2")))
    (is (= {:errors [],
            :tokens
            [(map->Token {:type :number, :lexeme 1.0, :literal nil, :pos [1 0]})
             (map->Token {:type :slash, :lexeme "/", :literal nil, :pos [1 1]})
             (map->Token {:type :number, :lexeme 2.0, :literal nil, :pos [1 2]})]}
           (tokenize "1/2")))))

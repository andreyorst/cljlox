(ns cljloc.parser-test
  (:require [cljloc.parser :refer [parse]]
            [cljloc.tokenizer :refer [tokenize map->Token]]
            [cljloc.ast :refer [map->Unary map->Binary map->Literal map->Grouping]]
            [clojure.test :refer [deftest is testing]]))

(deftest parser-test
  (testing "basic parsing"
    (is (= [(map->Binary
             {:left (map->Literal
                     {:value (map->Token
                              {:type :number, :lexeme 1.0, :literal nil, :pos [1 0]})}),
              :operator (map->Token
                         {:type :plus, :lexeme "+", :literal nil, :pos [1 2]}),
              :right
              (map->Binary
               {:left (map->Literal
                       {:value (map->Token {:type :number, :lexeme 2.0, :literal nil, :pos [1 4]})}),
                :operator (map->Token {:type :star, :lexeme "*", :literal nil, :pos [1 6]}),
                :right (map->Literal
                        {:value (map->Token
                                 {:type :number, :lexeme 3.0, :literal nil, :pos [1 8]})})})})]
           (parse (:tokens (tokenize "1 + 2 * 3")))))))

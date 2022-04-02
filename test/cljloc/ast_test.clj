(ns cljloc.ast-test
  (:require [cljloc.ast :refer [tostring-expr map->Literal map->Binary]]
            [cljloc.tokenizer :refer [map->Token]]
            [clojure.test :refer [deftest is testing]]))

(deftest ast-print-test
  (testing "converting AST to a string"
    (is (= (tostring-expr (map->Binary
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
                                               {:type :number, :lexeme 3.0, :literal nil, :pos [1 8]})})})}))
           "(+ 1.0 (* 2.0 3.0))"))))

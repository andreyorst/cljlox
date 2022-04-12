(ns cljloc.parser-test
  (:require [cljloc.parser :refer [parse]]
            [cljloc.tokenizer :refer [tokenize map->Token]]
            [cljloc.ast :refer [map->Binary map->Literal]]
            [clojure.test :refer [deftest is testing]]))

(deftest parser-test
  (testing "basic parsing"
    (is (= [(map->Binary
             {:left (map->Literal {:value 1.0}),
              :operator (map->Token {:type :plus, :lexeme "+", :literal nil, :pos [1 3]}),
              :right
              (map->Binary
               {:left (map->Literal {:value 2.0}),
                :operator (map->Token {:type :star, :lexeme "*", :literal nil, :pos [1 7]}),
                :right (map->Literal {:value 3.0})})})]
           (parse (:tokens (tokenize "1 + 2 * 3")))))))

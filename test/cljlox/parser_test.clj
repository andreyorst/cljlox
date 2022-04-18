(ns cljlox.parser-test
  (:require
   [cljlox.ast :refer [map->Binary map->Literal]]
   [cljlox.parser :refer [parse]]
   [cljlox.tokenizer :refer [map->Token tokenize]]
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

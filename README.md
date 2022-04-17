# CljLox - Lox interpreter in Clojure

Lox is a language developed through the craftinginterpreters.com book.
This repository contains a tree walking interpreter written in Clojure.
Because of Clojure's focus on immutability and avoidance of OOP, some changes had to be made to the code structure.

Instead of the Visitor pattern, as used in the book, this implementation relies on protocols and everything is immutable where it is not too out of hand.
The only mutable part of the interpreter is the runtime environment, which is implemented as a Clojure's volatile, and the parser, resolver, and tokenizer are immutable.

## Building and running

Execute `clojure -T:build uber` in the root directory of the repository.
Run with `java -jar target/cljlox-0.0.X-standalone.jar`, where `X` is the number of git revisions.

## License

[MIT](LICENSE)

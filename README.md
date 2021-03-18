pegtool
=======

*Note:* **This project is a work in progress and is not ready for usage.**

A PEG parser generator for C++.

### API Usage

```cpp
const char grammarSpec[] = R"(
Example <- Foo Bar
Foo <- 'foo'
Bar <- 'bar'
)";

peg::Grammar grammar(grammarSpec);

std::unique_ptr<peg::NonTerminal> root = grammar.parse("foobar");

root->print(std::cout);
```

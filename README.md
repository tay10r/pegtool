pegtool
=======

*Note:* **This project is a work in progress and is not ready for usage.**

`pegtool` is a dynamic parser generator for generating parse trees from input
files.

### API Usage

```cpp
const char grammarSpec[] = R"(
Example <- Foo Space Bar
Foo <- 'foo'
Bar <- 'bar'
Space <- [ \t]+
)";

peg::Grammar grammar(grammarSpec);

std::unique_ptr<peg::Node> rootNode = grammar.parse("foo bar");

root->print(std::cout);
```

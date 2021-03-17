pegtool
=======

*Note:* **This project is a work in progress and is not ready for usage.**

A PEG parser generator for C++.

Notable features:

 - linear, memory efficient parse tree
 - runtime generated parser VM

### API Usage

```cpp
const char grammarSpec[] = R"(
Example <- 'foo'
)";

peg::Grammar grammar;

grammar.parse(grammarSpec);

peg::Module module; // <- Stores code for the VM to execute.

module.load(grammar); // <- Converts the grammar to bytecode.

peg::ParseTree parseTree = module.exec("foo bar", sizeof("foo bar") - 1); // <- Invokes the VM.

parseTree.print(parseTree.getRoot(), std::cout); // <- Recursively prints the resulting parse tree.
```

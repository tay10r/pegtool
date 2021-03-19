Writing Grammar
===============

The syntax for PEG was first described by Bryan Ford in the article [Parsing Expression Grammars: A Recognition-Based Syntactic Foundation](https://bford.info/pub/lang/peg.pdf).
Much of the syntax can be found on that article. However, the syntax is also described here so that any specific details not covered in the article can also be discussed.

A very simple PEG can be considered as a starting point:

```
Example <- 'foo'
```

The line `Example <- 'foo'` all together is called a definition. On the left side is a unique name and on the right side is the parsing
expression that is being associated to the name. In this case, `foo` is the expression (a string literal expression to be specific). It
is much like declaring a function, where in C it would look something like this:

```c
struct Result Example(struct Input input) {
  return MatchStringLiteral(input, "foo");
}
```

This definition will generate a parser that checks for the letters `foo` and nothing less. If the following data:

```
foo bar
```

Then the parser would recognize 'foo', stopping at the space character, and generate the parse tree branch called `Example`. The parse tree (or concrete syntax tree) can be printed
showing the result. In this case, it would look like this:

```
Example:
  'foo'
```

The rest of the input is left alone. In this parse tree, `'foo'` is a leaf node and `Example` is a branch node.

There are other types of expressions, such as character classes (similar to the ones in regular expressions), dot expressions, and more.

Character classes are useful for matching a range of characters. Here's an example of a definition that matches a range of characters from zero to nine.

```
DecimalDigit <- [0-9]
```

The square brackets indicate the beginning and end of a character class expression. More than one range of characters can be put into a character class.
For another example:

```
HexDigit <- [0-9a-fA-F]
```

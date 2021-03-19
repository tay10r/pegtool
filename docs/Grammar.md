Writing Grammar
===============

The syntax for PEG was first described by Bryan Ford in the article [Parsing Expression Grammars: A Recognition-Based Syntactic Foundation](https://bford.info/pub/lang/peg.pdf).
Much of the syntax can be found on that article. However, the syntax is also described here so that any specific details not covered in the article can also be discussed.

A very simple PEG file can be considered as a starting point:

```
Example <- 'foo'
```

This will generate a parser that checks for the letters `foo` and nothing less. If the following data:

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

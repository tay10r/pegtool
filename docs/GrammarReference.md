Grammar Reference
=================

### Character Literal

A character literal is used in [string literals](#String Literal) and
[character classes](#Character Class) to express character values. This either
going to be a unicode character or an escape sequence. The following escape
sequences are supported:

| Sequence     | Description                                                       |
|--------------|-------------------------------------------------------------------|
|         `\t` | Used to indicate a tab character.                                 |
|         `\n` | Used to indicate a line feed character.                           |
|         `\r` | Used to indicate a carriage return character.                     |
|         `\'` | Used to indicate a single quote character.                        |
|         `\"` | Used to indicate a double quote character.                        |
|         `\[` | Used to indicate a left square bracket.                           |
|         `\]` | Used to indicate a right square bracket.                          |
|       `\***` | An octal byte, where `***` are octal digits describing the value. |
|       `\x**` | A hex byte, where `**` are hex digits describing the value.       |
|     `\u****` | A series of four hex digits describing a unicode value.           |
| `\U********` | A series of eight hex digits, describing a unicode value.         |

Here are examples of character literals

### Character Class

A character class expression begins with `[` and ends with `]`. Within these
brackets is a list of sub-expressions. The parser will match any one of the
sub-expressions. A sub-expression is either an interval expression or an
equality expression. An interval expression is in the form:

```
a-b
```

Where `a` can be any character literal and `b` can be any character literal
greater than `a`. If `b` is not greater than `a`, an error is emitted.

Equality sub-expressions are just a single character literal. The only special
case is the character `-`, which must be the last character literal in the
character class for it to be matched as a single character.

Finally, a character class can contain any number of sub-expressions. The
sub-expressions are tested in the order that they appear in.

Here are some examples of valid character class expressions.

```
NonDigit <- [a-zA-Z_]

Digit <- [0-9]

PlusOrMinus <- [+-]

CapitalAWithTopGlyph <- [À-Å]
```

### Dot

### String Literal

A string literal begins with either a double quote or single quote.
The body of the string literal consists of character literals and
spans until the same quote character is found that started the literal.
Escaped quote characters do not terminate a string literal.

Here are some examples of valid string literals.

```
"example 1"
'example 2'
"example '3'"
'example "4"'
"example \"5\""
'example \'6\''
```

There is no difference between using `'` and `"`, it's entirely a matter of
preference. You may prefer to use double quotes when the body of the string
literal contains single quotes (and vice versa).

### Reference

### Sequence

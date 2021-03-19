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

### Reference

### Sequence

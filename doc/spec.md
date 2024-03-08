DR - Specification & Syntax
===========================

Derelict Resources (DR) is a configuration language and parser library. The language's syntax aims to be both human-readable and easy to parse. Yet provides enough tools to the end user to create branching and dynamic configurations that can be modified and reloaded on the fly. As a core feature, DR represents, and stores, all resources as arrays of strings that can be then converted to any type.

Other notable features :

- variables
- user-defined sections
- arithmetic operations
- string operations
- color operations
- iteration loops
- conditionals
- child file inclusion

Table of contents
-----------------

- usecase & scope
- fundamentals
	- sequences
	- tokens
	- numerals tokens
- sequences types
	- resources
	- sections
	- section additions
	- section deletions
	- variable declaration
	- iterations
	- child file inclusion
- inline operations
	- variable inclusion
	- comments
	- ...

Usecase & scope
---------------

TODO

Fundamentals
------------

Every DR configuration file is series of 'sequences' separated by newlines until EOF is reached. Empty lines between sequences are ignored. Leading and trailing whitespaces (either space or tab characters) are ignored too.

```
sequence
sequence
sequence
sequence
```

Sequences themselves are a series of 'tokens', words encoded in UTF-8 separated by any amount of whitespace in-between.

```
token token token
token token
token token token token token
token token token token
```

If a whitespace need to be explicitely part of a token, said token can be wrapped in single or double quotes. Likewhise, single quotes can be wrapped in double quotes and vice-versa. Additionaly, inside a quote wrap, newlines are also conserved.

```
"  token "
' token  '
'line 1
line2'
'"'
"'"
```

Following that, token can be of 4 types:

- string, generic and default value type
- numeral, a double number representation
- special, specific string values that equate to a DR language function
- invalid, in case of a parsing or conversion error.

When an invalid type token is encountered in the middle of a sequence, the sequence is ended prematurely and the remaining tokens until a newline are skipped.

Numeral token values can be represented in 3 styles:

- decimal
- hexadecimal
- RGBA color hexadecimal

A decimal number consists of a nonempty sequence of decimal digits possibly containing a radix character (decimal point, locale-dependent, usually '.'), optionally followed by a decimal exponent. A decimal exponent consists of an 'E' or 'e', followed by an optional plus or minus sign, followed by a nonempty sequence of decimal digits, and indicates multiplication by a power of 10.

A hexadecimal number consists of a "0x" or "0X" followed by a nonempty sequence of hexadecimal digits possibly containing a radix character, optionally followed by a binary exponent. A binary exponent consists of a 'P' or 'p', followed by an optional plus or minus sign, followed by a nonempty sequence of decimal digits, and indicates multiplication by a power of 2. At least one of radix character and binary exponent must be present.

A RGBA color hexadecimal number consists of a "#" followed by 6 or 8 hexadecimal digits that represent the red, green, blue and optionaly alpha color component. If the alpha color component is omitted, a fully opaque "FF". If there are no 6 or 8 digits an invalid token will be returned instead. This is a convenience syntax to represent and operate on colors, and internally they are first converted into an ARGB 32-bits unsigned integer then cast into a double.

```
1234.567
0xFa12
#12A0ff
```


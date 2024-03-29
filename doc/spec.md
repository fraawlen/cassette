DR - Specification & Syntax (WIP)
===========================

Derelict Resources (DR) is a configuration language and parser library featuring array based values and short s-like expressions based functions. The language's syntax aims to be both human-readable and easy to parse. Yet provides enough tools to the end user to create branching and dynamic configurations that can be modified and reloaded on the fly.

This document specifies and details all of DR's features and syntax rules. It's aimed at both end-users of software that uses this language and developers of such software.

Table of contents
-----------------

1. Use-case & Scope
2. Fundamentals
	1. Sequences
	2. Tokens
	3. Resources
3. Tokens Types
	1. Invalids
	2. Strings
	3. Numerals
	4. Functions
4. Sequences Leads
	1. Sections
	2. Section Additions
	3. Section Deletions
	4. Variable Declaration
	5. Enumeration Declaration
	6. Iterations
	7. Child File Inclusion
	8. Seed Override
5. Substitutions
6. Tips & Tricks

## 1. Use-case & Scope

TODO

## 2. Fundamentals


### 2.1. Sequences

Every DR configuration file is a series of 'sequences' separated by newlines until EOF is reached. Empty lines between sequences are ignored. Leading and trailing white-spaces (either space or tab characters) are ignored too.

```
sequence
sequence
sequence
sequence
```

### 2.2. Tokens

Sequences themselves are a series of 'tokens'. Tokens are 32 bytes words, null terminator included, encoded in ASCII / UTF-8, separated by any amount of white space in-between. If the word's length exceeds 31 bytes, only the first 31 bytes are kept and a null terminator is appended at the 32th byte.

```
token token token
token token
token token token token token
token token token token
```


In DR, there is no concept of priority based on the presence of parenthesis, brackets, or braces. Tokens are strictly parsed from left to right and any language function that gets triggered should happen as the tokens are read. Thanks to that, words read from a source configuration file only need to be parsed and processed once. The only exception to this rule is the [iteration-type sequences]().

If white spaces need to be explicitly part of a token, said token can be wrapped in single or double quotes. Likewise, single quotes can be wrapped in double quotes and vice-versa. Additionally, within quote wraps, newlines are also conserved and do not terminate sequences. Non white space characters directly preceding or following quotes are considered to be part of the same word. The same applies to multiple groups of quoted word sections.

```
"  token "
' token  '
'multi-line
token'
'token with internal "quotes"'
"token with internal 'quotes'"
'this is a single 'token
'this is'" still a single "token
```

### 2.3. Resources

Sequences that make up a program's configuration are called resources. The first token represents the resource's namespace and the second one is the resource's name. Namespaces help reuse resource names without collisions. The tokens that follow until the end of the sequence are interpreted as values attributed to the resource. Resources definitions with no values are ignored. Therefore, valid resources are sequences of at least 3 tokens. There are no upper token count limit (apart from the system's memory). If the same resource is defined more than once, the latest definition overwrites the previous ones.

```
namespace name value_1 value_2 value_3 ...
```

If a program requires a resource with N values and the matching resource in the configuration file has less than N values defined, the interpretation of missing values is up to the program.

By convention, and to minimize possible collision with function tokens, resources namespaces, resources names and (if possible) resource values should be written in full lowercase.

At its simplest, a DR configuration file can just be a series of resources. In this example, for a hypothetical GUI library, we define 6 resources spread across 2 widgets, `button` and `label`. The resources namespaces borrow their widget name. The resources named `corner-radius` get assigned 4 values, one for each widget corner.

```
button background_color #808080
button border_width 3
button corner-radius 0 0 3 0

label background_color #555555
label border_width 0
label corner-radius 0 0 0 0
```

## 3. Tokens Types

 When parsed, tokens get converted into one of the following 4 types:

- invalid
- string
- numeral
- function

### 3.1. Invalids

If any error during token parsing happens, like a failed conversion, wrong or missing function parameters, or a system error, the token will be marked as invalid. If an invalid type token is encountered in the middle of a sequence, the sequence is ended prematurely and the following tokens are skipped until a new sequence begins.

### 3.2. Strings

String tokens are the default token type, as they are just a text value that represents a resource namespace, resource name, or general purpose value with no language function. 

### 3.3. Numerals

Numeral tokens are double-floating IEEE-754 values. Tokens are only interpreted as numerals when a language function requires it. They can be represented in 3 styles:

- decimal
- hexadecimal
- RGBA color hexadecimal

A decimal number consists of a non-empty sequence of decimal digits possibly containing a radix character (decimal point, locale-dependent, usually '.'), optionally followed by a decimal exponent. A decimal exponent consists of an 'E' or 'e', followed by an optional plus or minus sign, followed by a non-empty sequence of decimal digits, and indicates multiplication by a power of 10.

```
1234.567
-> 1234.567
```

A hexadecimal number consists of a "0x" or "0X" followed by a non-empty sequence of hexadecimal digits possibly containing a radix character, optionally followed by a binary exponent. A binary exponent consists of a 'P' or 'p', followed by an optional plus or minus sign, followed by a non-empty sequence of decimal digits, and indicates multiplication by a power of 2.

```
0xFa12
-> 64018.0
```

An RGBA color hexadecimal number consists of a '#' followed by 6 or 8 hexadecimal digits that represent the red, green, blue, and optionally alpha color components. If the alpha color component is omitted, a fully opaque "FF" alpha value is assumed. If the number is not made of 6 or 8 digits an invalid token will be returned instead. This number representation is a convenience syntax to represent and operate on colors. Internally they are first converted into an ARGB 32-bit unsigned integer and then cast into a double. This also means that resources that expect 'colors' values, can technically accept decimal or hexadecimal doubles.

```
#12A0b801
-> 0x0112A0B8
-> 17998008.0

#12A0b8
-> #12A0b8FF
-> 0xFF12A0B8
-> 4279410872.0

#1208A
-> invalid, not enough digits
```

### 3.4. Functions

Function tokens are defined by an exact, case-sensitive ASCII sequence of non-white-space characters. As soon as they're read, they should trigger a language function. If the function fails due to invalid input parameters or internal memory issues, an invalid token is returned. Functions tokens can be separated into two sub-types: transformation tokens and sequences leads.

By convention, and to minimize possible collision with user strings, all function tokens are defined in fully capitalized characters.

Substitution tokens are inline function tokens that may optionally take the next few tokens as input parameters, and then return a different token as a function result. These functions feature an s-like expression syntax without parenthesis. Parenthesis in DR functions are unnecessary because the amount of input parameters for each function is known in advance. Multiple substitutions tokens can be chained together, resulting in nested functions. Some functions may take string or numeral tokens as inputs, it is up to the parser to make the necessary implicit conversions. In the next few examples, the `JOIN` function tokens take the next 2 string tokens and return a concatenated string token.

```
a b
-> a b

JOIN a b
-> ( JOIN a b )
-> ab

a JOIN b c d
-> a ( JOIN b c ) d
-> a bc d

a JOIN JOIN b c d
-> a ( JOIN ( JOIN b c ) d )
-> a ( JOIN bc d )
-> a bcd

a JOIN b JOIN c d
-> a ( JOIN b ( JOIN c d ) )
-> a ( JOIN b cd )
-> a bcd

JOIN a
-> invalid, missing 2nd JOIN parameter
```

Sequence lead tokens are special function tokens that define the role and type of a given sequence. As their name implies they are always the first token in a sequence. If they are placed elsewhere they have no effect. However, if the first token in a sequence is a transformation token, and this token returns a valid sequence lead token, then the sequence lead may still be processed normally. But if the first token, after transformation, is not a sequence lead nor is invalid, the started sequence will be then treated as resource definition. The following examples demonstrate the applications of the `INCLUDE` sequence lead. When valid, the remaining tokens in the sequence (post-transformation) are interpreted as paths to child configuration files.

```
INCLUDE path1 path2
-> open and parse file at path1 then do the same for path2

a INCLUDE path1 path2
-> not an inclusion because INCLUDE is not first, instead the sequence is now a resource definition with a resource namespace "a", resource name "INCLUDE", and assigned resource values "path_1" and "path_2"
```

## 4. Sequences Leads

### 4.1. Sections

Sequences can be grouped into user-defined sections. Sections can be then selectively parsed. When a section is not enabled, all other sequences will be skipped until a new section starts.

A section sequence is defined with a `SECTION` lead token, followed by section names. All section names need to be enabled (see [Section Additions]() and [Section Deletions]()) for the given section to be allowed to be parsed. Sections can be repeated. By convention, to differentiate section names from function tokens and generic strings or values, section names should capitalize their first character.

Before a section is defined, all sequences are part of the 'always' section, and will always be read and processed. Likewise, if a section definition does not have any section name following it, the following sequences will also belong to the 'always' section and will always be processed.

```
	sequence // always read
	sequence // always read

SECTION Sa
	sequence // read only if Sa has been enabled
	sequence // read only if Sa has been enabled

SECTION Sb
	sequence // read only if Sb has been enabled

SECTION Sa Sb
	sequence // read only if Sa AND Sb have been enabled

SECTION Sa
	sequence // read only if Sa has been enabled

SECTION
	sequence // always read
```

### 4.2. Section Additions

### 4.3. Section Deletions

### 4.4. Variable Declaration

### 4.5. Enumerations Declaration

### 4.6. Iterations

### 4.7. Child File Inclusion

### 4.8, Seed override

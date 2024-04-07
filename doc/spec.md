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
	6. Combination of Variables
	7. Iterations
	8. Child File Inclusion
	9. Seed Override
	10. Debug Print
5. Substitutions
	1. Comments
	2. Fillers
	3. End-of-File
	4. Escape
	5. Variable Injection
	6. Iteration Injection
	7. Join
	8. Conditions
	9. Math Operations
	10. Color Operations
	11. Constants
	12. Timestamp
	13. Random value
6. Full examples
	1. GUI Configuration
	2. Network Simulator

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

In DR, there is no concept of priority based on the presence of parentheses, brackets, or braces. In fact, parentheses get interpreted as white space, so even if they can be used to visually group tokens together, they bear no effect. Instead, tokens are strictly parsed from left to right and any language function that gets triggered should happen as the tokens are read. Thanks to that, words read from a source configuration file only need to be parsed and processed once. The only exception to this rule is the [iteration-type sequences]().

```
These 3 sequences are parsed the same way
These 3 (sequences are) parsed ((the same ) way )
These 3 sequences )()()( are parsed )))))the same way))))))
```

If white spaces or parentheses need to be explicitly part of a token, said token can be wrapped in single or double quotes. Likewise, single quotes can be wrapped in double quotes and vice-versa. Additionally, within quote wraps, newlines are also conserved and do not terminate sequences. Non white space characters directly preceding or following quotes are considered to be part of the same word. The same applies to multiple groups of quoted word sections.

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

By convention, and to minimize possible collision with function tokens, resources tokens (namespaces, names and values) should be written in full lowercase.

At its simplest, a DR configuration file can just be a series of resources. In this example, for a hypothetical GUI library, we define 8 resources spread across 2 widgets, `button` and `label`. The resources namespaces borrow their widget name. The resources named `corner-radius` and `corner-style` get assigned 4 values, one for each widget corner. 

```
button background_color #808080
button border_width 3
button corner-radius 0 0 3 0
button corner-style straight straight radii straight

label background_color #555555
label border_width 0
label corner-radius 4 4 4 4
button corner-style radii radii radii radii
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
-> (JOIN a b)
-> ab

a JOIN b c d
-> a (JOIN b c) d
-> a bc d

a JOIN JOIN b c d
-> a (JOIN (JOIN b c) d)
-> a (JOIN bc d)
-> a bcd

a JOIN b JOIN c d
-> a (JOIN b (JOIN c d))
-> a (JOIN b cd)
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

```
SECTION [section_name] [section_name] ...
```

Sequences can be grouped into user-defined sections. Sections can be then selectively parsed. When a section is not enabled, all other sequences will be skipped until a new section starts.

A section sequence is defined with a `SECTION` lead token, followed by section names. All section names need to be enabled (they are disabled by default, see [Section Additions]() and [Section Deletions]()) for the given section to be allowed to be parsed. Sections can be repeated. By convention, to differentiate section names from function tokens and generic strings or values, section names should capitalize their first character.

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

```
SECTION_ADD [section_name] [section_name] ...
```

Sequences belonging to a section will only be parsed and processed if said section has been enabled. This can be done with a sequence starting with `SECTION_ADD` followed by any number of tokens. These tokens represent the names of sections to enable.

### 4.3. Section Deletions

```
SECTION_DEL [section_name] [section_name] ...
```

Sequences belonging to a section will only be parsed and processed if said section has been enabled. But after having been enabled, sections can be disabled again with a sequence starting with `SECTION_DEL` followed by any number of tokens.  These tokens represent the names of sections to disable. The effects of this sequence are only applicable after the present section ends.

### 4.4. Variable Declaration

```
LET [new_variable_name] [value] [value] ...
```

The DR configuration language offers support for user-defined variables. Similarly to resources, variables are arrays of values. They can be declared with a section starting with the token `LET` followed by the name of the variable (that will be used to reference it in other parts of the file), then by any number of tokens. After that, when a variable is invoked in another sequence (see [Variable Injection]()) the variable values will be injected into that sequence.

```
LET var a b c
1 2 $ var 3
-> 1 2 a b c 3

LET var a b
JOIN $ var
-> (JOIN a b)
-> ab
```

And like any other sequences, variable declarations are subject to token substitutions, meaning function tokens, including variable injections, will be resolved when the variable declaration is parsed.

```
LET var_a a b
LET var_b JOIN $ var_a c
-> LET var_b (JOIN a b) c
-> LET var_b ab c

$ var_b 1 2 3 $ var_a
-> ab c 1 2 3 a b
```

Finally,  if the same variable is declared more than once, the latest declaration overwrites the previous ones.

```
LET var a b c d
LET var h i j
$ var
-> h i j
```

### 4.5. Enumerations Declaration

```
LET_ENUM [new_variable_name] [min] [max] [steps] [precision]
LET_ENUM [new_variable_name] [min] [max] [steps]
LET_ENUM [new_variable_name] [min] [max]
LET_ENUM [new_variable_name] [max]
```

Enumerations are variables whose values are not written down strings but instead are a sequence of numbers generated according to 4 parameters:

- min : value to start from
- max : value to end at
- steps : 
- precision : the amount of decimals, because numbers are stored as doubles

TODO

### 4.6. Combination of variables

```
LET_APPEND  [new_variable_name] [variable_name] [string]
LET_PREPEND [new_variable_name] [variable_name] [string]
LET_MERGE   [new_variable_name] [variable_name] [variable_name]
```

TODO

### 4.7. Iterations

```
ITERATE_RAW [variable_name] [token] [token] ...
ITERATE     [variable_name] [token] [token] ...
```

TODO

### 4.8. Child File Inclusion

```
INCLUDE [filename] [filename] ...
```

Alongside sections, resources can also be put in separate files that can be opened and parsed with an inclusion sequence that starts with an `INCLUDE`token followed by filenames of files to include. The filenames are relative to the location of the file the sequence is read from. Unless they start with '/' in which case the given filename are absolute.

The contents of the included files are treated as if they've been copy-pasted into the parent file at the position of the inclusion sequence. In other words, declared variables, enumerations, and section states are carried over to the included child file. And any modifications that happen in inclusions are also brought back to the parent file.

For example, these 3 files :

```
// ~/.config/parent
SECTION_ADD Sa
INCLUDE ../child_1 /child_2
1 2 3 $ var
```
```
// ~/child_1
SECTION Sa
```
```
// /child_2
LET var a b c
```

Are equivalent to a single file like this :

```
SECTION_ADD Sa
SECTION Sa
	LET var a b c
	1 2 3 % var
```

That gets resolved into :
```
1 2 3 a b c
```

### 4.9. Seed Override

```
SEED_OVERRIDE [seed_value]
```

Resets and sets the seed of the parser's random number LCG. Can be used to shuffle the values returned by [random substitutions](). However, this sequence is an override. It can conflict with software that sets its own parser seed. Check the software's documentation before using it.

### 4.10. Debug Print

```
DEBUG_PRINT [token] [token] ...
```

Prints to `stderr` the resolved sequence that follows the lead token.

## 5. Substitutions
	
### 5.1. Comments

```
//
```

Any tokens written after this token will be ignored until the next newline.

It should be noted that this is a token, therefore there should be some white space before the start of the comment text.

```
a b // proper comment
a b //not a comment
```

### 5.2. Fillers

```
=
:=
```

Eye-candy that will be skipped during parsing.

In this example, all three definitions are identical :
```
namespace property    value
namespace property  = value
namespace property := value
```

### 5.3. End-of-File

```
EOF
```

Ends a sequence and file early. If the file this token is read from happens to be an [included file](), only that file gets closed, and parsing resumes in the parent file.

### 5.4. Escape

```
\ [token]
\ [newline]
```

The next token is forcefully interpreted as a string and no substitutions will be performed on it. But if there is no token following the escape, then the newline is ignored instead. Thanks to that, it's possible to define sequences that span multiple lines.

```
JOIN a b
-> ab

\ JOIN a b
-> JOIN a b

this \
is \
a \
multiline \
sequence
-> this is a multiline sequence
```

### 5.5. Variable Injection

```
$ [variable_name]
```

Injects a variable's sequence of values into the current sequence. The variable being called needs to be declared first with a sequence starting with `LET`, `LET_ENUM`, `LET_APPEND`, `LET_PREPEND`, or `LET_MERGE`. If the variable was not declared beforehand, an invalid token will be returned instead.

```
LET var a b c
1 2 $ var 2
-> 1 2 a b c 3
```

### 5.6. Iteration Injection

```
%
```

Within an iterated sequence, this token returns a value of the iteration's variable. Outside iterations this token is treated as a string. See [Iterations]() for more details.

### 5.7. Join

```
JOIN [string] [string]
```

Concatenates the next 2 tokens and returns the resulting string.

```
JOIN a b
-> ab
```

### 5.8. Conditions

```
<  [double] [double] [token_if_true] [token_if_false]
<= [doudle] [double] [token_if_true] [token_if_false]
>  [double] [double] [token_if_true] [token_if_false]
>= [double] [double] [token_if_true] [token_if_false]
== [double] [double] [token_if_true] [token_if_false]
!= [double] [double] [token_if_true] [token_if_false]
```

Compares two doubles. Depending on the result, either the 3rd or 4th token is returned.

```
!= 0.3 0.5 a b
-> a
```

Only one token is returned at a time. However it can be combined with variables is whole sequences need to be returned instead :

```
LET var_a a b c
LET var_b 1 2 3
$ == 0.0 1.0 var_a var_b
-> ($ (== 0.0 1.0 var_a var_b))
-> ($ var_b)
-> 1 2 3
```

### 5.9. Math Operations

```
SQRT  [double]
CBRT  [double]
ABS   [double]
CEIL  [double]
FLOOR [double]
ROUND [double]
COS   [double]
SIN   [double]
TAN   [double]
ACOS  [double]
ASIN  [double]
ATAN  [double]
COSH  [double]
SINH  [double]
LN    [double]
LOG   [double]
+     [double] [double]
-     [double] [double]
*     [double] [double]
/     [double] [double]
MOD   [double] [mod]
POW   [double] [exponent]
BIG   [double] [double]
SMALL [double] [double]
ITPRL [double] [double] [0.0-1.0] // Interpolation
LIMIT [double] [min] [max]
```

Performs a mathematical operation and returns the resulting double value.

### 5.10. Color Operations

```
CITPRL [color] [color] [0.0-1.0]       // Color interpolation
RGB    [0-255] [0-255] [0-255]         // Composes a color
RGBA   [0-255] [0-255] [0-255] [0-255] // Composes a color
```

Performs color specific operations and returns a double value representing a color. Color being internally represented as number, normal [mathematical operations]() can also be used. However this set of substitutions perform operations channel by channel.

### 5.11. Constants

```
PI    // 3.1415926535897932
E     // 0.5772156649015328
TRUE  // 1.0
FALSE // 0.0
```

Returns a constant value in double format.

### 5.12. Timestamp

```
TIME
```

Returns a UNIX timestamp in seconds.

### 5.13 Random value

```
RAND [min] [max]
```

Returns a pseudo-random double ranging from `min` to `max`.
Because the internal RNG is an LCG, as long as the initial seed is not modified, the same value will be returned between different reloads.

## 6. Full Examples

### 6.1. GUI Configuration

In this first example, we consider a hypothetical GUI tooklit or application that lets its end-users customize the appearance of its widgets. Thanks to DR's sections, different themes can co-exist in the same source file. Moreover, the themes are selected automatically depending on the value of a brightness variable. Said variable could be then set by an external program that tracks the values of a light sensor. Hence, with this kind of configuration, an application or toolkit can switch between a light and dark theme depending on the amount of light hitting the device they're running from.

```
	LET brightness 0.6 // [0.0-1.0]
	LET widgets \
		button \
		switch \
		label 
	
	SECTION_ADD (> ($ brightness) 0.5 Theme_light Theme_dark)

SECTION Theme_light

	LET bg_color (CITRPL #808080ff (RGB 255 255 255) ($ brightness))

SECTION Theme_dark

	LET bg_color #00000000

SECTION

	ITERATE_RAW widgets % background_color ($ bg_color)
```

Result :

```
button background_color #ccccccff
switch background_color #ccccccff
label  background_color #ccccccff
```

### 6.2. Network Simulator

This second example shows off a possible simulation description/input file for some sort of wireless network simulator. Said simulation requires a list of communication nodes to be declared and set up. Here, a linear wireless network consisting of 5 nodes is created. Each node is then attributed a (simplified) mac address, an x-y position, and a random communication range. Thanks to the usage of variables and iterations, that same simulation can be scaled up to hundreds of nodes by just changing the `number_of_nodes` value.

```
LET         number_of_nodes 5
LET_ENUM    nodes_ids 1 ($ number_of_nodes)
LET_PREPEND nodes_names nodes_ids "n_"

node list ($ nodes_names)

ITERATE     nodes_ids (\ JOIN "n_" %) address %
ITERATE     nodes_ids (\ JOIN "n_" %) x (\ - (\ * % 10) 10)
ITERATE     nodes_names % y 0
ITERATE_RAW nodes_names % range (ROUND (RAND 15 35))
```

Result :

```
node list n_1 n_2 n_3 n_4 n_5

n_1 address 1
n_2 address 2
n_3 address 3
n_4 address 4
n_5 address 5

n_1 x 0
n_2 x 10
n_3 x 20
n_4 x 30
n_5 x 40

n_1 y 0
n_2 y 0
n_3 y 0
n_4 y 0
n_5 y 0

n_1 range 23
n_2 range 30
n_3 range 18
n_4 range 18
n_5 range 28
```

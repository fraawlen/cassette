# CCFG - Specification & Syntax

Cassette Configuration (CCFG) is a configuration language and parser library featuring array based values and short s-like expressions based functions. The language's syntax aims to be both human-readable and easy to parse. Yet provides enough tools to the end user to create branching and dynamic configurations that can be modified and reloaded on the fly.

This document specifies and details all of CCFG's features and syntax rules. It's aimed at both end-users of software that uses this language and developers of such software.

## Table of contents <a name="toc"></a>

1. [Fundamentals](#fundamentals)
	1. [Sequences](#sequences)
	2. [Tokens](#tokens)
	3. [Resources](#resources)
2. [Tokens Types](#tokens-types)
	1. [Invalids](#invalids)
	2. [Strings](#strings)
	3. [Numerals](#numerals)
	4. [Functions](#functions)
3. [Sequences Leads](#leads)
	1. [Sections](#section)
	2. [Section Additions](#section-add)
	3. [Section Deletions](#section-del)
	4. [Variable Declaration](#let)
	5. [Enumeration Declaration](#enum)
	6. [Combination of Variables](#combine)
	7. [Iterations](#iterate)
	8. [Child File Inclusion](#include)
	9. [Seed Override](#seed)
	10. [Debug Print](#debug)
4. [Substitutions](#substitutions)
	1. [Comments / End-of-Sequence](#eos)
	2. [Fillers](#filler)
	3. [End-of-File](#eof)
	4. [Escape](#esc)
	5. [Variable Injection](#var)
	6. [Parameter Injection](#param)
	7. [Iteration Injection](#iter)
	8. [Join](#join)
	9. [Conditions](#if)
	10. [Math Operations](#math)
	11. [Color Operations](#color)
	12. [Constants](#const)
	13. [Timestamp](#time)
	14. [Random value](#rand)
5. [Full examples](#examples)
	1. [GUI Configuration](#gui-conf)
	2. [Network Simulator](#net-sim)

## 1. Fundamentals <a name="fundamentals"></a>

<div align="right">[ <a href="#toc">back to top</a> ]</div>

### 1.1. Sequences <a name="sequences"></a>

Every CCFG configuration file is a series of 'sequences' separated by newlines until EOF is reached. Empty lines between sequences are ignored. Leading and trailing white-spaces (either space or tab characters) are ignored too.

```
sequence
sequence
sequence
sequence
```

<div align="right">[ <a href="#toc">back to top</a> ]</div>

### 1.2. Tokens <a name="tokens"></a>

Sequences themselves are a series of 'tokens'. Tokens are 256 bytes words, null terminator included, encoded in ASCII / UTF-8, separated by any amount of white space in-between. If the word's length exceeds 255 bytes, only the first 255 bytes are kept and a null terminator is appended at the 256th byte.

```
token token token
token token
token token token token token
token token token token
```

In CCFG, there is no concept of priority based on the presence of parentheses, brackets, or braces. In fact, parentheses get interpreted as white space, so even if they can be used to visually group tokens together, they bear no effect. Instead, tokens are strictly parsed from left to right and any language function that gets triggered should happen as the tokens are read. Thanks to that, words read from a source configuration file only need to be parsed and processed once. The only exception to this rule is the [iteration-type sequences](#iterate).

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
<div align="right">[ <a href="#toc">back to top</a> ]</div>

### 1.3. Resources <a name="resources"></a>

Sequences that make up a program's configuration are called resources. The first token represents the resource's namespace and the second one is the resource's name. Namespaces help reuse resource names without collisions. The tokens that follow until the end of the sequence are interpreted as values attributed to the resource. Resources definitions with no values are ignored. Therefore, valid resources are sequences of at least 3 tokens. There are no upper token count limit (apart from the system's memory). If the same resource is defined more than once, the latest definition overwrites the previous ones.

```
namespace name value_1 value_2 value_3 ...
```

If a program requires a resource with N values and the matching resource in the configuration file has less than N values defined, the interpretation of missing values is up to the program.

By convention, and to minimize possible collision with function tokens, resources tokens (namespaces, names and values) should be written in full lowercase.

At its simplest, a CCFG configuration file can just be a series of resources. In this example, for a hypothetical GUI library, we define 8 resources spread across 2 widgets, `button` and `label`. The resources namespaces borrow their widget name. The resources named `corner-radius` and `corner-style` get assigned 4 values, one for each widget corner. 

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

<div align="right">[ <a href="#toc">back to top</a> ]</div>

## 2. Tokens Types <a name="tokens-types"></a>

 When parsed, tokens get converted into one of the following 4 types:

- invalid
- string
- numeral
- function

<div align="right">[ <a href="#toc">back to top</a> ]</div>

### 2.1. Invalids <a name="invalids"></a>

If any error during token parsing happens, like a failed conversion, wrong or missing function parameters, or a system error, the token will be marked as invalid. If an invalid type token is encountered in the middle of a sequence, the sequence is ended prematurely and the following tokens are skipped until a new sequence begins.

<div align="right">[ <a href="#toc">back to top</a> ]</div>

### 2.2. Strings <a name="strings"></a>

String tokens are the default token type, as they are just a text value that represents a resource namespace, resource name, or general purpose value with no language function. 

<div align="right">[ <a href="#toc">back to top</a> ]</div>

### 2.3. Numerals <a name="numerals"></a>

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

<div align="right">[ <a href="#toc">back to top</a> ]</div>

### 2.4. Functions <a name="functions"></a>

Function tokens are defined by an exact, case-sensitive ASCII sequence of non-white-space characters. As soon as they're read, they should trigger a language function. If the function fails due to invalid input parameters or internal memory issues, an invalid token is returned. Functions tokens can be separated into two sub-types: transformation tokens and sequences leads.

By convention, and to minimize possible collision with user strings, all function tokens are defined in fully capitalized characters.

Substitution tokens are inline function tokens that may optionally take the next few tokens as input parameters, and then return a different token as a function result. These functions feature an s-like expression syntax without parenthesis. Parenthesis in CCFG functions are unnecessary because the amount of input parameters for each function is known in advance. Multiple substitutions tokens can be chained together, resulting in nested functions. Some functions may take string or numeral tokens as inputs, it is up to the parser to make the necessary implicit conversions. In the next few examples, the [`JOIN`](#join) function tokens take the next 2 string tokens and return a concatenated string token.

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

Sequence lead tokens are special function tokens that define the role and type of a given sequence. As their name implies they are always the first token in a sequence. If they are placed elsewhere they have no effect. However, if the first token in a sequence is a transformation token, and this token returns a valid sequence lead token, then the sequence lead may still be processed normally. But if the first token, after transformation, is not a sequence lead nor is invalid, the started sequence will be then treated as resource definition. The following examples demonstrate the applications of the [`INCLUDE`](include) sequence lead. When valid, the remaining tokens in the sequence (post-transformation) are interpreted as paths to child configuration files.

```
INCLUDE path1 path2
-> open and parse file at path1 then do the same for path2

a INCLUDE path1 path2
-> not an inclusion because INCLUDE is not first, instead the sequence is now a resource definition with a resource namespace "a", resource name "INCLUDE", and assigned resource values "path_1" and "path_2"
```

<div align="right">[ <a href="#toc">back to top</a> ]</div>

## 3. Sequences Leads <a name="leads"></a>

<div align="right">[ <a href="#toc">back to top</a> ]</div>

### 3.1. Sections <a name="section"></a>

```
SECTION [section_name] [section_name] ...
```

Sequences can be grouped into user-defined sections. Sections can be then selectively parsed. When a section is not enabled, all sequences following the section definition will be skipped until a new section starts.

A section sequence is defined with a `SECTION` lead token, followed by section names. All section names within a section definition need to be enabled beforehand with [`SECTION_ADD`](#section-add), otherwhise the given section is disabled. Sections can be repeated. By convention, to differentiate section names from function tokens and generic strings or values, section names should capitalize their first character.

Sequences defined before the first section (if any) will always be read and processed. Likewise, if a section definition does not have any section name following it, the sequences following it will always be processed.

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

<div align="right">[ <a href="#toc">back to top</a> ]</div>

### 3.2. Section Additions <a name="section-add"></a>

```
SECTION_ADD [section_name] [section_name] ...
```

Sequences belonging to a section will only be parsed and processed if said section has been enabled. This can be done with a sequence starting with `SECTION_ADD` followed by any number of tokens. These tokens represent the names of sections to enable.

<div align="right">[ <a href="#toc">back to top</a> ]</div>

### 3.3. Section Deletions <a name="section-del"></a>

```
SECTION_DEL [section_name] [section_name] ...
```

Sequences belonging to a section will only be parsed and processed if said section has been enabled. But after having been enabled, sections can be disabled again with a sequence starting with `SECTION_DEL` followed by any number of tokens. These tokens represent the names of sections to disable. The effects of this sequence are only applicable after the present section ends.

<div align="right">[ <a href="#toc">back to top</a> ]</div>

### 3.4. Variable Declaration <a name="let"></a>

```
LET [new_variable_name] [value] [value] ...
```

The CCFG configuration language offers support for user-defined variables. Similarly to resources, variables are arrays of values. They can be declared with a sequence starting with the token `LET` followed by the name of the variable (that will be used to reference it in other parts of the file), then by any number of tokens. After that, when a variable is invoked in another sequence with [`$`](#var) or [`VAR`](var) the variable values will be injected into that sequence.

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

Finally, if the same variable is declared more than once, the latest declaration overwrites the previous ones.

```
LET var a b c d
LET var h i j
$ var
-> h i j
```

<div align="right">[ <a href="#toc">back to top</a> ]</div>

### 3.5. Enumerations Declaration <a name="enum"></a>

```
LET_ENUM [new_variable_name] [min] [max] [steps] [precision]
LET_ENUM [new_variable_name] [min] [max] [steps]
LET_ENUM [new_variable_name] [min] [max]
LET_ENUM [new_variable_name] [max]
```

Enumerations are variables whose values are not written down strings but instead are a sequence of numbers generated according to 4 parameters:

- min : value to start from
- max : value to end at
- steps : amount of intervals between min and max
- precision : the amount of decimals, because numbers are stored as doubles

The `min`, `steps` and `precision` tokens are optional. If ommited they will assume the following default values:

- min = 0.0
- steps = max - min
- precision = 0

```
LET_ENUM enum_1 5
-> 0 1 2 3 4 5

LET_ENUM enum_2 3 5
-> 3 4 5

LET_ENUM enum_3 0 6 3
-> 0 2 4 6

LET_ENUM enum_4 1 5 8 1
-> 1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0 
```

<div align="right">[ <a href="#toc">back to top</a> ]</div>

### 3.6. Combination of variables <a name="combine"></a>

```
LET_APPEND  [new_variable_name] [variable_name] [string]
LET_PREPEND [new_variable_name] [variable_name] [string]
LET_MERGE   [new_variable_name] [variable_name] [variable_name]
```

Append or prepend a string to each value of a given variable or merge the values of two variables. Then save the resulting token into a new variable.

<div align="right">[ <a href="#toc">back to top</a> ]</div>

### 3.7. Iterations <a name="iterate"></a>

```
FOR_EACH [variable_name] [alt_variable_name]
	sequence
	sequence
	...
FOR_END
```

Creates a for-loop that iterates through each token of the given variable. Inside the loop, the current iteration token can be accessed with the [`%`](#var) or [`ITER`](var) substitution tokens followed by the `alt_variable_name`. `alt_variable_name` is an optional token. If not provided, its value would be assumed to be the same as the given variable name. Loops can be nested. To close a loop, a sequence with a `FOR_END` leading token needs to be added; otherwise, the parser will assume the loop lasts until the end of the file or the parent loop.

```
LET var a b c d
FOR_EACH var
	% var
FOR_END
-> a
   b
   c
   d
```

<div align="right">[ <a href="#toc">back to top</a> ]</div>

### 3.8. Child File Inclusion <a name="include"></a>

```
INCLUDE [filename] [filename] ...
```

Alongside sections, resources can also be split into separate files that can be opened and parsed with an inclusion sequence that starts with an `INCLUDE` token followed by filenames of files to include. The filenames are relative to the location of the file the sequence is read from. Unless they start with '/' in which case the given filename are absolute.

The contents of the included files are treated as if they've been copy-pasted into the parent file at the position of the inclusion sequence. In other words, declared variables, enumerations, and section states are carried over to the included child file. And any modifications that happen in inclusions are also brought back to the parent file.

For example, the following 3 files (respectively : `~/.config/parent`, `~/child_1` and `/child_2`) :

```
SECTION_ADD Sa
INCLUDE ../child_1 /child_2
1 2 3 $ var
```
```
SECTION Sa
```
```
LET var a b c
```

Are equivalent to a single file like this :

```
SECTION_ADD Sa
SECTION Sa
	LET var a b c
	1 2 3 $ var
```

That gets resolved into :
```
1 2 3 a b c
```

<div align="right">[ <a href="#toc">back to top</a> ]</div>

### 3.9. Seed Override <a name="seed"></a>

```
SEED_OVERRIDE [seed_value]
```

Resets and sets the seed of the parser's random number LCG. Can be used to shuffle the values returned by [`RAND`](#rand). 

<div align="right">[ <a href="#toc">back to top</a> ]</div>

### 3.10. Debug Print <a name="debug"></a>

```
DEBUG_PRINT [token] [token] ...
```

Prints to `stderr` the resolved sequence that follows the lead token.

<div align="right">[ <a href="#toc">back to top</a> ]</div>

## 4. Substitutions <a name="substitutions"></a>

<div align="right">[ <a href="#toc">back to top</a> ]</div>

### 4.1. Comments / End-of-Sequence <a name="eos"></a>

```
//
EOS
```

Any tokens written after this token will be ignored until the next newline.

It should be noted that this is a token, therefore there should be some white space before the start of the comment text.

```
a b EOS proper comment
a b // proper comment
a b //not a comment
```

<div align="right">[ <a href="#toc">back to top</a> ]</div>

### 4.2. Fillers <a name="filler"></a>

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

<div align="right">[ <a href="#toc">back to top</a> ]</div>

### 4.3. End-of-File <a name="eof"></a>

```
EOF
```

Ends a sequence and file early. If the file this token is read from happens to be an [included file](#include), only that file gets closed, and parsing resumes in the parent file.

<div align="right">[ <a href="#toc">back to top</a> ]</div>

### 4.4. Escape <a name="esc"></a>

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

<div align="right">[ <a href="#toc">back to top</a> ]</div>

### 4.5. Variable Injection <a name="var"></a>

```
$   [variable_name]
VAR [variable_name]
```

Injects a variable's sequence of values into the current sequence. The variable being called needs to be declared first with a sequence starting with [`LET`](#let), [`LET_ENUM`](#enum), [`LET_APPEND`](#combine), [`LET_PREPEND`](#combine), or [`LET_MERGE`](#combine). If the variable was not declared beforehand, an invalid token will be returned instead.

```
LET var a b c
1 2 $ var 2
-> 1 2 a b c 3
```

<div align="right">[ <a href="#toc">back to top</a> ]</div>

### 4.6. Parameter Injection <a name="param"></a>

```
$$    [parameter_name]
PARAM [parameter_name]
```

Returns the value of a parameter defined within the calling program. If the parameter is not declared within the calling program, an invalid token will be returned instead.

```c
/* inside caller program */
ccfg_push_param(cfg, "param_a", "abc");
```

```
$$ param_a
-> abc

$$ param_b
-> invalid, not defined in the calling program
```

<div align="right">[ <a href="#toc">back to top</a> ]</div>

### 4.7. Iteration Injection <a name="iter"></a>

```
%
ITER
```

Within an iterated sequence, this token returns a value of the iteration's variable. Outside iterations this token is treated as a string. See [Iterations](#iterate) for more details.

<div align="right">[ <a href="#toc">back to top</a> ]</div>

### 4.8. Join <a name="join"></a>

```
JOIN [string] [string]
```

Concatenates the next 2 tokens and returns the resulting string.

```
JOIN a b
-> ab
```

<div align="right">[ <a href="#toc">back to top</a> ]</div>

### 4.9. Conditions <a name="if"></a>

```
<     [double] [double] [token_if_true] [token_if_false]
<=    [doudle] [double] [token_if_true] [token_if_false]
>     [double] [double] [token_if_true] [token_if_false]
>=    [double] [double] [token_if_true] [token_if_false]
==    [double] [double] [token_if_true] [token_if_false]
!=    [double] [double] [token_if_true] [token_if_false]
STREQ [string] [string] [token_if_true] [token_if_false]
```

Compares two doubles or two strings. Depending on the result, either the 3rd or 4th token is returned.

```
!= 0.3 0.5 a b
-> a
```

Only one token is returned at a time. But conditionals, being substitutions, can be combined with [variables](#var) if a multi-token sequence needs to be returned :

```
LET var_a a b c
LET var_b 1 2 3
$ == 0.0 1.0 var_a var_b
-> ($ (== 0.0 1.0 var_a var_b))
-> ($ var_b)
-> 1 2 3
```

<div align="right">[ <a href="#toc">back to top</a> ]</div>

### 4.10. Math Operations <a name="math"></a>

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
ITRPL [double] [double] [0.0-1.0] // Linear interpolation
LIMIT [double] [min] [max]
```

Performs a mathematical operation and returns the resulting double value.

<div align="right">[ <a href="#toc">back to top</a> ]</div>

### 4.11. Color Operations <a name="color"></a>

```
CITRPL [color] [color] [0.0-1.0]       // Linear color interpolation
RGB    [0-255] [0-255] [0-255]         // Composes a color
RGBA   [0-255] [0-255] [0-255] [0-255] // Composes a color
```

Performs color specific operations and returns a double value representing a color. Since colors are [internally represented as doubles](#numerals), normal [mathematical operations](#math) can also be used. But unlike 'normal' math, this set of substitutions perform operations on each color channel individually.

<div align="right">[ <a href="#toc">back to top</a> ]</div>

### 4.12. Constants <a name="const"></a>

```
PI    // 3.1415926535897932
E     // 0.5772156649015328
TRUE  // 1.0
FALSE // 0.0
```

Returns a constant value in double format.

<div align="right">[ <a href="#toc">back to top</a> ]</div>

### 4.13. Timestamp <a name="time"></a>

```
TIME
```

Returns a UNIX timestamp in seconds.

<div align="right">[ <a href="#toc">back to top</a> ]</div>

### 4.14. Random value <a name="rand"></a>

```
RAND [min] [max]
```

Returns a pseudo-random double ranging from `min` to `max`.
Because the internal RNG is an LCG, as long as the initial seed is not modified (either by the caller program or with [`SEED_OVERRIDE`](#seed)), the same value will be returned between different reloads.

<div align="right">[ <a href="#toc">back to top</a> ]</div>

## 5. Full Examples <a name="examples"></a>

<div align="right">[ <a href="#toc">back to top</a> ]</div>

### 5.1. GUI Configuration <a name="gui-conf"></a>

In this first example, we consider a hypothetical GUI tooklit or application that lets its end-users customize the appearance of its widgets. Thanks to CCFG's sections, different themes can co-exist in the same source file. Moreover, the themes are selected automatically depending on the value of a brightness variable. Said variable could be then set by an external program that tracks the values of a light sensor. Hence, with this kind of configuration, an application or toolkit can switch between a light and dark theme depending on the amount of light hitting the device they're running from.

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

	FOR_EACH widgets
		(% widgets) background_color ($ bg_color)
	FOR_END
```

Resolved resources :

```
button background_color #ccccccff
switch background_color #ccccccff
label  background_color #ccccccff
```

<div align="right">[ <a href="#toc">back to top</a> ]</div>

### 5.2. Network Simulator <a name="net-sim"></a>

This second example shows off a possible simulation description/input file for some sort of wireless network simulator. Said simulation requires a list of communication nodes to be declared and set up. Here, a linear wireless network consisting of 5 nodes is created. Each node is then attributed a (simplified) mac address, an x-y position, and a random communication range. Thanks to the usage of variables and iterations, that same simulation can be scaled up to hundreds of nodes by just changing the `number_of_nodes` value.

```
LET         number_of_nodes 5
LET_ENUM    nodes_ids 1 ($ number_of_nodes)
LET_PREPEND nodes_names nodes_ids "n_"

node list ($ nodes_names)

FOR_EACH nodes_ids id
	(JOIN "n_" (% id)) address (% id)	
	(JOIN "n_" (% id)) x       (- (* (% id) 10) 10)	
	(JOIN "n_" (% id)) y       0	
	(JOIN "n_" (% id)) range   (ROUND (RAND 15 35))
FOR_END
```

Resolved resources :

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

<div align="right">[ <a href="#toc">back to top</a> ]</div>

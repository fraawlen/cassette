
![Derelict Objects banner](./extras/banner.png)

Derelict Objects (DO) is a little collection self-contained data structures. Its API is written in a (somewhat) safe C style in which all structures that depend on dynamic memory allocation are opaque and their handler functions are designed to minimize the return of null pointer values. In other words, save for a few explicit exceptions, functions including constructors always return valid values or pointers, even in case of memory allocation failure. Moreover, destructor functions are also protected from double-free and dandling pointers.

The library is free and open-source software licensed under the [LGPL-2.1](https://www.gnu.org/licenses/old-licenses/lgpl-2.1.html). It's made to run on modern POSIX-compliant systems, and except for the compiler and build system, is not dependent on third-party software.

Features
--------------------

- Book : a dynamic array/vector for C strings with grouping features
- Dictionary : an hashmap with string + group keys, FNV-1A hashing and linear probing
- Tracker : a hybrid vector/stack or pointers used to keep track of instanced components.
- String : UTF-8 strings with 2D (rows and columns) information and manipulation functions
- Color : RGBA color representation, manipulation and conversion
- Rand : a re-implementation of POSIX's rand48 functions with a slightly more convenient API

Dependencies
------------

Tools :

- C99 compiler with a stdlib + POSIX 200809L
- Make
- xxd

Installation
------------

First, edit the makefile if you want to change the installation destinations. These are represented by the variables `DEST_HEADERS` and `DEST_LIBS` for the public API headers and library files respectively. By default, they are set to `/usr/include/derelict/` and `/usr/lib`.
Then, build and install DO with the following commands :

```
make
make install
```

After these steps, both a shared binary and static archive will be generated and installed on your system. Examples will also be built and placed under `./build/bin`. The examples are statically compiled and can be run from anywhere on your system.

Usage
-----

Add this include to get access to the library functions :

```
#include <derelict/do.h>
```

As well as this compilation flag :

```
-ldo
```

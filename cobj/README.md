<p align=center><img src="banner.svg"></p>

Cassette Objects (COBJ) is a little collection self-contained data structures. Its API is written in a (somewhat) safe C style in which all structures that depend on dynamic memory allocation are opaque and their handler functions are designed to minimize the return of null pointer values. In other words, save for a few explicit exceptions, functions including constructors always return valid values or pointers, even in case of memory allocation failure. 

## Objects

Because each COBJ's object is self-contained, unlike CCFG and CGUI, each object gets its own namespace.

| Object  | Description                                                                       |
| ------- | --------------------------------------------------------------------------------- |
| cbook   | dynamic C-strings stack with grouping features                                    |
| ccolor  | RGBA color representation, manipulation and conversion                            |
| cdict   | hashmap with string + group keys, FNV-1A hashing and linear probing               |
| cerr    | error codes used by every Cassette component                                      |
| cinputs | 2D input (screen touches, key / button presses) tracker array                     |
| crand   | re-implementation of POSIX's rand48 functions with a slightly more convenient API |
| cref    | reference counter used to keep track of instanced components                      |
| csafe   | set of arithmetics operations on size_t with overflow and underflow protection    |
| cseg    | 1D segment represenation and manipulation with bound checks and UB prevention     |
| cstr    | UTF-8 strings with 2D (rows, columns, tabsize, wrapping) features                 |

## Usage

Add this include to get access to all of the library features :

```
#include <cassette/cobj.h>
```

If you want to be more explicit, you can include the specific headers you need :

```
#include <cassette/cbook.h>
#include <cassette/ccolor.h>
#include <cassette/cdict.h>
#include <cassette/cerr.h>
#include <cassette/crand.h>
#include <cassette/cref.h>
#include <cassette/csafe.h>
#include <cassette/cseg.h>
#include <cassette/cstr.h>
```

Then, to compile your program, add this flag :

```
-lcobj
```

Check out the `examples` directory for demonstrations.

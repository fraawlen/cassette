Derelict Objects
================

Gist
----

Derelict Objects (DO) is a collection of data structures and convenience functions for the [Derelict Graphics (DG)](https://codeberg.org/fraawlen/derelict-graphics) library as well as other projects part of the Derelict ecosystem. This library is far from being exhaustive but it is incrementally updated with new features when they are needed by DO dependent projects. If you wish to use this library but need a specific feature related to the existing data structures, feel free to create a feature request.

Currently provided components:

- Book : a fancy dynamic array / vector for c-strings
- Dictionary : a string-group-value hashmap
- Tracker : a hybrid pointer vector / stack data structure to keep track of instanced components.
- String : a 2D UTF-8 C-string wrapper with simple to use string manipulation methods
- Color : RGBA color representation, manipulation and conversion

Setup
-----

### Requirements

All that's needed to build DO are the following tools :

- C99 compiler with a stdlib + POSIX 200809L
- Make

### Build & Installation

Once you have met all the necessary requirements, you can build and install DO by simply running  :
```
make lib
make install
```
After these steps, both a shared binary and static archive will be generated and installed on your system. By default, headers and library binaries will be respectively placed in `/usr/derelict/`  and `/usr/lib`. If you want to install them elsewhere modify the `DEST_HEADERS` and `DEST_LIBS` makefile variables.

### Demos

The DO project also include a few demos programs that showcase different use-cases and capabilities of the library.
You can build them with the following command (you will need to build the library first, but installation is not necessary) :
```
make examples
```
The resulting executable binaries will be placed in `build/bin`.

Usage
-----

Simply add the following include to use all that the library has to offer :
```
#include <derelict/do.h>
```
Then add this compilation flag to link your project to DO :
```
-ldo
```

License
-------

[LGPL-2.1 License](https://www.gnu.org/licenses/old-licenses/lgpl-2.1.html)

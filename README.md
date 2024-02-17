Derelict Utilities
==================

Gist
----

Derelict Utilities (DU) is a collection of data structures and convenience functions for the [Derelict Graphics (DG)](https://codeberg.org/fraawlen/derelict-graphics) library as well as other projects part of the Derelict ecosystem. This library is far from being exhaustive but it is incrementally updated with new features when they are needed by DU dependent projects. If you wish to use this library but need a specific feature related to the existing data structures, feel free to create a feature request.

Currently provided components:

- Book : a fancy dynamic array / vector for c-strings
- Dictionary : a string-group-value hashmap
- Tracker : a hybrid pointer vector / stack data structure to keep track of instanced components.
- String : a 2D UTF-8 C-string wrapper with simple to use string manipulation methods
- Color : RGBA color representation, manipulation and conversion
- Input : similar to Trackers but dedicated to end-user input tracking (screen touches, mouse buttons, ...)
- Some extra small miscellaneous functions and types

Status
------

Latest release : **n/a**

This library was initialy embeded within DG < 0.2.0. However, to better separate the actual GUI components from internal data structures, and to re-use said data structures in other not necessarily GUI-related projects, it has been split into its own library.

Complete documentation is currently being written and should be published soon. In the meantime, consult the headers. All functions, structures, and behaviors are fully commented.

Setup
-----

### Requirements

All that's needed to build DU are the following tools :

- C99 compiler with a stdlib + POSIX 200809L
- Make

### Build & Installation

Once you have met all the necessary requirements, you can build and install DU by simply running  :
```
make
make install
```
After these steps, both a shared binary and static archive will be generated and installed on your system. By default, headers and library binaries will be respectively placed in `/usr/derelict/`  and `/usr/lib`. If you want to install them elsewhere modify the `DEST_HEADERS` and `DEST_LIBS` makefile variables.

### Demos

The DU project also include a few demos programs that showcase different use-cases and capabilities of the library.
You can build them with the following command (you will need to build the library first, but installation is not necessary) :
```
make demos
```
The resulting executable binaries will be placed in `build/bin`.

### Clean-up

Once you're done with everything, you can remove all build files with :
```
make clean
```

Usage
-----

Simply add the following include to use all that the library has to offer :
```
#include <derelict/du.h>
```
Then add this compilation flag to link your project to DU :
```
-ldu
```

License
-------

[LGPL-2.1 License](https://www.gnu.org/licenses/old-licenses/lgpl-2.1.html)

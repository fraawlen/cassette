Derelict Resources
==================

Gist
----

Derelict Resources (DR) is a configuration language that aim to be simple to both read and parse but still provide enough tools to the end user to create complex configurations schematics.

TODO better description

Status
------

Latest release : **n/a**

Setup
-----

### Requirements

To build DR you need the following tools :

- C99 compiler with a stdlib + POSIX 200809L
- Make

As well as the following first-party library :

- [Derelict-Utilities (DU)](https://codeberg.org/fraawlen/derelict-utilities)

### Build & Installation

Once you have met all the necessary requirements, you can build and install DR by simply running  :
```
make
make install
```
After these steps, both a shared binary and static archive will be generated and installed on your system. By default, headers and library binaries will be respectively placed in `/usr/derelict/`  and `/usr/lib`. If you want to install them elsewhere modify the `DEST_HEADERS` and `DEST_LIBS` makefile variables.

Usage
-----

Simply add the following include to use all that the library has to offer :
```
#include <derelict/dr.h>
```
Then add this compilation flag to link your project to DU :
```
-ldr
```

License
-------

[LGPL-2.1 License](https://www.gnu.org/licenses/old-licenses/lgpl-2.1.html)

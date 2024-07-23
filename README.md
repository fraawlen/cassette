<p align="center"><img src="./extras/banner.svg"></p>

Cassette Ada (CADA) is a set of first-party thick Ada bindings to Cassette libraries.

The library is free and open-source software licensed under the [LGPL-2.1](https://www.gnu.org/licenses/old-licenses/lgpl-2.1.html). It's made to run on modern POSIX-compliant systems, and except for the compiler and build system, is not dependent on third-party software.

Dependencies
------------

- Tools :

	- Ada 2012 compiler
	- Gprbuild
	- Xxd (optional, is only needed to build examples)

- First-party libraries :

	- [Cassette-Objects (COBJ)](https://codeberg.org/fraawlen/cassette-objects)
	- [Cassette-Configuration (CCFG)](https://codeberg.org/fraawlen/cassette-configuration)
	- [Cassette-Graphics (CGUI)](https://codeberg.org/fraawlen/cassette-graphics)

Installation
------------

```
gprbuild cassette.gpr
gprbuild examples.gpr
```

Mirrors
-------

- https://codeberg.org/fraawlen/cassette-ada
- https://github.com/fraawlen/cassette-ada


![Derelict Resources banner](./extras/banner.png)

Derelict Resources (DR) is a configuration language and parser library featuring array based values and short s-like expressions based functions. The language's syntax aims to be both human-readable and easy to parse. Yet provides enough tools to the end user to create branching and dynamic configurations that can be modified and reloaded on the fly.

The library is free and open-source software licensed under the [LGPL-2.1](https://www.gnu.org/licenses/old-licenses/lgpl-2.1.html). It's made to run on modern POSIX-compliant systems, and except for the compiler and build system, is not dependent on third-party software.

Language Features
-----------------

- comments
- variables
- user-defined sections
- arithmetic operations
- string operations
- color operations
- iteration loops
- conditionals
- child file inclusion

For more information about the language usage, features and syntax check out the [full language spec](./doc/spec.md).

Dependencies
------------

Tools :

- C99 compiler with a stdlib + POSIX 200809L
- Make

First-party libraries :

- [Derelict-Utilities (DU)](https://codeberg.org/fraawlen/derelict-utilities)

Installation
------------

First, edit the makefile if you want to change the installation destinations. These are represented by the variables `DEST_HEADERS` and `DEST_LIBS` for the public API headers and library files respectively. By default, they are set to `/usr/derelict/` and `/usr/lib`.
Then, build and install DR with the following commands :

```
make
make install
```

After these steps, both a shared binary and static archive will be generated and installed on your system.

Usage
-----

Add the this include to get access to the library functions :
```
#include <derelict/dr.h>
```
As well as this compilation flag :
```
-ldr
```

Minimal Example
-------

The following code snippet shows a minimal example of the library usage. When compiled and run, it will look for the file `/tmp/dr-example` and load its data if it can. It then attempts to fetch a resource named `example_property` under the namespace `example_namespace` and load a single string value into a buffer.

```c
#include <derelict/dr.h>

int
main(void)
{
    dr_config_t *cfg = dr_config_create(0);
    dr_config_push_source(cfg, "/tmp/dr-example");
    dr_config_load(cfg);

    char value[32];
    if (dr_config_find_resource(cfg, "example_namespace", "example_property", val, 1, 32)) {
        /* resource successfully fetched, its value is stored as a c-string in 'val' */
    } else {
        /* resource not found */
    }
}
```

A matching minimal DR configuration in `/tmp/dr-example` will then look like this :

```
example_namespace example_property value
```

Check out the `examples` directory for more in depth demonstrations.

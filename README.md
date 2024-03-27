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

- [Derelict-Objects (DO)](https://codeberg.org/fraawlen/derelict-objects)

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

The following code snippet shows a minimal example of the library usage. When compiled and run, it will look for the file `/tmp/dr-example` and load its data. It then attempts to fetch a resource named `property` under the namespace `namespace` and load 2 string values into a buffer to be printed out.

```c

#include <stdio.h>
#include <stdlib.h>
#include <derelict/dr.h>

#define MAX_VALUES 2
#define VALUE_LEN  32

int
main(void)
{
	dr_config_t *cfg;
	char values[MAX_VALUES][VALUE_LEN];
	size_t n;

	cfg = dr_config_create(0);
	dr_config_push_source(cfg, "/tmp/dr-example");
	dr_config_load(cfg);

	n = dr_config_find(cfg, "namespace", "property", *values, MAX_VALUES, VALUE_LEN);
	if (n > 0)
	{
		for (size_i = 0; i < n; i++)
		{
			printf("%s\t", values[i]);
		}
	}
	else
	{
		/* resource not found */
	}
}
```

A matching minimal DR configuration in `/tmp/dr-example` will then look like this :

```
namespace property value_A value_B
```

Output when run:

```
value_A	value_B
```

Check out the `examples` directory for more in depth demonstrations.

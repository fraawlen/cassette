<p align="center"><img src="./extras/banner.svg"></p>


Cassette Ada (CADA) is a set of first-party thick Ada bindings to Cassette libraries. Example programs programs have also been ported to Ada. The library is free and open-source software licensed under the [LGPL-2.1](https://www.gnu.org/licenses/old-licenses/lgpl-2.1.html). It's made to run on modern POSIX-compliant systems.

Notice
------

Only a few Cassette libraries have been ported so far (COBJ and CCFG). No bindings are currently provided for CGUI, which is undergoing a rewrite.

Features
--------

### Packaging

You no longer need to prefix every type, function, or macro with the library's name, including the 'c' for 'Cassette'. Instead, each component is now namespaced into its package with a root package called `Cassette`. This package serves as the central point for using the `use` Ada clause when working with Cassette libraries, and it defines a few common types used throughout the child packages. Previously, error codes and types were defined in a separate header called `cerr.h`; now, they are included inside the parent `Cassette` package. And the `cobj.h` meta header goes away. `ccfg` and `cgui` namespaces have also been renamed into `Cassette.Config` and `Cassette.Graphics`.

![package tree](./extras/packages.svg)

### Types

All C native types are either typed/subtyped for their specific applications, sometimes with extra range limitations. All C NUL terminated strings variables, inputs or outputs are converted into Ada's native strings. Access types and System.Address variables are avoided in favour of in/out parameter modes.

### Encapsulation

All objects are encapsulated into Ada's tagged records. Objects without dynamic memory allocation, and therefore with a public, non-opaque definition, get their C struct equivalent hidden from public view. Opaque handles are also encapsulated in limited records. Furthermore, since these records can hold default values, C's *_PLACEHOLDER macros are not needed anymore. And constructor functions do not return the object. Instead, they take it as a tagged argument.

Also, because of Ada's package semantics, to avoid repeating oneself when declaring a new object as a variable like this `Cassette.Dict.Dict` object's type is simply refereed as `T` within its package.

Thus, a dictionary definition and example in COBJ in C like so:

``` C
#typedef struct cdict cdict;
...
```

``` C
#include <cassette/cobj.h>

cdict d = CDICT_PLACHEHOLDER;

int
main(void)
{
	d = cdict_create();
	cdict_write(d, 0, 12);
	cdict_clear(d);
	cdict_destroy(d);
}
```

Becomes the following in Ada:

``` Ada
package Cassette.Dict is

	type T is tagged limited private;
	...
```

``` Ada
with Cassette.Dict; use Cassette;

procedure Main
is
	D : Dict.T;
begin

	D.Create;
	D.Write (0, 12);
	D.Clear;
	D.Destroy;

end Main;
```

### Exceptions

Instead of relying on explicit calls to *_error() in C to check if an object has errored after a method call, an exception is raised. Each object will carry its exception defined as `E` in its package if it can error at any time. Error methods are still available to check the exact error encountered.

Thus, this C code :

``` C
#include <cassette/cobj.h>

cdict d = CDICT_PLACHEHOLDER;

int
main(void)
{
	...
	
	cdict_prealloc(d, 123456);
	if (cdict_error(d) != CERR_NONE)
	{
		// Error handling
	}
	
	...
}
```

Becomes the following in Ada:

``` Ada
with Cassette.Dict; use Cassette;

procedure Main
is
	D : Dict.T;
begin

	...
	
	D.Prealloc (123465);
	
	...

exception

	when Dict.E =>
		-- Error handling, call D.Error to check the error code

end Main;
```

### Overloading and default values

Methods variants that do similar things but with different variables types, were 'overloaded' in their C11 implementation using  _Generic macros. In CADA, these methods are overloaded instead. Moreover, a few *_find() methods that took an optional index pointer argument that could be left to NULL are also overloaded to allow skipping that optional argument defined with an out parameter mode.

Thus, this C code :

``` C
#include <cassette/cobj.h>

cdict d = CDICT_PLACHEHOLDER;

int
main(void)
{
	size_t v;
	
	...
	
	if cdict_find(d, 0, 12, &v)
	{
		// Do something with v
 	}
	
	if cdict_find(d, 0, 12, NULL)
	{
		// Do something without v
	}
	
	...
}
```

Becomes the following in Ada:

``` Ada
with Cassette.Dict; use Cassette;

procedure Main
is
	D : Dict.T;
	V : Index -- Type definition in root Cassette package
begin

	...
	
	if (D.Find (0, 12, V))
	then
		-- Do something with V
	end if;
	
	if (D.Find (0, 12))
	then
		-- Do something without V
	end if;
	
	...

exception

	when Dict.E =>
		-- Error handling

end Main;
```

Dependencies
------------

- Tools :

	- Ada 2012 GNAT
	- [Gprbuild](https://github.com/AdaCore/gprbuild)

- First-party libraries :

	- [Cassette-Objects (COBJ)](https://codeberg.org/fraawlen/cassette-objects)
	- [Cassette-Configuration (CCFG)](https://codeberg.org/fraawlen/cassette-configuration)
	- [Cassette-Graphics (CGUI)](https://codeberg.org/fraawlen/cassette-graphics)

Build
-----

CADA bindings can be built with one Gprbuild command:

```
gprbuild cassette.gpr
```

The examples can be built using the following command. Ensure that COBJ, CCFG, and CGUI are already installed on your system.

```
gprbuild examples.gpr
```

Usage
-----


To use the Ada library, first, make sure to obtain a local copy of the repository. Then, in your Gprbuild project file, include the following clause: 

``` Ada
with ".../path_to_repo/cassette.gpr";
```

Next, add the following block inside your project definition: 

``` Ada
package Linker
is
	for Default_Switches ("Ada") use Cassette.Linker_Switches;
end Linker;
```

Finally, in your source files, add the relevant `with` and `use` clauses: 

``` Ada
with Cassette; use Cassette;
with Cassette.Dict;
with Cassette.Graphics;
with Cassette.Config;
...
```

Mirrors
-------

- https://codeberg.org/fraawlen/cassette-ada
- https://github.com/fraawlen/cassette-ada

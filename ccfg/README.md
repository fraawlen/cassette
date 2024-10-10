<p align="center"><img src="banner.svg"></p>

Cassette Configuration (CCFG) is a configuration language and parser library featuring array based values and short s-like expressions based functions. The language's syntax aims to be both human-readable and easy to parse. Yet provides enough tools to the end user to create branching and dynamic configurations that can be modified and reloaded on the fly.

## Usage

Add this include to get access to the library functions :

```
#include <cassette/ccfg.h>
```

As well as this compilation flag :

```
-lccfg
```

## Minimal Example

The following code snippet shows a minimal example of the library usage. When compiled and run, it will look for `/tmp/data.conf` or `data.conf` and load their contents. It then attempts to fetch a resource named `property` under the namespace `namespace`, and if found, prints its values.

```c
#include <stdio.h>
#include <cassette/ccfg.h>

int
main(void)
{
	ccfg *cfg = ccfg_create();

	ccfg_push_source(cfg, "/tmp/data.conf"); /* primary  source */
	ccfg_push_source(cfg,      "data.conf"); /* fallback source */
	ccfg_load(cfg);

	ccfg_fetch(cfg, "namespace", "property");
	while (ccfg_iterate(cfg))
	{
		printf("%s\n", ccfg_resource(cfg));
	}

	return 0;
}
```

A matching minimal CCFG configuration in `/tmp/data.conf` or `data.conf` will then look like this :

```
namespace property value_A value_B
```

Output :

```
value_A
value_B
```

Check out the `examples` directory for more in depth demonstrations and `include/cassette/*.h` header files for full functions descriptions. For more information about the language usage, features and syntax check out the [language spec](../docs/ccfg-spec.md).

## Fizz Buzz

Here's a Fizz Buzz example that prints the results to stderr using DEBUG_PRINT sequences. It's important to note that although it is possible, CCFG is not intended to be used for computation. Instead, all language features and functions have been created with dynamic configurations in mind.

```
LET_ENUM n 1 100
FOR_EACH n

	SECTION_DEL Fizz Buzz No_Fizz No_Buzz

	SECTION_ADD (== (MOD ((% n) 3) 0 Fizz No_Fizz)
	SECTION_ADD (== (MOD ((% n) 5) 0 Buzz No_Buzz)

	SECTION Fizz Buzz
		DEBUG_PRINT "Fizz Buzz"
	SECTION Fizz No_Buzz
		DEBUG_PRINT "Fizz"
	SECTION No_Fizz Buzz
		DEBUG_PRINT "Buzz"
	SECTION No_Fizz No_Buzz
		DEBUG_PRINT (% n)
	SECTION

FOR_END
```

## Fuzzing

This project comes with an integrated fuzz test case. First make sure to have AFL++ and the necessary utilities (afl-gcc-fast) installed. Then build and run it with the following command:

```
cd test
make
```

By default, CCFG will run without restrictions; thus, some language functions can generate hangs during fuzzing. CCFG provides a restricted mode ([Section 2.5](../docs/ccfg-spec.md)). In restricted mode, only resources definitions are valid, and all language functions are disabled. To run the fuzz test case in restricted mode, run the following command:

```
CCFG_RESTRICT= make
```

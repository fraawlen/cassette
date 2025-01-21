# CCFG Minimal Example

The following code snippet shows a minimal example of the library usage. When compiled and run, it will look for `/tmp/data.ccfg` or `data.ccfg` and load their contents. It then attempts to fetch a resource named `property` under the namespace `namespace`, and if found, prints its values.

```c
#include <cassette/ccfg.h>
#include <stdio.h>

int
main(void)
{
	ccfg *cfg = ccfg_create();

	ccfg_push_source(cfg, "/tmp/data.ccfg"); /* primary  source */
	ccfg_push_source(cfg,      "data.ccfg"); /* fallback source */
	ccfg_load(cfg);

	ccfg_fetch(cfg, "namespace", "property");
	while (ccfg_iterate(cfg))
	{
		printf("%s\n", ccfg_resource(cfg));
	}

	return 0;
}
```

A matching minimal CCFG configuration in `/tmp/data.ccfg` or `data.ccfg` will then look like this :

```
namespace property value_A value_B
```

Output :

```
value_A
value_B
```

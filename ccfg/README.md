CCFG comes with an integrated fuzz test case. First make sure to have AFL++ and the necessary utilities (afl-gcc-fast) installed. Then build and run it with the following command:

```
cd test
make
```

By default, CCFG will run without restrictions; thus, some language functions can generate hangs during fuzzing. CCFG provides a restricted mode ([Section 2.5](../docs/ccfg-language.md)). In restricted mode, only resources definitions are valid, and all language functions are disabled. To run the fuzz test case in restricted mode, run the following command:

```
CCFG_RESTRICT= make
```

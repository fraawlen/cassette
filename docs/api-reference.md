# API Reference

TODO - proper page generation.

In the meantime, check out the libraries headers directly, every function and type are fully described.

## CGUI

### Top level headers

| file                                                          | description                                |
|---------------------------------------------------------------|--------------------------------------------|
| [cgui.h](../cgui/include/cassette/cgui.h)                     | GUI session management and umbrella header |
| [cgui-clipboard.h](../cgui/include/cassette/cgui-clipboard.h) | copy & paste actions                       |
| [cgui-grid.h](../cgui/include/cassette/cgui-grid.h)           | WGC grid                                   |
| [cgui-screen.h](../cgui/include/cassette/cgui-screen.h)       | monitor and pointer information            |
| [cgui-window.h](../cgui/include/cassette/cgui-window.h)       | WGC window                                 |

### Top level headers - cells

| file                                                              | description                                |
|-------------------------------------------------------------------|--------------------------------------------|
| [cgui-beacon.h](../cgui/include/cassette/cgui-beacon.h)           | warning label that can light up and blink  |
| [cgui-button.h](../cgui/include/cassette/cgui-button.h)           | basic button with callback on click        |
| [cgui-filler.h](../cgui/include/cassette/cgui-filler.h)           | just a box, to fill empty space in layouts |
| [cgui-label.h](../cgui/include/cassette/cgui-label.h)             | basic static label                         |
| [cgui-gauge.h](../cgui/include/cassette/cgui-gauge.h)             | progress bar                               |
| [cgui-placeholder.h](../cgui/include/cassette/cgui-placeholder.h) | static box with a cross                    |
| [cgui-stripes.h](../cgui/include/cassette/cgui-stripes.h)         | static box with diagonal hazard stripes    |

### Low level headers

| file                                                            | description                            |
|-----------------------------------------------------------------|----------------------------------------|
| [cgui-attributes.h](../cgui/include/cassette/cgui-attributes.h) | GCC function attributes                |
| [cgui-box.h](../cgui/include/cassette/cgui-box.h)               | drawing primitive                      |
| [cgui-cell.h](../cgui/include/cassette/cgui-cell.h)             | WGC cell, api to create custom cells   |
| [cgui-config.h](../cgui/include/cassette/cgui-config.h)         | access to configuration and theme data |
| [cgui-event.h](../cgui/include/cassette/cgui-event.h)           | access to window and session events    |
| [cgui-swap.h](../cgui/include/cassette/cgui-swap.h)             | input swap map values definition       |
| [cgui-text.h](../cgui/include/cassette/cgui-text.h)             | drawing primitive                      |
| [cgui-types.h](../cgui/include/cassette/cgui-types.h)           | misc structs and enums                 |

## CCFG
 
| file                                      | description                                                      |
|-------------------------------------------|------------------------------------------------------------------|
| [ccfg.h](../ccfg/include/cassette/ccfg.h) | main and only header, CCFG parser instantiation and manipulation |

## COBJ

| file                                            | description                                                                       |
|-------------------------------------------------|-----------------------------------------------------------------------------------|
| [cbook.h](../cobj/include/cassette/cbook.h)     | dynamic C-strings stack with grouping features                                    |
| [ccolor.h](../cobj/include/cassette/ccolor.h)   | RGBA color representation, manipulation and conversion                            |
| [cdict.h](../cobj/include/cassette/cdict.h)     | hashmap with string + group keys, FNV-1A hashing and linear probing               |
| [cerr.h](../cobj/include/cassette/cerr.h)       | error codes used by every Cassette component                                      |
| [cinputs.h](../cobj/include/cassette/cinputs.h) | 2D input (screen touches, key / button presses) tracker array                     |
| [cobj.h](../cobj/include/cassette/cobj.h)       | umbrella header                                                                   |
| [crand.h](../cobj/include/cassette/crand.h)     | re-implementation of POSIX's rand48 functions with a slightly more convenient API |
| [cref.h](../cobj/include/cassette/cref.h)       | reference counter used to keep track of instanced components                      |
| [csafe.h](../cobj/include/cassette/csafe.h)     | set of arithmetics operations on size_t with overflow and underflow protection    |
| [cseg.h](../cobj/include/cassette/cseg.h)       | 1D segment represenation and manipulation with bound checks and UB prevention     |
| [cstr.h](../cobj/include/cassette/cstr.h)       | UTF-8 strings with 2D (rows, columns, tabsize, wrapping) features                 |


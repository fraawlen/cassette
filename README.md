<p align="center"><img src="extras/banner.svg"></p>

Cassette is a lightweight framework written in C11, with a UI inspired by the cassette-futurism aesthetic. Built for modern POSIX systems, it includes three main components: a custom GUI toolkit (CGUI), a configuration language and parser (CCFG), and a set of high-level data structures and utilities (COBJ). 

Cassette is free and open-source software, licensed under the [LGPL-3.0](https://www.gnu.org/licenses/lgpl-3.0.en.html).

> [!Warning]
> You are currently viewing the 'rewrite' branch which is preparing a new release version (0.2.0). It is non-functional yet. In the meantime, checkout the 'trunk' branch or the release tab, where there are usable builds of the framework. Cassette is still in early development, and some features are missing, but the releases builds are fully functional, albeit the next version will COMPLETELY CHANGE THE API.

> [!WARNING]
> Readme and documentation under construction.

> [!IMPORTANT]
> Rewrite progress:  
> COBJ : 100%, usable  
> CCFG : 100%, usable  
> CGUI :  70%, missing text drawing, widgets, copy-paste, popups, keyboard navigation

## Table of Contents <a name="toc"></a>

- [Libraries Overview](#overview)
- [CGUI features](#cgui)
- [CCFG features](#ccfg)
- [COBJ features](#cobj)
- [Dependencies](#dependencies)
- [Build and installation](#build)
- [Post-Installation](#post-install)
- [Gallery](#gallery)
- [Mirrors](#mirrors)

## Libraries Overview <a name="overview"></a>

### CGUI

A dynamic GUI toolkit library designed as a universal interface, equally targeting desktop, laptop, mobile, and miscellaneous devices with more or less limited inputs. All thanks to a flexible grid layout, simple widget appearance, and an advanced configuration system powered by CCFG, allowing one to tailor the theme, behavior, keybinds and input interpretation for each device class.

[Usage information](cgui)

### CCFG

A configuration language and parser library featuring array based values and short s-like expressions based functions. The language's syntax aims to be both human-readable and easy to parse. Yet provides enough tools to the end user to create branching and dynamic configurations that can be modified and reloaded on the fly.

[Usage information](ccfg)

### COBJ

A collection self-contained data structures and utilities used by both CCFG and CGUI.

[Usage information](cobj)

### Bindings

A set of thick Ada bindings are provided. Currently only COBJ and CCFG are supported. Bindings for CGUI will be made when CGUI's rewrite is completed.

[Usage information](bindings/ada)

## CGUI Features<a name="cgui"></a>

- Retained mode
- Responsive layouts
- Font based window geometry
- Pointer, Keyboard and Multi-Touch inputs
- Run-time configuration and theme reload
- Custom widgets support
- Enhanced WM hinting
- Fractional scaling
- Native transparency
- Vertically synced animations
- Smart corner styling

## CCFG Features <a name="ccfg"></a>

- Comments
- User-defined sections
- User-defined variables
- Program-defined parameters
- Arithmetic operations
- String operations
- Color operations
- Iteration loops
- Conditionals
- Child file inclusion
- [More](docs/ccfg-spec.md)

## COBJ Features <a name="cobj"></a>

- cbook: dynamic C-strings stack with grouping features
- ccolor: RGBA color representation, manipulation and conversion
- cdict: hashmap with string + group keys, FNV-1A hashing and linear probing
- cerr: error codes used by every Cassette component
- cinputs: 2D input (screen touches, key / button presses) tracker array
- crand: re-implementation of POSIX's rand48 functions with a slightly more convenient API
- cref: reference counter used to keep track of instanced components
- csafe: set of arithmetic operations on size_t with overflow and underflow protection
- cseg: 1D segment represenation and manipulation with bound checks and UB prevention
- cstr: UTF-8 strings with 2D (rows, columns, tabsize, wrapping) features

## Dependencies <a name="dependencies"></a>

- Tools :
	- C11 compiler with a stdlib + POSIX 200809L
	- Make

- Libraries :
	- [Cairo](https://cgit.freedesktop.org/cairo/)
	- [FontConfig](https://gitlab.freedesktop.org/fontconfig/fontconfig)
	- [XKBCommon](https://github.com/xkbcommon/libxkbcommon)
	- [XCB](https://gitlab.freedesktop.org/xorg/lib/libxcb)
		- XCB-ICCCM
		- XCB-Keysyms
		- XCB-Present
		- XCB-Randr
		- XCB-Render
		- XCB-XInput

## Build and Installation <a name="build"></a>

First, edit the makefile if you want to change the installation destinations. These are represented by the variables `DIR_INSTALL_INC` and `DIR_INSTALL_LIB` for the public API headers and library files respectively. By default, they are set to `/usr/include/cassette/` and `/usr/lib`.
Then, build and install Cassette with the following commands (Examples will also be built and placed under `*/build/bin`):

```
make
make install
```

Once you're done you can get rid of build files with:

```
make clean
```

If you want to uninstall the library:

```
make uninstall
```

## Post-Installation <a name="post-install"></a>

By default, the CGUI library is set to use the font "Monospace" with size 14 because it currently does not ship with its own built-in font. But because the windows geometry is dependent on the font, it is recommended to customize your font before anything else. Do note, that the font must be mono-spaced since CGUI has been specifically developed around this class of font. To set it, create a configuration file `~/.config/cgui.conf` and add to it these two lines :

```
font face "FONT_NAME"
font size  VALUE
```

Replace `FONT_NAME` and `VALUE` with your preferred font name and size. The font name follows the FontConfig naming convention. After that, if the rendered text still looks wrong, check out the other font configuration parameters in the [sample configuration file](cgui/test/cgui.conf) and add them to your current configuration to further tweak font rendering. 

## Gallery <a name="gallery"></a>

![Screenshot 1](extras/screenshot-1.png)
![Screenshot 2](extras/screenshot-2.png)
![Screenshot 3](extras/screenshot-3.png)

## Credits <a name="credits"></a>

The following third-party resources were used to make the visuals :

- [Nostromo font](https://www.fontspring.com/fonts/great-scott/nostromo)
- [Terminus font](https://terminus-font.sourceforge.net/)
- [Scientifica font](https://github.com/nerdypepper/scientifica)
- [Sevastopol Interface font](https://www.dafont.com/sevastopol-interface.font)
- [NASA Curiosity's view of Mars sky at sunset](https://www.nasa.gov/)
- [Picom for shadow and blur effects](https://github.com/yshui/picom)

## Mirrors <a name="mirrors"></a>

- https://github.com/fraawlen/cassette
- https://codeberg.org/fraawlen/cassette

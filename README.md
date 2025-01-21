<h1 align="center"><img src="extras/banner.svg"></h1>

Cassette is a lightweight framework written in C11, with a UI inspired by the cassette-futurism aesthetic. Built for modern POSIX systems, it's made out of three libraries: CGUI, CCFG and COBJ. Cassette is free and open-source software, licensed under the [LGPL-3.0](https://www.gnu.org/licenses/lgpl-3.0.en.html).

## Table of Contents <a name="toc"></a>

- [Overview](#overview)
- [Dependencies](#dependencies)
- [Build and installation](#build)
- [Post-Installation](#post-install)
- [Planned features](#future)
- [Credits](#credits)
- [Mirrors](#mirrors)

## Overview <a name="overview"></a>

#### [CGUI - Cassette Graphics](cgui)

A dynamic, retained-mode GUI toolkit library designed as a universal interface, equally targeting desktop, laptop, mobile, and miscellaneous devices with more or less limited inputs. All thanks to a flexible grid layout, simple widget appearance, and an advanced configuration system powered by CCFG, allowing one to tailor the theme, behavior, keybinds and input interpretation for each device class.

##### Features:

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

#### [CCFG - Cassette Configuration](ccfg)

A configuration language and parser library featuring array based values and short s-like expressions based functions. The language's syntax aims to be both human-readable and easy to parse. Yet provides enough tools to the end user to create branching and dynamic configurations that can be modified and reloaded on the fly.

##### Features:

- comments
- user-defined sections
- user-defined variables
- program-defined parameters
- arithmetic operations
- string operations
- color operations
- iteration loops
- conditionals
- child file inclusion

#### [COBJ - Cassette Objects](cobj)

A collection of self-contained data structures and utilities shared by both CCFG and CGUI. Notably, it includes a versatile 2D UTF-8 string object with associated methods, designed for easy manipulation of strings in monospace text displays.


##### Features;

- cbook, dynamic C-strings stack with grouping features
- ccolor, RGBA color representation, manipulation and conversion
- cdict, hashmap with string + group keys, FNV-1A hashing and linear probing
- cerr, error codes used by every Cassette component
- cinputs, 2D input (screen touches, key / button presses) tracker array
- crand, re-implementation of POSIX's rand48 functions with a slightly more convenient API
- cref, reference counter used to keep track of instanced components
- cseg, 1D segment represenation and manipulation with bound checks and UB prevention
- cstr, UTF-8 strings with 2D (rows, columns, tabsize, wrapping) features

#### [Bindings](bindings/ada)

Thick bindings for Ada 2012 are provided.

## Dependencies <a name="dependencies"></a>

- Tools :
	- C11 compiler with a stdlib + POSIX 200809L
	- Make

- Libraries :
	- [Cairo](https://cgit.freedesktop.org/cairo/)
	- [FontConfig](https://gitlab.freedesktop.org/fontconfig/fontconfig)
	- [XKBCommon](https://github.com/xkbcommon/libxkbcommon)
	- [XCB](https://gitlab.freedesktop.org/xorg/lib/libxcb)
	- [XCB-ICCCM](https://gitlab.freedesktop.org/xorg/lib/libxcb)
	- [XCB-Keysyms](https://gitlab.freedesktop.org/xorg/lib/libxcb)
	- [XCB-Present](https://gitlab.freedesktop.org/xorg/lib/libxcb)
	- [XCB-Randr](https://gitlab.freedesktop.org/xorg/lib/libxcb)
	- [XCB-Render](https://gitlab.freedesktop.org/xorg/lib/libxcb)
	- [XCB-XInput](https://gitlab.freedesktop.org/xorg/lib/libxcb)

For Debian
```
sudo apt install libcairo2-dev libfontconfig1-dev libxkbcommon-dev libxcb1-dev libxcb-icccm4-dev libxcb-keysyms1-dev libxcb-present-dev libxcb-randr0-dev libxcb-render0-dev libxcb-xinput-dev
```
For Fedora
```
sudo dnf install cairo-devel fontconfig-devel libxkbcommon-devel libxcb-devel libxcb-icccm-devel libxcb-keysyms-devel libxcb-present-devel libxcb-randr-devel libxcb-render-devel libxcb-xinput-devel
```
For Arch
```
sudo pacman -S cairo fontconfig libxkbcommon libxcb xcb-util xcb-util-keysyms xcb-util-renderutil xcb-util-wm xcb-util-image
```
For Alpine
```
sudo apk add cairo-dev fontconfig-dev libxkbcommon-dev libxcb-dev xcb-util-dev xcb-util-keysyms-dev xcb-util-wm-dev xcb-util-renderutil-dev xcb-util-image-dev
```

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

## Planned features <a name="future"></a>

- Documentation
- Navigation-to-text output for complete accessibility
- Drag and drop
- Wayland support
- Transition to a Vulkan backend (in part to support Wayland)
	- Transition from cairo to vkvg

## Credits <a name="credits"></a>

- [Nostromo font](https://www.fontspring.com/fonts/great-scott/nostromo)

## Mirrors <a name="mirrors"></a>

- https://github.com/fraawlen/cassette
- https://codeberg.org/fraawlen/cassette

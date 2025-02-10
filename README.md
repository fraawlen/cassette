<h1 align="center"><img src="extras/banner.svg" alt="project banner"></h1>

Cassette is a GUI application framework written in C23, with a UI inspired by the cassette-futurism aesthetic. Built for modern POSIX systems, it's made out of three libraries: CGUI, CCFG and COBJ. Cassette is free and open-source software, licensed under the [LGPL-3.0](https://www.gnu.org/licenses/lgpl-3.0.en.html).

## Table of Contents <a name="toc"></a>

- [Overview](#overview)
- [Status](#status)
- [Documentation](#documentation)
- [Dependencies](#dependencies)
- [Build and Installation](#build)
- [Post-Installation](#post-install)
- [Gallery](#gallery)
- [Planned Features](#future)
- [Third-Party Visual Resources](#credits)
- [Mirrors](#mirrors)

## Overview <a name="overview"></a>

#### [CGUI - Cassette Graphics](cgui)

The main component of the framework - a retained-mode X11 GUI toolkit library designed as a universal interface, equally targeting desktop, laptop, mobile, and miscellaneous devices with more or less limited inputs. All thanks to a flexible and responsive grid layout, simple widget appearance, and an advanced configuration system powered by CCFG, allowing one to tailor the theme, behavior, keybinds and input interpretation for each device class.

#### [CCFG - Cassette Configuration](ccfg)

A configuration language and parser library featuring array based values and short s-like expressions based functions. The language's syntax aims to be both human-readable and easy to parse. Yet provides enough tools to the end user to create branching and dynamic configurations that can be modified and reloaded on the fly.

#### [COBJ - Cassette Objects](cobj)

A collection of self-contained data structures and utilities shared by both CCFG and CGUI. Notably, it includes a versatile 2D UTF-8 string object with associated methods, designed for easy manipulation of strings in monospace text displays.

#### [Bindings](bindings/ada)

COBJ and CCFG Thick bindings for Ada 2012 are provided. CGUI bindings coming soon.

## Status <a name="status"></a>

> [!Warning]
> Cassette is currently in an Alpha stage. COBJ and CCFG (with the exception of cstr.h) are largely complete and considered stable, but CGUI remains under active development. Notably, CGUI still needs broader Unicode support—only single codepoint glyphs work reliably at the moment—and it lacks a substantial set of widgets. Although some minimal documentation exists for end users, it is also a work in progress. Ongoing development aims to address these shortcomings and expand Cassette’s feature set.

## Documentation <a name="documentation"></a>

- [UI model](docs/ui-model.md)
- [CGUI theming](docs/cgui-theming.md)
- [CCFG language](docs/ccfg-language.md)
- [API reference](docs/api-reference.md)

## Dependencies <a name="dependencies"></a>

Tools:

- C23 compiler with a stdlib + POSIX 200809L
- Make

Libraries:

- [Cairo](https://cgit.freedesktop.org/cairo/)
- [FontConfig](https://gitlab.freedesktop.org/fontconfig/fontconfig)
- [XKBCommon](https://github.com/xkbcommon/libxkbcommon)
- [XCB](https://gitlab.freedesktop.org/xorg/lib/libxcb)
- [XCB-ICCCM](https://gitlab.freedesktop.org/xorg/lib/libxcb-wm)
- [XCB-Keysyms](https://gitlab.freedesktop.org/xorg/lib/libxcb-keysyms)
- [XCB-Present](https://gitlab.freedesktop.org/xorg/lib/libxcb)
- [XCB-Randr](https://gitlab.freedesktop.org/xorg/lib/libxcb)
- [XCB-XInput](https://gitlab.freedesktop.org/xorg/lib/libxcb)

## Build and Installation <a name="build"></a>

First, edit the makefile if you want to change the installation destinations. These are represented by the variables `DIR_INSTALL_INC` and `DIR_INSTALL_LIB` for the public API headers and library files respectively. By default, they are set to `/usr/include/cassette/` and `/usr/lib`.
Then, build and install Cassette with the following commands (Examples will also be built and placed under the `*/build/bin` directory of each library):

```
make
make install
```

Optional step to install CCFG vim syntax highlighting:

```
make install-syntax
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

By default, the CGUI library is set to use the font "Monospace" with size 14 because it currently does not ship with its own built-in font. But because the windows geometry is dependent on the font, it is recommended to customize your font before anything else. Do note, that the font must be mono-spaced since CGUI has been specifically developed around this class of font. To set it, create a configuration file `~/.config/cassette/cgui.ccfg` and add to it these two lines :

```
font face "FONT_NAME"
font size  VALUE
```

Replace `FONT_NAME` and `VALUE` with your preferred font name and size. The font name follows the FontConfig naming convention. More font and CGUI theme configuration options can be found [here](docs/cgui-theming.md). For full theme examples, check out these [pre-made themes](cgui/themes).

## Usage

Add these includes to access the functions of each library :

```
#include <cassette/cgui.h>
#include <cassette/ccfg.h>
#incluce <cassette/cobj.h>
```

As well as these compilation flags :

```
-lcgui
-lccfg
-lcobj
```
Minimal examples:

- [Hello world window](docs/cgui-example.md)
- [Simple resource lookup](docs/ccfg-example.md)

More elaborate demos:

- [CGUI](cgui/examples)
- [CCFG](ccfg/examples)
- [COBJ](cobj/examples)

## Gallery <a name="gallery"></a>

| ![Screenshot 1](extras/screenshots/1.png) | ![Screenshot 2](extras/screenshots/2.png) |
|-------------------------------------------|-------------------------------------------|
| ![Screenshot 3](extras/screenshots/3.png) | ![Screenshot 4](extras/screenshots/4.png) |

## Planned Features <a name="future"></a>

- Proper Unicode Plane-0 EGC handling
- Improved font rendering
- Native Wayland backend
- Navigation-to-text output for screen-readers accessibility
- Drag and drop
- More cells (widgets)
- More theming options
- Complete API reference
- Step-by-step CGUI tutorial

## Third-Party Visual Resources <a name="credits"></a>

- [Nostromo font](https://www.fontspring.com/fonts/great-scott/nostromo)
- [Terminus font](https://terminus-font.sourceforge.net/)
- [Fira Code font](https://github.com/tonsky/FiraCode)
- [Scientifica font](https://github.com/nerdypepper/scientifica)
- [Picom shadow and blur effects](https://github.com/yshui/picom)

## Mirrors <a name="mirrors"></a>

- [Github](https://github.com/fraawlen/cassette)
- [Codeberg](https://codeberg.org/fraawlen/cassette)


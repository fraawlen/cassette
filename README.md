<p align=center><img src="./extras/banner.svg"></p>

Cassette is a modular general-purpose GUI toolkit written in C for X11 end-user applications. It's designed as a universal GUI, equally targeting desktop, laptop, mobile, and miscellaneous devices with more or less limited inputs. All thanks to a flexible grid layout, simple widget appearance, and an advanced configuration system, allowing one to tailor the theme, behavior, keybinds and input interpretation for each device class. CGUI also tries to limit the amount of direct external dependencies to make it easier to set it up on any system running an X11 display server.

The library is free and open-source software licensed under the [LGPL-2.1](https://www.gnu.org/licenses/old-licenses/lgpl-2.1.html). It's made to run on modern POSIX-compliant systems.

> [!Warning]
> This library is alpha software! Some features and widgets are still missing. Moreover, it is currently being rewritten in a better code and interface style under the Cassette name (formerly Derelict Graphics or DG, and on this branch the functions namespaces have not been yet updated). Because of that, the function names, will change in the next release. Checkout the 'rewrite' branch for the latest developments.

Features
--------

- C API
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

Dependencies
------------

- Tools :

	- C11 compiler with a stdlib + POSIX 200809L
	- Make
	- Rsync

- Third-party libraries :

	- [Cairo](https://cgit.freedesktop.org/cairo/)
	- [FontConfig](https://gitlab.freedesktop.org/fontconfig/fontconfig)
	- [XCB](https://gitlab.freedesktop.org/xorg/lib/libxcb)
	- [XKBCommon](https://github.com/xkbcommon/libxkbcommon)

Installation
------------

First, edit the makefile if you want to change the installation destinations. These are represented by the variables DEST_HEADERS and DEST_LIBS for the public API headers and library files respectively. By default, they are set to /usr/include/dg/ and /usr/lib. Then, build and install CGUI with the following commands :

```
make
make install
```

After these steps, a shared binary will be generated and installed on your system. Examples will also be built and placed under `build/bin`.

Post-Installation
-----------------

By default, the library is set to use the font "Monospace" with size 14 because it currently does not ship with its own built-in font. But because the windows geometry is dependent on the font, it is recommended to customize your font before anything else. Do note, that the font must be mono-spaced since CGUI has been specifically developed around this class of font. To set it, create a configuration file `~/.config/dg.conf` and add to it these two lines :

```
core.font_face = "FONT_NAME"
core.font_size = VALUE
```

Replace `FONT_NAME` and `VALUE` with your preferred font name and size. The font name follows the FontConfig naming convention. After that, if the rendered text still looks wrong, check out the other font configuration parameters `core.font_*` in the [sample configuration file](dg.conf) and add them to your current configuration to further tweak font rendering. A few themes are also provided in the `theme` directory. To install them, simply copy paste their contents into your working configuration file.

Minimal Example
---------------

One of the simplest GUI programs, a HelloWorld :

```c
#include <dg/core/core.h>
#include <dg/base/base.h>

int
main(int argc, char **argv)
{
	dg_core_window_t *w;
	dg_core_grid_t *g;
	dg_core_cell_t *c;

	/* library modules initialisation */

	dg_core_init(argc, argv, NULL, NULL, NULL);
	dg_base_init();

	/* object instantiation */

	w = dg_core_window_create(DG_CORE_WINDOW_DEFAULT);
	g = dg_core_grid_create(1, 1);
	c = dg_base_label_create();

	/* cell configuration */

	dg_base_label_set_label(c, "Hello World");
	
	/* grid configuration */

	dg_core_grid_set_column_width(g, 0, 11);
	dg_core_grid_assign_cell(g, c, 0, 0, 1, 1);
	
	/* window configuration */

	dg_core_window_push_grid(w, g);
	dg_core_window_activate(w);

	/* run */

	dg_core_loop_run();

	/* end */

	return 0;
}
```

Compile with :

```
cc hello.c -ldg -ldg-base 
```

Output :

![hello world output](./extras/hello.png)

Check out the `examples` directory for more in depth demonstrations.

Screenshots
-----------

![Screenshot 1](./extras/screenshot-1.png)
![Screenshot 2](./extras/screenshot-2.png)
![Screenshot 3](./extras/screenshot-3.png)

In these screenshots, the following third-party resources were used :

- [Terminus font](https://terminus-font.sourceforge.net/)
- [Scientifica font](https://github.com/nerdypepper/scientifica)
- [NASA Curiosity's view of Mars sky at sunset](https://www.nasa.gov/)
- [Picom for shadow and blur effects](https://github.com/yshui/picom)

Mirrors
-------

- https://codeberg.org/fraawlen/cassette-graphics
- https://github.com/fraawlen/cassette-graphics



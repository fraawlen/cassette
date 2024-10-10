<p align=center><img src="banner.svg"></p>

Cassette Graphics, or Cassette GUI (CGUI) is a modular general-purpose GUI toolkit written in C for X11 end-user applications. It's designed as a universal GUI, equally targeting desktop, laptop, mobile, and miscellaneous devices with more or less limited inputs. All thanks to a flexible grid layout, simple widget appearance, and an advanced configuration system, allowing one to tailor the theme, behavior, keybinds and input interpretation for each device class. CGUI also tries to limit the amount of direct external dependencies to make it easier to set it up on any system running an X11 display server.

Usage
-----

Add this include to get access to the library functions :

```
#include <cassette/cgui.h>
```

As well as this compilation flag :

```
-lcgui
```

Minimal Example
---------------

One of the simplest GUI programs, a HelloWorld :

```c
#include <cassette/cgui.h>

int
main(int argc, char **argv)
{
	cgui_window *window;
	cgui_grid   *grid;
	cgui_label  *label;

	/* library modules initialisation */

	cgui_init(argc, argv);

	/* object instantiation */

	window = cgui_window_create();
	grid   = cgui_grid_create(1, 1);
	label  = cgui_label_create();

	/* cell configuration */

	cgui_label_set(label, "Hello World");
	
	/* grid configuration */

	cgui_grid_resize_col(grid, 0, 11);
	cgui_grid_assign_cell(grid, label, 0, 0, 1, 1);
	
	/* window configuration */

	cgui_window_push_grid(window, grid);
	cgui_window_activate(window);

	/* run */

	cgui_run();

	/* end */

	return 0;
}
```

Compile with :

```
cc hello.c -lcgui
```

Output :

![hello world output](../extras/hello.png)

Check out the `examples` directory for more in depth demonstrations.

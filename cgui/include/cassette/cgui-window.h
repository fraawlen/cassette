/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Graphics (CGUI) library.
 *
 * This library is free software; you can redistribute it and/or modify it either under the terms of the GNU
 * Lesser General Public License as published by the Free Software Foundation; either version 3.0 of the
 * License or (at your option) any later version.
 *
 * This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
 * See the LGPL for the specific language governing rights and limitations.
 *
 * You should have received a copy of the GNU Lesser General Public License along with this program. If not,
 * see <http://www.gnu.org/licenses/>.
 */

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#pragma once

#include <cairo/cairo.h>
#include <stdbool.h>
#include <xcb/xcb.h>

#include "cgui-attributes.h"
#include "cgui-cell.h"
#include "cgui-grid.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

 /**
 * Opaque window object that holds states.
 * Some methods, upon failure, will set an error that can be checked with cgui_error(). If any error is set
 * all window methods will exit early with default return values and no side-effects. It's possible to try to
 * resolve errors with cgui_repair().
 */
typedef struct cgui_window cgui_window;

/**
 * Available window types.
 * CGUI_WINDOW_UNDERLAY, CGUI_WINDOW_OVERLAY and CGUI_WINDOW_POPUP have an override-redirect flag set,
 * therefore they are not controlled by the window manager. Popup windows will automatically deactivate when
 * user input is outside the bounds of the popup.
 */
enum cgui_window_type
{
	CGUI_WINDOW_NORMAL,
	CGUI_WINDOW_DIALOG,
	CGUI_WINDOW_UNDERLAY,
	CGUI_WINDOW_OVERLAY,
	CGUI_WINDOW_POPUP,
};

/**
 * Enumeration of window state masks.
 */
enum cgui_window_state_mask
{
	CGUI_WINDOW_ACTIVE,
	CGUI_WINDOW_MAPPED,
	CGUI_WINDOW_FOCUSED,
	CGUI_WINDOW_DISABLED,
	CGUI_WINDOW_LOCKED_GRID,
	CGUI_WINDOW_LOCKED_FOCUS,
};

/**
 * Structure holding the current state flags of a window.
 *
 * @param active       : Window is activated
 * @param mapped       : Window is displayed
 * @param focused      : Window is currently focused by the window manager
 * @param disabled     : Window is disabled
 * @param locked_grid  : Window’s grid is locked, layout responsiveness is off
 * @param locked_focus : Window’s cell focus is locked. Mouse and touch events will not change it.
 */
struct cgui_window_state_flags
{
	bool active;
	bool mapped;
	bool focused;
	bool disabled;
	bool locked_grid;
	bool locked_focus;
};

/**
 * Structure describing the visual style for a window used during configuration.
 *
 * @param cn_type : Corner types. From top-left, clockwise
 * @param cn_size : Corner sizes. From top-left, clockwise
 * @param bd_size : Border thickness
 * @param bd_cl   : Border color
 * @param bg_cl   : Background color
 * @param ena     : Flag to indicate if custom styling is enabled
 */
struct cgui_window_style
{
	enum cgui_corner cn_type[4];
	double           cn_size[4];
	double           bd_size;
	struct ccolor    bd_cl;
	struct ccolor    bg_cl;
	bool             ena;
};

/************************************************************************************************************/
/* GLOBALS **************************************************************************************************/
/************************************************************************************************************/

/**
 * A macro that gives an uninitialized window a non-NULL value that is safe to use with the window's
 * related functions. However, any function called with a handle set to this value will return early without
 * any side effects.
 */
#define CGUI_WINDOW_PLACEHOLDER (&cgui_window_placeholder_instance)

/**
 * Global window instance with the error state set to CERR_INVALID. This instance is only made
 * available to allow the static initialization of window pointers with the macro CGUI_WINDOW_PLACEHOLDER.
 */
extern cgui_window cgui_window_placeholder_instance;

/************************************************************************************************************/
/* CONSTRUCTORS / DESTRUCTORS *******************************************************************************/
/************************************************************************************************************/

/**
 * Creates an empty window instance.
 *
 * @return     : Created window instance
 * @return_err : CGUI_WINDOW_PLACEHOLDER
 */
cgui_window *
cgui_window_create(void)
CGUI_NONNULL_RETURN;

/**
 * Destroys the given window instance and frees all associated resources.
 *
 * @param window : Window instance to interact with
 */
void
cgui_window_destroy(cgui_window *window)
CGUI_NONNULL(1);

/************************************************************************************************************/
/* IMPURE METHODS *******************************************************************************************/
/************************************************************************************************************/

/**
 * Activates the specified window, making it visible and interactive.
 * The return value is important for popup activation, because it can fail if CGUI fails to grab the pointer
 * and keyboard (generally because an other process already own the grab).
 *
 * @param window : Window instance to interact with
 *
 * @return     : True if the window was successfully activated; false otherwise.
 * @return_err : False if activation failed or if the window is already active or invalid.
 *
 * @error CERR_XCB
 */
bool
cgui_window_activate(cgui_window *window)
CGUI_NONNULL(1);

/**
 * Deactivates the specified window, hiding it and disabling interactions.
 * Any cells that were visible receive a cancellation cell event.
 *
 * @param window : Window instance to interact with
 *
 * @error CERR_XCB
 */
void
cgui_window_deactivate(cgui_window *window)
CGUI_NONNULL(1);

/**
 * Deactivates all active popup windows.
 *
 * @error CERR_XCB
 */
void
cgui_window_deactivate_all_popups(void);

/**
 * Deactivates any child popup windows associated with the given window. The given window has to be a
 * popup.
 *
 * @param window : Window instance to interact with
 *
 * @error CERR_XCB
 */
void
cgui_window_deactivate_children_popups(cgui_window *window)
CGUI_NONNULL(1);

/**
 * Disables a window, making it non-interactive but still visible.
 *
 * @param window : Window instance to interact with
 *
 * @error CERR_XCB
 */
void
cgui_window_disable(cgui_window *window)
CGUI_NONNULL(1);

/**
 * Enables a window if it was previously disabled.
 *
 * @param window : Window instance to interact with
 *
 * @error CERR_XCB
 */
void
cgui_window_enable(cgui_window *window)
CGUI_NONNULL(1);

/**
 * Moves a window to the given coordinates. The window's origin is situated on the top-left.
 * If the window is not an underlay, overlay or popup, this function only acts as a "suggestion", as the
 * window manager can choose to override the position.
 *
 * @param window : Window instance to interact with
 * @param x      : X-coordinate
 * @param y      : Y-coordinate
 *
 * @error CERR_XCB
 */
void
cgui_window_move(cgui_window *window, double x, double y)
CGUI_NONNULL(1);

/**
 * Moves a window to new coordinates using smart repositioning based on monitor boundaries.
 * If the window's current width doesn't fit between the primary x_1 coordinate and monitor edge, then x_2
 * cooridnate will be used instead, and the window origin will be shifted to the right. Idem for y_1 and y_2
 * on the vertical axis.
 *
 *
 * @param window : Window instance to interact with
 * @param x_1    : Primary X-coordinate
 * @param y_1    : Primary Y-coordinate
 * @param x_2    : Alternative X-coordinate if the first is out of bounds.
 * @param y_2    : Alternative Y-coordinate if the first is out of bounds.
 *
 * @error CERR_XCB
 */
void
cgui_window_move_smart(cgui_window *window, double x_1, double y_1, double x_2, double y_2)
CGUI_NONNULL(1);

/**
 * Marks the specified window as non-urgent for the window mananger.
 *
 * @param window : Window instance to interact with
 *
 * @error CERR_XCB
 */
void
cgui_window_non_urgent(cgui_window *window)
CGUI_NONNULL(1);

/**
 * Registers a callback function to be invoked when the window is requested to close.
 * If a NULL function pointer is given, the default callback will be used instead.
 * The default callback simply deactivate the window.
 *
 * @param window : Window instance to interact with
 * @param fn     : Callback function
 *
 * @param fn->window : Window that received the close request
 */
void
cgui_window_on_close(cgui_window *window, void (*fn)(cgui_window *window))
CGUI_NONNULL(1);

/**
 * Registers a callback function to be invoked after the window is redrawn.
 * If a NULL function pointer is given, the callback is deactivated.
 *
 * @param window : Window instance to interact with
 * @param fn     : Callback function
 *
 * @param fn->window : Window that got redrawn
 * @param fn->delay  : Elapsed time (in microseconds) since the window was last drawn
 */
void
cgui_window_on_draw(cgui_window *window, void (*fn)(cgui_window *window, unsigned long delay))
CGUI_NONNULL(1);

/**
 * Registers a callback function to be invoked when the window's focused cell changes.
 * If a NULL function pointer is given, the callback is deactivated.
 *
 * @param window : Window instance to interact with
 * @param fn     : Callback function
 *
 * @param fn->window : Window whose focus changed
 * @param fn->cell   : New cell that got focused
 */
void
cgui_window_on_focus(cgui_window *window, void (*fn)(cgui_window *window, cgui_cell *cell))
CGUI_NONNULL(1);

/**
 * Registers a callback function to be invoked when the window's shown grid changes.
 * If a NULL function pointer is given, the callback is deactivated.
 *
 * @param window : Window instance to interact with
 * @param fn     : Callback function
 *
 * @param fn->window : Window that received the close request
 * @param fn->grid   : Grid that is now shown
 */
void
cgui_window_on_grid(cgui_window *window, void (*fn)(cgui_window *window, cgui_grid *grid))
CGUI_NONNULL(1);

/**
 * Registers a callback function to be invoked when the window's state changes.
 * If a NULL function pointer is given, the callback is deactivated.
 *
 * @param window : Window instance to interact with
 * @param fn     : Callback function
 *
 * @param fn->window : Window that received the close request
 * @param fn->mask   : States that were modified
 */
void
cgui_window_on_state(cgui_window *window, void (*fn)(cgui_window *window, enum cgui_window_state_mask mask))
CGUI_NONNULL(1);

/**
 * Removes the specified grid from the window's grid collection.
 * This function is only valid to use when the window is deactivated.
 *
 * @param window : Window instance to interact with
 * @param grid   : Grid instance to remove
 */
void
cgui_window_pull_grid(cgui_window *window, cgui_grid *grid)
CGUI_NONNULL(1, 2);

/**
 * Adds a new grid to the window's grid collection.
 * As per WGC's model, the new grid should be stricly bigger or smaller than the grids previously added.
 * This function is only valid to use when the window is deactivated.
 * Use cgui_window_can_push_grid() to test grid compatibility.
 *
 * @param window : Window instance to interact with
 * @param grid   : Grid instance to add
 *
 * @error CERR_MEMORY
 * @error CERR_OVERFLOW
 */
void
cgui_window_push_grid(cgui_window *window, cgui_grid *grid)
CGUI_NONNULL(1, 2);

/**
 * Schedules a redraw of the window.
 *
 * @param window : Window instance to interact with
 */
void
cgui_window_redraw(cgui_window *window)
CGUI_NONNULL(1);

/**
 * Schedules an asynchronous redraw of the window (if the end-user configuration allows it, otherwhise, 
 * this function behaves like cgui_window_redraw()).
 *
 * @param window : Window instance to interact with
 */
void
cgui_window_redraw_async(cgui_window *window)
CGUI_NONNULL(1);

/**
 * Schedules a redraw of the window after a specified delay. The actual redraw time may be affected by the
 * display server and screen refresh rate.
 *
 * @param window : Window instance to interact with
 * @param delay  : Mircoseconds before the redraw should occurs
 */
void
cgui_window_redraw_delayed(cgui_window *window, unsigned long delay)
CGUI_NONNULL(1);

/**
 * Renames the specified window.
 *
 * @param window : Window instance to interact with
 * @param name   : NUL terminated string.
 *
 * @error CERR_XCB
 * @error CERR_MEMORY
 */
void
cgui_window_rename(cgui_window *window, const char *name)
CGUI_NONNULL(1, 2);

/**
 * Resets the currently shown grid to the default state.
 * This function is only valid to use when the window is deactivated.
 *
 * @param window : Window instance to interact with
 */
void
cgui_window_reset_grid(cgui_window *window)
CGUI_NONNULL(1);

/**
 * Resizes the specified window to the given dimensions.
 * If the window is not an underlay, overlay or popup, this function only acts as a "suggestion", as the
 * window manager can choose to override the dimensions.
 *
 * @param window : Window instance to interact with
 * @param width  : New width
 * @param height : New height
 *
 * @error CERR_XCB
 */
void
cgui_window_resize(cgui_window *window, double width, double height)
CGUI_NONNULL(1);

/**
 * Sets an accelerator (custom shortcut) for the window. It gets triggered by the end-user.
 * If the given callback function is NULL, the accelerator at ID is disabled.
 * The ID should be set between 1 and 12. Any other values are forbidden.
 * The name will be discoverable by third-party software.
 *
 * @param window : Window instance to interact with
 * @param id     : Accelerator ID (1-12)
 * @param name   : Name of the accelerator
 * @param fn     : Callback function
 *
 * @param fn->window : Window that received the accelerator trigger
 * @param fn->id     : Callback ID
 *
 * @error CERR_XCB
 * @error CERR_PARAM
 * @error CERR_MEMORY
 */
void
cgui_window_set_accelerator(cgui_window *window, int id, const char *name, void (*fn)(cgui_window *window, int id))
CGUI_NONNULL(1, 3);

/**
 * Adds a key-value pair. Used to bind arbitrary data to the given specific window.
 * Its up to the caller of this function to manage the validity of the given pointer.
 *
 * @param window : Window instance to interact with
 * @param key    : NUL terminated string
 * @param data   : pointer to user data
 *
 * @error CERR_OVERFLOW
 * @error CERR_MEMORY
 */
void
cgui_window_set_data(cgui_window *window, const char *key, void *data)
CGUI_NONNULL(1, 2, 3);

/**
 * Sets the type of the window.
 *
 * @param window : Window instance to interact with
 * @param type   : Window type.
 */
void
cgui_window_set_type(cgui_window *window, enum cgui_window_type type)
CGUI_NONNULL(1);

/**
 * Swaps the currently displayed grid with another grid. One of the grids has to be part of the window (with
 * cgui_window_push_grid()). Unlike cgui_window_push_grid(), this function is valid to call only when the
 * window is activated and the two grids are the exact same size.
 * Use cgui_window_can_swap_grid() to test grid compatibility.
 *
 * @param window : Window instance to interact with
 * @param grid_1 : First  grid instance.
 * @param grid_2 : Second grid instance.
 */
void
cgui_window_swap_grid(cgui_window *window, cgui_grid *grid_1, cgui_grid *grid_2)
CGUI_NONNULL(1, 2, 3);

/**
 * Attaches a window to another window, making it transient.
 *
 * @param window       : Window instance to interact with
 * @param window_under : Underlying window instance to which the first window will be attached.
 *
 * @error CERR_XCB
 */
void
cgui_window_tack(cgui_window *window, cgui_window *window_under)
CGUI_NONNULL(1, 2);

/**
 * Detaches the window from any underlying window.
 *
 * @param window : Window instance to interact with
 *
 * @error CERR_XCB
 */
void
cgui_window_untack(cgui_window *window)
CGUI_NONNULL(1);

/**
 * Marks the specified window as urgent for the window-manager.
 *
 * @param window : Window instance to interact with
 *
 * @error CERR_XCB
 */
void
cgui_window_urgent(cgui_window *window)
CGUI_NONNULL(1);

/************************************************************************************************************/
/* PURE METHODS *********************************************************************************************/
/************************************************************************************************************/

/**
 * Retrieves the Cairo drawable context associated with the window.
 *
 * @param window : Window instance to interact with
 *
 * @return     : Cairo drawing context
 * @return_err : NULL
 */
cairo_t *
cgui_window_cairo_drawable(const cgui_window *window)
CGUI_NONNULL(1)
CGUI_PURE;

/**
 * Retrieves the Cairo surface associated with the window.
 *
 * @param window : Window instance to interact with
 *
 * @return     : Cairo surface
 * @return_err : NULL
 */
cairo_surface_t *
cgui_window_cairo_surface(const cgui_window *window)
CGUI_NONNULL(1)
CGUI_PURE;

/**
 * Checks whether the specified grid can be added to the window's grid collection.
 *
 * @param window : Window instance to interact with
 * @param grid   : Grid instance to check.
 *
 * @return     : True if the grid can be added; false otherwise
 * @return_err : False
 */
bool
cgui_window_can_push_grid(const cgui_window *window, cgui_grid *grid)
CGUI_NONNULL(1, 2)
CGUI_PURE;

/**
 * Checks if the two specified grids can be swapped within the window.
 *
 * @param window : Window instance to interact with
 * @param grid_1 : First cgui_grid instance
 * @param grid_2 : Second cgui_grid instance
 *
 * @return     : True if the grids are swappable; false otherwise.
 * @return_err : False
 */
bool
cgui_window_can_swap_grid(const cgui_window *window, cgui_grid *grid_1, cgui_grid *grid_2)
CGUI_NONNULL(1, 2, 3)
CGUI_PURE;

/**
 * Retrieves a pointer matching the key that was previously set with cgui_window_set_data().
 * Its up to the caller of this function to manage the validity of the given pointer.
 *
 * @param window : Window instance to interact with
 * @param key    : NUL terminated string
 *
 * @return     : Pointer that matches the key, NULL is none is found
 * @return_err : NULL
 */
void *
cgui_window_data(const cgui_window *, const char *key)
CGUI_NONNULL(1);

/**
 * Retrieves the currently focused cell within the window.
 *
 * @param window : Window instance to interact with
 *
 * @return     : Currently focused cell instance, if no cell is focused, CGUI_CELL_PLACEHOLDER is returned
 * @return_err : CGUI_CELL_PLACEHOLDER
 */
cgui_cell *
cgui_window_focused_cell(const cgui_window *window)
CGUI_NONNULL_RETURN
CGUI_NONNULL(1)
CGUI_PURE;

/**
 * Returns the current height of the window.
 *
 * @param window : Window instance to interact with
 *
 * @return     : Current window height in pixels.
 * @return_err : 0.0
 */
double
cgui_window_height(const cgui_window *window)
CGUI_NONNULL(1)
CGUI_PURE;

/**
 * Checks if the window instance is valid.
 *
 * @param window : Window instance to interact with
 *
 * @return     : True if the window is valid; false otherwise
 * @return_err : False
 */
bool
cgui_window_is_valid(const cgui_window *window)
CGUI_NONNULL(1)
CGUI_PURE;

/**
 * Returns the minimum height required by the window based on its grid layout
 *
 * @param window : Window instance to interact with
 *
 * @return     : Minimum window height in pixels
 * @return_err : 0.0
 */
double
cgui_window_min_height(const cgui_window *window)
CGUI_NONNULL(1)
CGUI_PURE;

/**
 * Returns the minimum width required by the window based on its grid layout
 *
 * @param window : Window instance to interact with
 *
 * @return     : Minimum window width in pixels
 * @return_err : 0.0
 */
double
cgui_window_min_width(const cgui_window *window)
CGUI_NONNULL(1)
CGUI_PURE;

/**
 * Retrieves the currently shown grid of the window.
 *
 * @param window : Window instance to interact with
 *
 * @return     : Currently shown grid instance. If no grid is shown CGUI_GRID_PLACEHOLDER is returned
 * @return_err : CGUI_GRID_PLACEHOLDER
 */
cgui_grid *
cgui_window_shown_grid(const cgui_window *window)
CGUI_NONNULL_RETURN
CGUI_NONNULL(1)
CGUI_PURE;

/**
 * Retrieves the current state flags of the window.
 *
 * @param window : Window instance to interact with
 *
 * @return     : A struct cgui_window_state_flags representing the window's state
 * @return_err : All flags set to false
 */
struct cgui_window_state_flags
cgui_window_state(const cgui_window *window)
CGUI_NONNULL(1)
CGUI_PURE;

/**
 * Returns the current width of the window.
 *
 * @param window : Window instance to interact with
 *
 * @return     : Current window width in pixels.
 * @return_err : 0.0
 */
double
cgui_window_width(const cgui_window *window)
CGUI_NONNULL(1)
CGUI_PURE;

/**
 * Retrieves the X-coordinate of the window's position.
 *
 * @param window : Window instance to interact with
 *
 * @return     : X-coordinate
 * @return_err : 0.0
 */
double
cgui_window_x(const cgui_window *window)
CGUI_NONNULL(1)
CGUI_PURE;

/**
 * Retrieves the X11 window identifier associated with the window.
 *
 * @param window : Window instance to interact with
 *
 * @return     : X11 window ID
 * @return_err : 0
 */
xcb_window_t
cgui_window_x11_id(const cgui_window *window)
CGUI_NONNULL(1)
CGUI_PURE;

/**
 * Retrieves the y-coordinate of the window's position.
 *
 * @param window : Window instance to interact with
 *
 * @return     : Y-coordinate
 * @return_err : 0.0
 */
double
cgui_window_y(const cgui_window *window)
CGUI_NONNULL(1)
CGUI_PURE;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif


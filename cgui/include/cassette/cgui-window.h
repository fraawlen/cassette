/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Graphics (CGUI) library.
 *
 * This library is free software; you can redistribute it and/or modify it either under the terms of the GNU
 * Lesser General Public License as published by the Free Software Foundation; either version 2.1 of the
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
 *
 */
typedef struct cgui_window cgui_window;

/**
 *
 */
enum cgui_window_type
{
	CGUI_WINDOW_NORMAL,
	CGUI_WINDOW_DIALOG,
	CGUI_WINDOW_DESKTOP,
	CGUI_WINDOW_OVERLAY,
};

/**
 *
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
 *
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

/************************************************************************************************************/
/* GLOBALS **************************************************************************************************/
/************************************************************************************************************/

/**
 * A macro that gives uninitialized window a non-NULL value that is safe to use with the window's realted
 * functions. However, any function called with a handle set to this value will return early and without any
 * side effects.
 */
#define CGUI_WINDOW_PLACEHOLDER (&cgui_window_placeholder_instance)

/**
 * Global window instance with the error state set to CGUI_WINDOW_INVALID. This instance is only made
 * available to allow the static initialization of window pointers with the macro CGUI_WINDOW_PLACEHOLDER.
 */
extern cgui_window cgui_window_placeholder_instance;

/************************************************************************************************************/
/* CONSTRUCTORS / DESTRUCTORS *******************************************************************************/
/************************************************************************************************************/

/**
 *
 */
cgui_window *
cgui_window_create(void)
CGUI_NONNULL_RETURN;

/**
 *
 */
void
cgui_window_destroy(cgui_window *window)
CGUI_NONNULL(1);

/************************************************************************************************************/
/* IMPURE METHODS *******************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
void
cgui_window_activate(cgui_window *window)
CGUI_NONNULL(1);

/**
 *
 */
void
cgui_window_deactivate(cgui_window *window)
CGUI_NONNULL(1);

/**
 *
 */
void
cgui_window_disable(cgui_window *window)
CGUI_NONNULL(1);

/**
 *
 */
void
cgui_window_enable(cgui_window *window)
CGUI_NONNULL(1);

/**
 *
 */
void
cgui_window_move(cgui_window *window, double x, double y)
CGUI_NONNULL(1);

/**
 *
 */
void
cgui_window_non_urgent(cgui_window *window)
CGUI_NONNULL(1);

/**
 *
 */
void
cgui_window_on_close(cgui_window *window, void (*fn)(cgui_window *window))
CGUI_NONNULL(1);

/**
 *
 */
void
cgui_window_on_draw(cgui_window *window, void (*fn)(cgui_window *window, unsigned long delay_1, unsigned long delay_2))
CGUI_NONNULL(1);

/**
 *
 */
void
cgui_window_on_focus(cgui_window *window, void (*fn)(cgui_window *window, cgui_cell *cell))
CGUI_NONNULL(1);

/**
 *
 */
void
cgui_window_on_grid(cgui_window *window, void (*fn)(cgui_window *window, cgui_grid *grid))
CGUI_NONNULL(1);

/**
 *
 */
void
cgui_window_on_state(cgui_window *window, void (*fn)(cgui_window *window, enum cgui_window_state_mask mask))
CGUI_NONNULL(1);

/**
 *
 */
void
cgui_window_pull_grid(cgui_window *window, cgui_grid *grid)
CGUI_NONNULL(1, 2);

/**
 *
 */
void
cgui_window_push_grid(cgui_window *window, cgui_grid *grid)
CGUI_NONNULL(1, 2);

/**
 *
 */
void
cgui_window_redraw(cgui_window *window)
CGUI_NONNULL(1);

/**
 *
 */
void
cgui_window_rename(cgui_window *window, const char *name)
CGUI_NONNULL(1, 2);

/**
 *
 */
void
cgui_window_reset_grid(cgui_window *window)
CGUI_NONNULL(1);

/**
 *
 */
void
cgui_window_resize(cgui_window *window, double width, double height)
CGUI_NONNULL(1);

/**
 *
 */
void
cgui_window_set_accelerator(cgui_window *window, int id, const char *name, void (*fn)(cgui_window *window, int id))
CGUI_NONNULL(1, 3);

/**
 *
 */
void
cgui_window_set_type(cgui_window *window, enum cgui_window_type type)
CGUI_NONNULL(1);

/**
 *
 */
void
cgui_window_swap_grid(cgui_window *window, cgui_grid *grid_1, cgui_grid *grid_2)
CGUI_NONNULL(1, 2, 3);

/**
 *
 */
void
cgui_window_tack(cgui_window *window, cgui_window *window_under)
CGUI_NONNULL(1, 2);

/**
 *
 */
void
cgui_window_untack(cgui_window *window)
CGUI_NONNULL(1);

/**
 *
 */
void
cgui_window_urgent(cgui_window *window)
CGUI_NONNULL(1);

/************************************************************************************************************/
/* PURE METHODS *********************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
cairo_t *
cgui_window_cairo_drawable(const cgui_window *window)
CGUI_NONNULL(1)
CGUI_PURE;

/**
 *
 */
cairo_surface_t *
cgui_window_cairo_surface(const cgui_window *window)
CGUI_NONNULL(1)
CGUI_PURE;

/**
 *
 */
bool
cgui_window_can_push_grid(const cgui_window *window, cgui_grid *grid)
CGUI_NONNULL(1, 2)
CGUI_PURE;

/**
 *
 */
bool
cgui_window_can_swap_grid(const cgui_window *window, cgui_grid *grid_1, cgui_grid *grid_2)
CGUI_NONNULL(1, 2, 3)
CGUI_PURE;

/**
 *
 */
cgui_cell *
cgui_window_focused_cell(const cgui_window *window)
CGUI_NONNULL_RETURN
CGUI_NONNULL(1)
CGUI_PURE;

/**
 *
 */
double
cgui_window_height(const cgui_window *window)
CGUI_NONNULL(1)
CGUI_PURE;

/**
 *
 */
bool
cgui_window_is_valid(const cgui_window *window)
CGUI_NONNULL(1)
CGUI_PURE;

/**
 *
 */
cgui_grid *
cgui_window_shown_grid(const cgui_window *window)
CGUI_NONNULL_RETURN
CGUI_NONNULL(1)
CGUI_PURE;

/**
 *
 */
struct cgui_window_state_flags
cgui_window_state(const cgui_window *window)
CGUI_NONNULL(1)
CGUI_PURE;

/**
 *
 */
double
cgui_window_width(const cgui_window *window)
CGUI_NONNULL(1)
CGUI_PURE;

/**
 *
 */
double
cgui_window_x(const cgui_window *window)
CGUI_NONNULL(1)
CGUI_PURE;

/**
 *
 */
xcb_window_t
cgui_window_x11_id(const cgui_window *window)
CGUI_NONNULL(1)
CGUI_PURE;

/**
 *
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

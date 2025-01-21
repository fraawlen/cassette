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
#include <cassette/cgui.h>
#include <stdbool.h>

#include "grid.h"

/************************************************************************************************************/
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

enum window_draw_level
{
	WINDOW_DRAW_NONE    = 0,
	WINDOW_DRAW_PARTIAL = 1, /* cells              */
	WINDOW_DRAW_FULL    = 2, /* cells + background */
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct window_accel
{
	char *name;
	void (*fn)(cgui_window *, int);
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct cgui_window
{
	enum cgui_window_type type;

	/* real geometry */

	double x;
	double y;
	double width;
	double height;

	/* requested dimensions (after _move() and _resize() but before the transform event)*/

	double tmp_x;
	double tmp_y;
	double tmp_width;
	double tmp_height;

	/* backend stuff */

	uint32_t x_serial;
	xcb_window_t x_id;
	xcb_pixmap_t x_buffer;
	cairo_surface_t *surface;
	cairo_t *drawable;

	/* contents */

	char *name;
	struct window_accel accels[CGUI_CONFIG_ACCELS];
	cgui_window *popup_parent;
	cgui_window *popup_child;
	cinputs *buttons;
	cinputs *touches;
	cref *grids;

	/* callbacks */

	void (*fn_close) (cgui_window *);
	void (*fn_draw)  (cgui_window *, unsigned long, unsigned long);
	void (*fn_focus) (cgui_window *, cgui_cell *);
	void (*fn_grid)  (cgui_window *, cgui_grid *);
	void (*fn_state) (cgui_window *, enum cgui_window_state_mask);

	/* states */

	cgui_grid *shown_grid;
	struct grid_area focus;
	struct cgui_window_state_flags state;
	enum window_draw_level draw;
	bool wait_present;
	bool async_present;
	bool valid;
	bool wait_resize;
	bool wait_move;
	unsigned long draw_timestamp;

	/* wm data */

	bool wm_move;
	bool wm_resize;
	double old_width;
	double old_height;
};

/************************************************************************************************************/
/* CONSTRUCTORS / DESTRUCTORS *******************************************************************************/
/************************************************************************************************************/

void
window_destroy(cgui_window *window)
CGUI_NONNULL(1)
CGUI_HIDDEN;

/************************************************************************************************************/
/* IMPURE METHODS *******************************************************************************************/
/************************************************************************************************************/

void
window_cancel_cell_events(cgui_window *window)
CGUI_NONNULL(1)
CGUI_HIDDEN;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_draw(cgui_window *window)
CGUI_NONNULL(1)
CGUI_HIDDEN;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_focus(cgui_window *window, struct grid_area area)
CGUI_NONNULL(1)
CGUI_HIDDEN;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_focus_lock(cgui_window *window, bool lock)
CGUI_NONNULL(1)
CGUI_HIDDEN;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_focus_pointer(cgui_window *window, double x, double y)
CGUI_NONNULL(1)
CGUI_HIDDEN;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_present(cgui_window *window)
CGUI_NONNULL(1)
CGUI_HIDDEN;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
window_process_cell_event(cgui_window *window, struct grid_area area, struct cgui_cell_event *event)
CGUI_NONNULL(1, 3)
CGUI_HIDDEN;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_repair(cgui_window *window)
CGUI_NONNULL(1)
CGUI_HIDDEN;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_set_async_present(cgui_window *window)
CGUI_NONNULL(1)
CGUI_HIDDEN;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_set_draw_level(cgui_window *window, enum window_draw_level draw)
CGUI_NONNULL(1)
CGUI_HIDDEN;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_update_size(cgui_window *window, double width, double height)
CGUI_NONNULL(1)
CGUI_HIDDEN;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_update_size_hints(cgui_window *window)
CGUI_NONNULL(1)
CGUI_HIDDEN;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_update_shown_grid(cgui_window *)
CGUI_NONNULL(1)
CGUI_HIDDEN;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_update_state(cgui_window *window, enum cgui_window_state_mask mask, bool value)
CGUI_NONNULL(1)
CGUI_HIDDEN;

/************************************************************************************************************/
/* PURE METHODS *********************************************************************************************/
/************************************************************************************************************/

struct grid_area
window_area_at_coords(const cgui_window *window, double x, double y)
CGUI_NONNULL(1)
CGUI_HIDDEN
CGUI_PURE;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct grid_area
window_cell_area(const cgui_window *window, const cgui_cell *cell)
CGUI_NONNULL(1, 2)
CGUI_HIDDEN
CGUI_PURE;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
window_cell_touches(const cgui_window *window, const cgui_cell *cell)
CGUI_NONNULL(1, 2)
CGUI_HIDDEN
CGUI_PURE;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgui_window *
window_popup_at_coords(double x, double y)
CGUI_NONNULL_RETURN
CGUI_PURE
CGUI_HIDDEN;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgui_window *
window_popup_last(void)
CGUI_NONNULL_RETURN
CGUI_PURE
CGUI_HIDDEN;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct grid_area
window_touch_area(const cgui_window *window, uint32_t id)
CGUI_NONNULL(1)
CGUI_HIDDEN
CGUI_PURE;

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
	/* geometry */

	double x;
	double y;
	double width;
	double height;

	/* backend stuff */

	uint32_t x_serial;
	xcb_window_t x_id;
	cairo_surface_t *surface;
	cairo_t *drawable;

	/* contents */

	char *name;
	struct window_accel accels[CGUI_CONFIG_ACCELS];
	cref *grids;

	/* callbacks */

	void (*fn_close) (cgui_window *);
	void (*fn_draw)  (cgui_window *, unsigned long);
	void (*fn_focus) (cgui_window *, cgui_cell *);
	void (*fn_grid)  (cgui_window *, cgui_grid *);
	void (*fn_state) (cgui_window *, enum cgui_window_state_mask);

	/* states */

	cgui_grid *shown_grid;
	struct grid_area focus;
	struct cgui_window_state_flags state;
	enum window_draw_level draw;
	bool wait_present;
	bool valid;
	bool size_requested;
	unsigned long draw_timestamp;
};

/************************************************************************************************************/
/* CONSTRUCTORS / DESTRUCTORS *******************************************************************************/
/************************************************************************************************************/

void
window_destroy(cgui_window *window)
CGUI_NONNULL(1);

/************************************************************************************************************/
/* IMPURE METHODS *******************************************************************************************/
/************************************************************************************************************/

void
window_draw(cgui_window *window)
CGUI_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_focus_pointer(cgui_window *window, double x, double y)
CGUI_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_present(cgui_window *window)
CGUI_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
window_process_cell_event(cgui_window *window, struct grid_area area, struct cgui_cell_event *event)
CGUI_NONNULL(1, 3);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_repair(cgui_window *window)
CGUI_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_resize(cgui_window *window, double width, double height)
CGUI_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_set_draw_level(cgui_window *window, enum window_draw_level draw)
CGUI_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_update_size_hints(cgui_window *window)
CGUI_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_update_state(cgui_window *window, enum cgui_window_state_mask mask, bool value)
CGUI_NONNULL(1);

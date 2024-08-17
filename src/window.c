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

#include <cairo/cairo.h>
#include <cairo/cairo-xcb.h>
#include <cassette/cgui.h>
#include <cassette/cobj.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "area.h"
#include "cell.h"
#include "config.h"
#include "grid.h"
#include "main.h"
#include "window.h"
#include "x11.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define WIDTH(GRID)  cgui_grid_width(GRID)  + CONFIG->window_frame.padding * 2
#define HEIGHT(GRID) cgui_grid_height(GRID) + CONFIG->window_frame.padding * 2

/* impure */

static void _cairo_destroy     (cgui_window *)                              CGUI_NONNULL(1);
static bool _cairo_setup       (cgui_window *, uint16_t, uint16_t)          CGUI_NONNULL(1);
static void _dummy_fn_accel    (cgui_window *, int)                         CGUI_NONNULL(1);
static void _dummy_fn_close    (cgui_window *)                              CGUI_NONNULL(1);
static void _dummy_fn_draw     (cgui_window *)                              CGUI_NONNULL(1);
static void _dummy_fn_focus    (cgui_window *, cgui_cell *)                 CGUI_NONNULL(1);
static void _dummy_fn_grid     (cgui_window *, cgui_grid *)                 CGUI_NONNULL(1);
static void _dummy_fn_state    (cgui_window *, enum cgui_window_state_mask) CGUI_NONNULL(1);
static void _update_shown_grid (cgui_window *)                              CGUI_NONNULL(1);

/* pure */

static struct cgui_box _box         (const cgui_window *)                         CGUI_NONNULL(1) CGUI_PURE;
static bool            _cairo_error (const cgui_window *)                         CGUI_NONNULL(1) CGUI_PURE;
static cgui_grid      *_min_grid    (const cgui_window *)                         CGUI_NONNULL(1) CGUI_PURE;
static void            _min_size    (const cgui_window *, uint16_t *, uint16_t *) CGUI_NONNULL(1, 2, 3);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static const struct cgui_window_state_flags _default_states = {false};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgui_window cgui_window_placeholder_instance =
{
	.x              = 0,
	.y              = 0,
	.width          = 0,
	.height         = 0,
	.x_serial       = 0,
	.x_id           = 0,
	.surface        = NULL,
	.drawable       = NULL,
	.name           = NULL,
	.grids          = CREF_PLACEHOLDER,
	.fn_close       = _dummy_fn_close,
	.fn_draw        = _dummy_fn_draw,
	.fn_focus       = _dummy_fn_focus,
	.fn_grid        = _dummy_fn_grid,
	.fn_state       = _dummy_fn_state,
	.state          = _default_states,
	.shown_grid     = CGUI_GRID_PLACEHOLDER,
	.focus          = {0, 0, 0, 0, CGUI_CELL_PLACEHOLDER},
	.draw           = WINDOW_DRAW_NONE,
	.wait_present   = false,
	.valid          = false,
	.size_requested = false,
	.accels         =
	{
		{NULL, _dummy_fn_accel},
		{NULL, _dummy_fn_accel},
		{NULL, _dummy_fn_accel},
		{NULL, _dummy_fn_accel},
		{NULL, _dummy_fn_accel},
		{NULL, _dummy_fn_accel},
		{NULL, _dummy_fn_accel},
		{NULL, _dummy_fn_accel},
		{NULL, _dummy_fn_accel},
		{NULL, _dummy_fn_accel},
		{NULL, _dummy_fn_accel},
		{NULL, _dummy_fn_accel},
	}
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cgui_window_activate(cgui_window *window)
{
	if (cgui_error() || !window->valid || window->state.active || cref_length(window->grids) == 0)
	{
		return;
	}

	/* if no grid is shown, select the first grid and resize the window */
	/* (if no custom size has been requested)                           */

	if (!window->shown_grid->valid)
	{
		window->shown_grid = (cgui_grid*)cref_ptr(window->grids, 0);
		if (!window->size_requested)
		{
			x11_window_resize(
				window->x_id,
				WIDTH(window->shown_grid),
				HEIGHT(window->shown_grid));
		}
	}

	window->size_requested = false;

	/* activate the window */

	x11_window_activate(window->x_id);
	window_update_size_hints(window);
	window_update_state(window, CGUI_WINDOW_ACTIVE, true);
	if (CONFIG->window_focus_on_activation)
	{
		window_update_state(window, CGUI_WINDOW_FOCUSED, true);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cairo_t *
cgui_window_cairo_drawable(const cgui_window *window)
{
	if (cgui_error() || !window->valid)
	{
		return NULL;
	}

	return window->drawable;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cairo_surface_t *
cgui_window_cairo_surface(const cgui_window *window)
{
	if (cgui_error() || !window->valid)
	{
		return NULL;
	}

	return window->surface;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgui_window_can_push_grid(const cgui_window *window, cgui_grid *grid)
{
	if (cgui_error()
	 || !window->valid
	 || !grid->valid
	 || cref_find(window->grids, grid, NULL)
	 || cgui_grid_compare_flex((cgui_grid*)cref_ptr(window->grids, 0), grid) == CGUI_GRID_FLEX_DIFFERENT)
	{
		return false;
	}

	CREF_FOR_EACH(window->grids, i)
	{
		switch (cgui_grid_compare_size((cgui_grid*)cref_ptr(window->grids, i), grid))
		{
			case CGUI_GRID_SIZE_EQUAL:
			case CGUI_GRID_SIZE_UNDEFINED:
			case CGUI_GRID_SIZE_INVALID:
				return false;

			default:
				break;
		}
	}

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgui_window_can_swap_grid(const cgui_window *window, cgui_grid *grid_1, cgui_grid *grid_2)
{
	if (cgui_error()
	 || !window->valid
	 || !grid_1->valid
	 || !grid_2->valid
	 ||  grid_2->used
	 || !cref_find(window->grids, grid_1, NULL)
	 ||  cref_find(window->grids, grid_2, NULL))
	{
		return false;
	}

	return cgui_grid_compare_flex(grid_1, grid_2) == CGUI_GRID_FLEX_SAME
	    && cgui_grid_compare_size(grid_1, grid_2) == CGUI_GRID_SIZE_EQUAL;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgui_window *
cgui_window_create(void)
{
	const  int16_t x      = 0;
	const  int16_t y      = 0;
	const uint16_t width  = 400;
	const uint16_t height = 400;

	cgui_window *window;

	if (cgui_error())
	{
		goto fail_main;
	}

	if (!(window = malloc(sizeof(cgui_window))))
	{
		goto fail_alloc;
	}

	if ((window->grids = cref_create()) == CREF_PLACEHOLDER)
	{
		goto fail_grids;
	}

	if (!x11_window_create(&window->x_id, x, y, width, height))
	{
		goto fail_backend;
	}

	if (!_cairo_setup(window, width, height))
	{
		goto fail_cairo;
	}

	if (!main_push_instance(main_windows(), window))
	{
		goto fail_push;
	}

	for (size_t i = 0; i < CGUI_CONFIG_ACCELS; i++)
	{
		window->accels[i].name = NULL;
		window->accels[i].fn   = _dummy_fn_accel;
	}

	cref_set_default_ptr(window->grids, CGUI_GRID_PLACEHOLDER);

	window->x              = x;
	window->y              = y;
	window->width          = width;
	window->height         = height;
	window->x_serial       = 0;
	window->name           = NULL;
	window->fn_close       = _dummy_fn_close;
	window->fn_draw        = _dummy_fn_draw;
	window->fn_focus       = _dummy_fn_focus;
	window->fn_grid        = _dummy_fn_grid;
	window->fn_state       = _dummy_fn_state;
	window->state          = _default_states;
	window->shown_grid     = CGUI_GRID_PLACEHOLDER;
	window->focus          = AREA_PLACEHOLDER;
	window->draw           = WINDOW_DRAW_NONE;
	window->wait_present   = false;
	window->valid          = true;
	window->size_requested = false;

	return window;

	/* errors */

fail_push:
	_cairo_destroy(window);
fail_cairo:
	x11_window_destroy(window->x_id);
fail_backend:
	cref_destroy(window->grids);
fail_grids:
	free(window);
fail_alloc:
	main_set_error(CERR_INSTANCE);
fail_main:
	return CGUI_WINDOW_PLACEHOLDER;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_window_deactivate(cgui_window *window)
{
	if (cgui_error() || !window->valid || !window->state.active)
	{
		return;
	}

	x11_window_deactivate(window->x_id);
	window_update_state(window, CGUI_WINDOW_ACTIVE, false);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_window_destroy(cgui_window *window)
{
	window->valid = false;
	if (!cgui_is_running())
	{
		window_destroy(window);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_window_disable(cgui_window *window)
{
	if (cgui_error() || !window->valid || window->state.disabled)
	{
		return;
	}

	window_update_state(window, CGUI_WINDOW_DISABLED,    true);
	window_update_state(window, CGUI_WINDOW_LOCKED_GRID, false);

	// TODO set focus
	// TODO send event to all cells
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_window_enable(cgui_window *window)
{
	if (cgui_error() || !window->valid || !window->state.disabled)
	{
		return;
	}

	window_update_state(window, CGUI_WINDOW_DISABLED,    false);

	// TODO send event to all cells
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgui_cell *
cgui_window_focused_cell(const cgui_window *window)
{
	if (cgui_error() || !window->valid || !window->focus.cell->valid)
	{
		return CGUI_CELL_PLACEHOLDER;
	}

	return window->focus.cell;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

uint16_t
cgui_window_height(const cgui_window *window)
{
	if (cgui_error() || !window->valid)
	{
		return 0;
	}

	return window->height;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgui_window_is_valid(const cgui_window *window)
{
	if (cgui_error())
	{
		return false;
	}

	return window->valid;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_window_move(cgui_window *window, int16_t x, int16_t y)
{
	if (cgui_error() || !window->valid)
	{
		return;
	}

	x11_window_move(window->x_id, x, y);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_window_non_urgent(cgui_window *window)
{
	if (cgui_error() || !window->valid)
	{
		return;
	}

	x11_window_set_urgency(window->x_id, false);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_window_on_close(cgui_window *window, void (*fn)(cgui_window *window))
{
	if (cgui_error() || !window->valid)
	{
		return;
	}
	
	window->fn_close = fn ? fn : _dummy_fn_close;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_window_on_draw(cgui_window *window, void (*fn)(cgui_window *window))
{
	if (cgui_error() || !window->valid)
	{
		return;
	}
	
	window->fn_draw = fn ? fn : _dummy_fn_draw;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_window_on_grid(cgui_window *window, void (*fn)(cgui_window *window, cgui_grid *grid))
{
	if (cgui_error() || !window->valid)
	{
		return;
	}
	
	window->fn_grid = fn ? fn : _dummy_fn_grid;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_window_on_state(cgui_window *window, void (*fn)(cgui_window *window, enum cgui_window_state_mask mask))
{
	if (cgui_error() || !window->valid)
	{
		return;
	}
	
	window->fn_state = fn ? fn : _dummy_fn_state;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_window_pull_grid(cgui_window *window, cgui_grid *grid)
{
	size_t i;

	if (cgui_error()
	 || !grid->valid
	 || !window->valid
	 ||  window->state.active
	 || !cref_find(window->grids, grid, &i))
	{
		return;
	}

	if (window->shown_grid == grid)
	{
		cgui_window_reset_grid(window);
	}

	cref_pull(window->grids, i);
	grid->used = false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_window_push_grid(cgui_window *window, cgui_grid *grid)
{
	if (!cgui_window_can_push_grid(window, grid) || window->state.active)
	{
		return;
	}

	cref_push(window->grids, grid);
	grid->used = true;

	main_set_error(cref_error(window->grids));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_window_redraw(cgui_window *window)
{
	if (cgui_error() || !window->valid)
	{
		return;
	}

	window_set_draw_level(window, WINDOW_DRAW_FULL);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_window_rename(cgui_window *window, const char *name)
{
	char *tmp;

	if (cgui_error() || !window->valid)
	{
		return;
	}

	if (!(tmp = strdup(name)))
	{
		main_set_error(CERR_MEMORY);
		return;
	}

	free(window->name);
	window->name = tmp;

	x11_window_rename(window->x_id, name);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_window_reset_grid(cgui_window *window)
{
	if (cgui_error() || !window->valid || window->state.active || !window->shown_grid->valid)
	{
		return;
	}

	window->shown_grid = CGUI_GRID_PLACEHOLDER;
	window->fn_grid(window, window->shown_grid);

	// TODO set focus
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_window_resize(cgui_window *window, uint16_t width, uint16_t height)
{
	uint16_t min_width;
	uint16_t min_height;

	if (cgui_error() || !window->valid)
	{
		return;
	}

	if (width == 0 || height == 0)
	{
		main_set_error(CERR_PARAM);
		return;
	}

	if (!window->state.active)
	{
		window->size_requested = true;
	}

	_min_size(window, &min_width, &min_height);

	width  = width  < min_width  ? min_width  : width;
	height = height < min_height ? min_height : height;

	x11_window_resize(window->x_id, width, height);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_window_set_accelerator(cgui_window *window, int id, const char *name, void (*fn)(cgui_window *window, int id))
{
	char *tmp;

	if (cgui_error() || !window->valid)
	{
		return;
	}

	if (id < 1 || id > 12)
	{
		main_set_error(CERR_PARAM);
		return;
	}

	if (!(tmp = strdup(name)))
	{
		main_set_error(CERR_MEMORY);
		return;
	}

	free(window->accels[--id].name);
	window->accels[id].name = tmp;
	window->accels[id].fn   = fn;

	x11_window_set_accel(window->x_id, id, fn ? name : NULL);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_window_set_type(cgui_window *window, enum cgui_window_type type)
{
	if (cgui_error() || !window->valid)
	{
		return;
	}
	
	x11_window_set_type(window->x_id, type);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgui_grid *
cgui_window_shown_grid(const cgui_window *window)
{
	if (cgui_error() || !window->valid || !window->shown_grid->valid)
	{
		return CGUI_GRID_PLACEHOLDER;
	}

	return window->shown_grid;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct cgui_window_state_flags
cgui_window_state(const cgui_window *window)
{
	if (cgui_error() || !window->valid)
	{
		return _default_states;
	}

	return window->state;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_window_swap_grid(cgui_window *window, cgui_grid *grid_1, cgui_grid *grid_2)
{
	if (!cgui_window_can_swap_grid(window, grid_1, grid_2))
	{
		return;
	}

	cref_pull(window->grids, grid_1);
	cref_push(window->grids, grid_2);

	grid_1->used = false;
	grid_2->used = true;

	/* extra ops when the swap happens on a visible grid */

	if (window->shown_grid != grid_1)
	{
		return;
	}

	window->shown_grid = grid_2;
	window->fn_grid(window, grid_2);
	window_set_draw_level(window, WINDOW_DRAW_FULL);

	// TODO refocus
	// TODO update geometries
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_window_tack(cgui_window *window, cgui_window *window_under)
{
	if (cgui_error() || !window->valid)
	{
		return;
	}

	x11_window_set_transient(window->x_id, window_under->x_id);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_window_untack(cgui_window *window)
{
	if (cgui_error() || !window->valid)
	{
		return;
	}
	
	x11_window_set_transient(window->x_id, XCB_WINDOW_NONE);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_window_urgent(cgui_window *window)
{
	if (cgui_error() || !window->valid)
	{
		return;
	}

	x11_window_set_urgency(window->x_id, false);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

uint16_t
cgui_window_width(const cgui_window *window)
{
	if (cgui_error() || !window->valid)
	{
		return 0;
	}

	return window->width;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

int16_t
cgui_window_x(const cgui_window *window)
{
	if (cgui_error() || !window->valid)
	{
		return 0;
	}

	return window->x;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

xcb_window_t
cgui_window_x11_id(const cgui_window *window)
{
	if (cgui_error() || !window->valid)
	{
		return 0;
	}

	return window->x_id;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

int16_t
cgui_window_y(const cgui_window *window)
{
	if (cgui_error() || !window->valid)
	{
		return 0;
	}

	return window->y;
}

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

void
window_destroy(cgui_window *window)
{
	if (window == CGUI_WINDOW_PLACEHOLDER || window->valid)
	{
		return;
	}

	for (size_t i = 0; i < CGUI_CONFIG_ACCELS; i++)
	{
		free(window->accels[i].name);
	}

	_cairo_destroy(window);
	main_pull_instance(main_windows(), window);
	x11_window_destroy(window->x_id);
	cref_destroy(window->grids);
	free(window->name);
	free(window);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_draw(cgui_window *window)
{
	struct cgui_zone zone =
	{
		.drawable = window->drawable,
		.x        = 0,
		.y        = 0,
		.width    = window->width,
		.height   = window->height,
	};

	if (!window->state.mapped || window->draw == WINDOW_DRAW_NONE)
	{
		return;
	}

	cairo_set_operator(window->drawable, CAIRO_OPERATOR_SOURCE);

	/* draw background */

	if (window->draw == WINDOW_DRAW_FULL)
	{
		cgui_box_draw(_box(window), zone);
	}

	/* draw cells */

	// TODO

	/* check if there is a new draw cycle requested */

	window->draw         = WINDOW_DRAW_NONE;
	window->wait_present = false;

	// TODO

	/* run callback */

	window->fn_draw(window);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_present(cgui_window *window)
{
	if (!window->state.mapped || window->wait_present || window->draw == WINDOW_DRAW_NONE)
	{
		return;
	}

	x11_window_present(window->x_id, ++window->x_serial);

	window->wait_present = true; /* is set back to false after draw */
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_repair(cgui_window *window)
{
	if (!window->valid)
	{
		return;
	}

	if (_cairo_error(window))
	{
		_cairo_destroy(window);
		_cairo_setup(window, window->width, window->height);
	}

	cref_repair(window->grids);
	window_set_draw_level(window, WINDOW_DRAW_FULL);
	window_resize(window, window->width, window->height);
	x11_window_update_state_hints(window->x_id, window->state);
	x11_window_rename(window->x_id, window->name);
	for (size_t i = 0; i < CGUI_CONFIG_ACCELS; i++)
	{
		x11_window_set_accel(window->x_id, i, window->accels[i].fn ? window->accels[i].name : NULL);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_resize(cgui_window *window, uint16_t width, uint16_t height)
{
	window->width  = width;
	window->height = height;

	cairo_surface_flush(window->surface);
	cairo_xcb_surface_set_size(window->surface, width, height);
	if (_cairo_error(window))
	{
		main_set_error(CERR_CAIRO);
		return;
	}
	
	_update_shown_grid(window);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_set_draw_level(cgui_window *window, enum window_draw_level draw)
{
	if (window->draw < draw)
	{
		window->draw = draw;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_update_size_hints(cgui_window *window)
{
	uint16_t w_min;
	uint16_t w_max;
	uint16_t h_min;
	uint16_t h_max;

	if (window->state.locked_grid)
	{
		w_min = WIDTH(window->shown_grid);
		h_min = HEIGHT(window->shown_grid);
	}
	else
	{
		_min_size(window, &w_min, &h_min);
	}

	w_max = ((cgui_grid*)cref_ptr(window->grids, 0))->col_flex > 0.0 ? UINT16_MAX : w_min;
	h_max = ((cgui_grid*)cref_ptr(window->grids, 0))->row_flex > 0.0 ? UINT16_MAX : h_min;
	
	x11_window_update_size_hints(window->x_id, w_min, h_min, w_max, h_max);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_update_state(cgui_window *window, enum cgui_window_state_mask mask, bool value)
{
	bool old;

	if (cgui_error())
	{
		return;
	}

	switch (mask)
	{
		case CGUI_WINDOW_ACTIVE:
			old = window->state.active;
			window->state.active = value;
			break;

		case CGUI_WINDOW_MAPPED:
			old = window->state.mapped;
			window->state.mapped = value;
			break;

		case CGUI_WINDOW_FOCUSED:
			old = window->state.focused;
			window->state.focused = value;
			break;

		case CGUI_WINDOW_DISABLED:
			old = window->state.disabled;
			window->state.disabled = value;
			break;

		case CGUI_WINDOW_LOCKED_GRID:
			old = window->state.locked_grid;
			window->state.locked_grid = value;
			break;

		case CGUI_WINDOW_LOCKED_FOCUS:
			old = window->state.locked_focus;
			window->state.locked_focus = value;
			break;

		default:
			return;
	}

	if (old == value)
	{
		return;
	}

	if ((CONFIG->window_enable_locked   && mask == CGUI_WINDOW_LOCKED_GRID)
	 || (CONFIG->window_enable_disabled && mask == CGUI_WINDOW_DISABLED)
	 || (CONFIG->window_enable_focused  && mask == CGUI_WINDOW_FOCUSED))
	{
		window_set_draw_level(window, WINDOW_DRAW_FULL);	
	}

	x11_window_update_state_hints(window->x_id, window->state);
	window->fn_state(window, mask);
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static struct cgui_box
_box(const cgui_window *window)
{
	if (!window->state.focused)
	{
		return CONFIG->window_frame;
	}

	if (CONFIG->window_enable_disabled && window->state.disabled)
	{
		return CONFIG->window_frame_disabled;
	}

	if (CONFIG->window_enable_locked && window->state.locked_grid)
	{
		return CONFIG->window_frame_locked;
	}

	if (CONFIG->window_enable_focused)
	{
		return CONFIG->window_frame_focused;
	}

	return CONFIG->window_frame;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_cairo_destroy(cgui_window *window)
{
	cairo_destroy(window->drawable);
	cairo_surface_destroy(window->surface);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
_cairo_error(const cgui_window *window)
{
	return cairo_surface_status(window->surface) != CAIRO_STATUS_SUCCESS
	    || cairo_status(window->drawable)        != CAIRO_STATUS_SUCCESS;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
_cairo_setup(cgui_window *window, uint16_t width, uint16_t height)
{
	window->surface = cairo_xcb_surface_create(
		x11_connection(),
		window->x_id,
		x11_visual(),
		width,
		height);

	window->drawable = cairo_create(window->surface);

	if (_cairo_error(window))
	{
		main_set_error(CERR_CAIRO);
		return false;
	}

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_dummy_fn_accel(cgui_window *window, int id)
{
	(void)window;
	(void)id;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_dummy_fn_close(cgui_window *window)
{
	cgui_window_deactivate(window);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_dummy_fn_draw(cgui_window *window)
{
	(void)window;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_dummy_fn_focus(cgui_window *window, cgui_cell *cell)
{
	(void)window;
	(void)cell;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_dummy_fn_grid(cgui_window *window, cgui_grid *grid)
{
	(void)window;
	(void)grid;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_dummy_fn_state(cgui_window *window, enum cgui_window_state_mask mask)
{
	(void)window;
	(void)mask;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static cgui_grid *
_min_grid(const cgui_window *window)
{
	cgui_grid *grid_min;
	cgui_grid *grid;

	grid_min = (cgui_grid*)cref_ptr(window->grids, 0);
	for (size_t i = 1; i < cref_length(window->grids); i++)
	{
		grid = (cgui_grid*)cref_ptr(window->grids, i);
		if (cgui_grid_compare_size(grid, grid_min) == CGUI_GRID_SIZE_SMALLER)
		{
			grid_min = grid;
		}
	}

	return grid_min;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_min_size(const cgui_window *window, uint16_t *width, uint16_t *height)
{
	cgui_grid *grid;

	 grid   = _min_grid(window);
	*width  = WIDTH(grid);
	*height = HEIGHT(grid);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_update_shown_grid(cgui_window *window)
{
	cgui_grid *grid_old;
	cgui_grid *grid;

	if (window->state.locked_grid || cref_length(window->grids) < 2)
	{
		return;
	}

	/* find biggest grid that could fit in current window dimensions */

	grid_old           = window->shown_grid;
	window->shown_grid = _min_grid(window);

	CREF_FOR_EACH(window->grids, i)
	{
		grid = (cgui_grid*)cref_ptr(window->grids, i);
		if (cgui_grid_compare_size(grid, window->shown_grid) == CGUI_GRID_SIZE_BIGGER
		 && WIDTH(grid)  <= window->width
		 && HEIGHT(grid) <= window->height)
		{
			window->shown_grid = grid;
		}
	}

	/* updates to do if the shown grid changed */

	if (grid_old == window->shown_grid)
	{
		return;
	}

	cgui_window_swap_grid(window, grid_old, grid_old->ref);
	window->fn_grid(window, window->shown_grid);
	window_set_draw_level(window, WINDOW_DRAW_FULL);

	// TODO refocus
}

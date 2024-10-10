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
#include <float.h>
#include <limits.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "cell.h"
#include "config.h"
#include "grid.h"
#include "main.h"
#include "window.h"
#include "util.h"
#include "x11.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define WIDTH(GRID)  cgui_grid_width(GRID)  + CONFIG->window_padding * 2
#define HEIGHT(GRID) cgui_grid_height(GRID) + CONFIG->window_padding * 2

/* impure */

static void cairo_data_destroy (cgui_window *)                                  CGUI_NONNULL(1);
static bool cairo_setup        (cgui_window *, double, double)                  CGUI_NONNULL(1);
static void draw_area          (cgui_window *, struct grid_area, unsigned long) CGUI_NONNULL(1);
static void dummy_fn_accel     (cgui_window *, int)                             CGUI_NONNULL(1);
static void dummy_fn_close     (cgui_window *)                                  CGUI_NONNULL(1);
static void dummy_fn_draw      (cgui_window *, unsigned long, unsigned long)    CGUI_NONNULL(1);
static void dummy_fn_focus     (cgui_window *, cgui_cell *)                     CGUI_NONNULL(1, 2);
static void dummy_fn_grid      (cgui_window *, cgui_grid *)                     CGUI_NONNULL(1, 2);
static void dummy_fn_state     (cgui_window *, enum cgui_window_state_mask)     CGUI_NONNULL(1);
static void refocus            (cgui_window *)                                  CGUI_NONNULL(1);
static void update_shown_grid  (cgui_window *)                                  CGUI_NONNULL(1);

/* pure */

static bool             cairo_error    (const cgui_window *)                                         CGUI_NONNULL(1) CGUI_PURE;
static struct cgui_box  cell_frame     (const cgui_window *, struct grid_area)                       CGUI_NONNULL(1) CGUI_PURE;
static struct cgui_box  frame          (const cgui_window *)                                         CGUI_NONNULL(1) CGUI_PURE;
static cgui_grid       *min_grid       (const cgui_window *)                                         CGUI_NONNULL(1) CGUI_PURE;
void                    size_limits    (const cgui_window *, double *, double *, double *, double *) CGUI_NONNULL(1, 2, 3, 4, 5);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static const struct cgui_window_state_flags default_states = {false};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgui_window cgui_window_placeholder_instance =
{
	.x              = 0.0,
	.y              = 0.0,
	.width          = 0.0,
	.height         = 0.0,
	.x_serial       = 0,
	.x_id           = 0,
	.x_buffer       = 0,
	.surface        = NULL,
	.drawable       = NULL,
	.name           = NULL,
	.grids          = CREF_PLACEHOLDER,
	.buttons        = CINPUTS_PLACEHOLDER,
	.touches        = CINPUTS_PLACEHOLDER,
	.fn_close       = dummy_fn_close,
	.fn_draw        = dummy_fn_draw,
	.fn_focus       = dummy_fn_focus,
	.fn_grid        = dummy_fn_grid,
	.fn_state       = dummy_fn_state,
	.state          = default_states,
	.shown_grid     = CGUI_GRID_PLACEHOLDER,
	.draw           = WINDOW_DRAW_NONE,
	.wait_present   = false,
	.async_present  = false,
	.valid          = false,
	.size_requested = false,
	.wait_resize    = false,
	.wm_move        = false,
	.wm_resize      = false,
	.old_width      = 0.0,
	.old_height     = 0.0,
	.draw_timestamp = 0,
	.focus          =
	{
		.cell   = CGUI_CELL_PLACEHOLDER,
		.col    = 0,
		.row    = 0,
		.n_cols = 0,
		.n_rows = 0,
		.x      = 0.0,
		.y      = 0.0,
		.width  = 0.0,
		.height = 0.0,
	},
	.accels         =
	{
		{.name = NULL, .fn = dummy_fn_accel},
		{.name = NULL, .fn = dummy_fn_accel},
		{.name = NULL, .fn = dummy_fn_accel},
		{.name = NULL, .fn = dummy_fn_accel},
		{.name = NULL, .fn = dummy_fn_accel},
		{.name = NULL, .fn = dummy_fn_accel},
		{.name = NULL, .fn = dummy_fn_accel},
		{.name = NULL, .fn = dummy_fn_accel},
		{.name = NULL, .fn = dummy_fn_accel},
		{.name = NULL, .fn = dummy_fn_accel},
		{.name = NULL, .fn = dummy_fn_accel},
		{.name = NULL, .fn = dummy_fn_accel},
	},
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

	/* activate the window */

	window->size_requested = false;
	window->draw_timestamp = 0;

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
	const double x      = 0;
	const double y      = 0;
	const double width  = 400;
	const double height = 400;

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

	if ((window->buttons = cinputs_create(CGUI_CONFIG_BUTTONS)) == CINPUTS_PLACEHOLDER)
	{
		goto fail_buttons;
	}

	if ((window->touches = cinputs_create(CGUI_CONFIG_TOUCHES)) == CINPUTS_PLACEHOLDER)
	{
		goto fail_touches;
	}

	if (!x11_window_create(&window->x_id, &window->x_buffer, x, y, width, height))
	{
		goto fail_backend;
	}

	if (!cairo_setup(window, width, height))
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
		window->accels[i].fn   = dummy_fn_accel;
	}

	cref_set_default_ptr(window->grids, CGUI_GRID_PLACEHOLDER);
	cinputs_set_default_ptr(window->touches, CGUI_CELL_PLACEHOLDER);

	window->x              = x;
	window->y              = y;
	window->width          = width;
	window->height         = height;
	window->x_serial       = 0;
	window->name           = NULL;
	window->fn_close       = dummy_fn_close;
	window->fn_draw        = dummy_fn_draw;
	window->fn_focus       = dummy_fn_focus;
	window->fn_grid        = dummy_fn_grid;
	window->fn_state       = dummy_fn_state;
	window->state          = default_states;
	window->shown_grid     = CGUI_GRID_PLACEHOLDER;
	window->focus          = GRID_AREA_NONE;
	window->draw           = WINDOW_DRAW_NONE;
	window->wait_present   = false;
	window->async_present  = false;
	window->valid          = true;
	window->size_requested = false;
	window->wait_resize    = false;
	window->wm_move        = false;
	window->wm_resize      = false;
	window->old_width      = width;
	window->old_height     = height;
	window->draw_timestamp = 0;

	return window;

	/* errors */

fail_push:
	cairo_data_destroy(window);
fail_cairo:
	x11_window_destroy(window->x_id, window->x_buffer);
fail_backend:
	cref_destroy(window->grids);
fail_touches:
	cinputs_destroy(window->touches);
fail_buttons:
	cinputs_destroy(window->buttons);
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
	window_focus(window, GRID_AREA_NONE);
	window_cancel_cell_events(window);
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
	window_focus(window, GRID_AREA_NONE);
	window_cancel_cell_events(window);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_window_enable(cgui_window *window)
{
	if (cgui_error() || !window->valid || !window->state.disabled)
	{
		return;
	}

	window_update_state(window, CGUI_WINDOW_DISABLED, false);
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

double
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
cgui_window_move(cgui_window *window, double x, double y)
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
	
	window->fn_close = fn ? fn : dummy_fn_close;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_window_on_draw(cgui_window *window, void (*fn)(cgui_window *window, unsigned long delay_1, unsigned long delay_2))
{
	if (cgui_error() || !window->valid)
	{
		return;
	}
	
	window->fn_draw = fn ? fn : dummy_fn_draw;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_window_on_grid(cgui_window *window, void (*fn)(cgui_window *window, cgui_grid *grid))
{
	if (cgui_error() || !window->valid)
	{
		return;
	}
	
	window->fn_grid = fn ? fn : dummy_fn_grid;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_window_on_state(cgui_window *window, void (*fn)(cgui_window *window, enum cgui_window_state_mask mask))
{
	if (cgui_error() || !window->valid)
	{
		return;
	}
	
	window->fn_state = fn ? fn : dummy_fn_state;
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
	window_focus(window, GRID_AREA_NONE);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_window_resize(cgui_window *window, double width, double height)
{
	double min_w;
	double min_h;
	double max_w;
	double max_h;

	if (cgui_error() || !window->valid || cref_length(window->grids) == 0)
	{
		return;
	}

	if (!window->state.active)
	{
		window->size_requested = true;
	}

	size_limits(window, &min_w, &min_h, &max_w, &max_h);
	x11_window_resize(
		window->x_id,
		util_limit(width,  min_w, max_w),
		util_limit(height, min_h, max_h));
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
		return default_states;
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
	refocus(window);
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

double
cgui_window_width(const cgui_window *window)
{
	if (cgui_error() || !window->valid)
	{
		return 0;
	}

	return window->width;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

double
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

double
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

struct grid_area
window_area_at_coords(const cgui_window *window, double x, double y)
{
	struct grid_area area;
	struct cgui_box box;

	x -= CONFIG->window_padding;
	y -= CONFIG->window_padding;

	CREF_FOR_EACH(window->shown_grid->areas, i)
	{
		area = *(struct grid_area*)cref_ptr(window->shown_grid->areas, i);
		box  = cell_frame(window, area);
		if (cgui_box_is_in(box, x, y, area.x, area.y, area.width, area.height, window->drawable))
		{
			return area;
		}
	}

	return GRID_AREA_NONE;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_cancel_cell_events(cgui_window *window)
{
	struct grid_area area;
	struct cgui_cell_event event;

	CREF_FOR_EACH(window->shown_grid->areas, i)
	{
		event.type = CGUI_CELL_EVENT_CANCEL;
		area = *(struct grid_area*)cref_ptr(window->shown_grid->areas, i);
		window_process_cell_event(window, area, &event);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct grid_area
window_cell_area(const cgui_window *window, const cgui_cell *cell)
{
	struct grid_area area;

	CREF_FOR_EACH(window->shown_grid->areas, i)
	{
		area = *(struct grid_area*)cref_ptr(window->shown_grid->areas, i);
		if (cell == area.cell)
		{
			return area;
		}
	}

	return GRID_AREA_NONE;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
window_cell_touches(const cgui_window *window, const cgui_cell *cell)
{
	size_t n = 0;

	CINPUTS_FOR_EACH(window->touches, i)
	{
		n += cell == cinputs_ptr(window->touches, i) ? 1 : 0;
	}

	return n;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

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

	cairo_data_destroy(window);
	main_pull_instance(main_windows(), window);
	x11_window_destroy(window->x_id, window->x_buffer);
	cinputs_destroy(window->buttons);
	cinputs_destroy(window->touches);
	cref_destroy(window->grids);
	free(window->name);
	free(window);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_draw(cgui_window *window)
{
	unsigned long timestamp;
	unsigned long delay;

	if (!window->state.mapped || window->draw == WINDOW_DRAW_NONE)
	{
		return;
	}

	if (!CONFIG->window_enable_partial_redraws)
	{
		window->draw = WINDOW_DRAW_FULL;
	}

	timestamp = util_time();
	delay     = window->draw_timestamp == 0 ? 0 : timestamp - window->draw_timestamp;

	/* draw border and background */

	if (window->draw == WINDOW_DRAW_FULL)
	{
		cairo_new_path(window->drawable);
		cairo_set_operator(window->drawable, CAIRO_OPERATOR_SOURCE);
		cgui_box_draw(frame(window), 0.0, 0.0, window->width, window->height, window->drawable);
	}

	/* draw cells */
	
	CREF_FOR_EACH(window->shown_grid->areas, i)
	{
		draw_area(window, *(struct grid_area*)cref_ptr(window->shown_grid->areas, i), delay);
	}

	/* end */

	window->draw           = WINDOW_DRAW_NONE;
	window->draw_timestamp = timestamp;

	cairo_new_path(window->drawable);
	window->fn_draw(window, delay, util_time() - timestamp);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_focus(cgui_window *window, struct grid_area area)
{
	struct cgui_cell_event event_unfoc =
	{
		.type = CGUI_CELL_EVENT_FOCUS_LOSE,
	};

	struct cgui_cell_event event_info =
	{
		.type              = CGUI_CELL_EVENT_FOCUS_INFO,
		.focus_info_cell   = area.cell,
		.focus_info_x      = area.x,
		.focus_info_y      = area.y,
		.focus_info_width  = area.width,
		.focus_info_height = area.height,
	};

	if (window->focus.cell == area.cell)
	{
		return;
	}

	/* first, unfocus the previously unfocused cell (if any). It's assumed that the focus event was   */
	/* sent by the caller of this function. If a GRID_AREA_NONE is given (with an invalid cell), then */
	/* it means that the window loses focus completely.                                               */

	window_process_cell_event(window, window->focus, &event_unfoc);

	window->focus = area;

	/* the following line is here in case the focused cell is a meta-cell, as in a cell holding other */
	/* cells with its own internal focus system. The focus_info event that is sent is meant for them. */
	/* It queries the meta-cell for infomation about it's internal focus if any, focus info that will */
	/* then be used to set the window's focus hints. Non-meta cells should just reject that event. In */
	/* this situation, the cell's grid-level geometry is used for hints as set by the event's default */
	/* values.                                                                                        */

	window_process_cell_event(window, window->focus, &event_info);

	/* callbacks and hint updates */

	window->fn_focus(window, window->focus.cell);
	x11_window_update_focus_hints(
		window->x_id,
		event_info.focus_info_x,
		event_info.focus_info_y,
		event_info.focus_info_width,
		event_info.focus_info_height);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_focus_lock(cgui_window *window, bool lock)
{
	struct cgui_cell_event event =
	{
		.type = lock ? CGUI_CELL_EVENT_FOCUS_LOCK : CGUI_CELL_EVENT_FOCUS_UNLOCK,
	};

	if (!window->focus.cell->valid || lock == window->state.locked_focus)
	{
		return;
	}

	window_process_cell_event(window, window->focus, &event);
	window_update_state(window, CGUI_WINDOW_LOCKED_FOCUS, lock);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_focus_pointer(cgui_window *window, double x, double y)
{
	struct grid_area area;
	struct cgui_cell_event event =
	{
		.type    = CGUI_CELL_EVENT_FOCUS_GAIN_BY_POINTER,
		.focus_x = x,
		.focus_y = y,
	};

	/* do not update focus if there is an ongoing drag or if the focus is locked */

	if (window->state.locked_focus || cinputs_load(window->buttons) > 0)
	{
		return;
	}

	/* generate and send focus event */

	area = window_area_at_coords(window, x, y);
	if (window_process_cell_event(window, area, &event))
	{
		window_focus(window, area);
	}
	else if (!CONFIG->persistent_pointer)
	{
		window_focus(window, GRID_AREA_NONE);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_present(cgui_window *window)
{
	if (!window->state.mapped || window->wait_present || window->draw == WINDOW_DRAW_NONE)
	{
		return;
	}

	if (CONFIG->alt_present)
	{
		window_draw(window);
	}

	x11_window_present(window->x_id, window->x_buffer, ++window->x_serial, window->async_present);

	window->wait_present  = true; 
	window->async_present = false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
window_process_cell_event(cgui_window *window, struct grid_area area, struct cgui_cell_event *event)
{
	if (!area.cell->valid)
	{
		return false;
	}

	/* filter out some events depending on the window state */

	if (!window->state.disabled)
	{
		goto skip_filter;
	}

	switch (event->type)
	{
		// TODO

		default:
			return false;
	}

skip_filter:

	/* fill out common fields */
	
	event->msg        = CGUI_CELL_MSG_NONE;
	event->x          = area.x + CONFIG->window_padding;
	event->y          = area.y + CONFIG->window_padding;
	event->width      = area.width;
	event->height     = area.height;
	event->frame      = cell_frame(window, area);
	event->drawable   = window->drawable;
	event->is_focused = window->focus.cell == area.cell;

	/* send event */

	area.cell->fn_event(area.cell, event);

	/* process msg */

	switch (event->msg)
	{
		case CGUI_CELL_MSG_REJECT:
			return false;

		case CGUI_CELL_MSG_LOCK:
			if (area.cell == window->focus.cell && CONFIG->cell_auto_lock)
			{
				window_focus_lock(window, true);
			}
			break;

		case CGUI_CELL_MSG_UNLOCK:
			if (area.cell == window->focus.cell && CONFIG->cell_auto_lock)
			{
				window_focus_lock(window, false);
			}
			break;

		case CGUI_CELL_MSG_NONE:
		default:
			break;
	}

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_repair(cgui_window *window)
{
	if (!window->valid)
	{
		return;
	}

	if (cairo_error(window))
	{
		cairo_data_destroy(window);
		cairo_setup(window, window->width, window->height);
	}

	cref_repair(window->grids);
	cinputs_repair(window->buttons);
	cinputs_repair(window->touches);
	window_set_draw_level(window, WINDOW_DRAW_FULL);
	window_update_size(window, window->width, window->height);
	window_update_size_hints(window);
	x11_window_update_state_hints(window->x_id, window->state);
	x11_window_rename(window->x_id, window->name);
	for (size_t i = 0; i < CGUI_CONFIG_ACCELS; i++)
	{
		x11_window_set_accel(window->x_id, i, window->accels[i].fn ? window->accels[i].name : NULL);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_set_async_present(cgui_window *window)
{
	if (CONFIG->async_present)
	{
		window->async_present = true;
		window->wait_present  = false;
	}
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

struct grid_area
window_touch_area(const cgui_window *window, uint32_t id)
{
	size_t i;

	if (!cinputs_find(window->touches, id, &i))
	{
		return GRID_AREA_NONE;
	}

	 return window_cell_area(window, (cgui_cell*)cinputs_ptr(window->touches, i));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_update_size(cgui_window *window, double width, double height)
{
	window->width  = width;
	window->height = height;

	cairo_surface_flush(window->surface);
	if (CONFIG->alt_present)
	{
		x11_window_update_buffer(window->x_id, &window->x_buffer, width, height);
		cairo_xcb_surface_set_drawable(window->surface, window->x_buffer, width, height);
	}
	else
	{
		cairo_xcb_surface_set_size(window->surface, width, height);
	}

	if (cairo_error(window))
	{
		main_set_error(CERR_CAIRO);
		return;
	}
	
	update_shown_grid(window);
	grid_update_geometry(
		window->shown_grid,
		window->width  - CONFIG->window_padding * 2,
		window->height - CONFIG->window_padding * 2);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_update_size_hints(cgui_window *window)
{
	double min_w;
	double min_h;
	double max_w;
	double max_h;

	size_limits(window, &min_w, &min_h, &max_w, &max_h);
	if (window->state.locked_grid)
	{
		min_w = WIDTH(window->shown_grid);
		min_h = HEIGHT(window->shown_grid);
	}

	x11_window_update_size_hints(window->x_id, min_w, min_h, max_w, max_h);
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

static void
cairo_data_destroy(cgui_window *window)
{
	cairo_destroy(window->drawable);
	cairo_surface_destroy(window->surface);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
cairo_error(const cgui_window *window)
{
	return cairo_surface_status(window->surface) != CAIRO_STATUS_SUCCESS
	    || cairo_status(window->drawable)        != CAIRO_STATUS_SUCCESS;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
cairo_setup(cgui_window *window, double width, double height)
{
	window->surface = cairo_xcb_surface_create(
		x11_connection(),
		CONFIG->alt_present ? window->x_buffer : window->x_id,
		x11_visual(),
		util_limit(width,  0.0, INT_MAX - 1),
		util_limit(height, 0.0, INT_MAX - 1));

	window->drawable = cairo_create(window->surface);

	if (cairo_error(window))
	{
		main_set_error(CERR_CAIRO);
		return false;
	}

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static struct cgui_box
cell_frame(const cgui_window *window, struct grid_area area)
{
	struct cgui_box box   =
	{
		.corner           =   {0},
		.size_corner      = {0.0},
		.size_outline     =  0.0,
		.size_border      =  0.0,
		.color_outline    = {0.0},
		.color_border     = {0.0},
		.color_background = {0.0},
		.shape_outline    = false,
		.shape_border     = false,
		.draw             = false,
	};

	area.cell->fn_frame(area.cell, &box);

	/* adjust corners */

	if (area.col == 0 && area.row == 0)
	{
		cgui_box_pad_corner(&box, frame(window), CONFIG->window_padding, 0);
	}

	if (area.col + area.n_cols == window->shown_grid->n_cols && area.row == 0)
	{
		cgui_box_pad_corner(&box, frame(window), CONFIG->window_padding, 1);
	}

	if (area.col + area.n_cols == window->shown_grid->n_cols
	 && area.row + area.n_rows == window->shown_grid->n_rows)
	{
		cgui_box_pad_corner(&box, frame(window), CONFIG->window_padding, 2);
	}

	if (area.col == 0 && area.row + area.n_rows == window->shown_grid->n_rows)
	{
		cgui_box_pad_corner(&box, frame(window), CONFIG->window_padding, 3);
	}

	/* end */

	return box;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
draw_area(cgui_window *window, struct grid_area area, unsigned long delay)
{
	struct cgui_cell_context context =
	{
		.delay    = delay,
		.drawable = window->drawable,
		.x        = area.x + CONFIG->window_padding,
		.y        = area.y + CONFIG->window_padding,
		.width    = area.width,
		.height   = area.height,
	};

	if (window->draw != WINDOW_DRAW_FULL && !area.cell->draw)
	{
		return;
	}

	/* draw */

	context.frame   = cell_frame(window, area);
	area.cell->draw = false;

	cairo_new_path(window->drawable);
	cairo_save(window->drawable);
	area.cell->fn_draw(area.cell, context);
	cairo_restore(window->drawable);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
dummy_fn_accel(cgui_window *window, int id)
{
	(void)window;
	(void)id;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
dummy_fn_close(cgui_window *window)
{
	cgui_window_deactivate(window);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
dummy_fn_draw(cgui_window *window, unsigned long delay_1, unsigned long delay_2)
{
	(void)window;
	(void)delay_1;
	(void)delay_2;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
dummy_fn_focus(cgui_window *window, cgui_cell *cell)
{
	(void)window;
	(void)cell;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
dummy_fn_grid(cgui_window *window, cgui_grid *grid)
{
	(void)window;
	(void)grid;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
dummy_fn_state(cgui_window *window, enum cgui_window_state_mask mask)
{
	(void)window;
	(void)mask;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static struct cgui_box
frame(const cgui_window *window)
{
	struct cgui_box box =
	{
		.size_outline     =  0.0,
		.size_border      = CONFIG->window_size_border,
		.color_outline    = {0.0},
		.color_border     = CONFIG->window_color_border,
		.color_background = CONFIG->window_color_background,
		.shape_outline    = false,
		.shape_border     = false,
		.draw             = true,
		.draw_foreground  = false,
		.corner =
		{
			CONFIG->window_corner[0],
			CONFIG->window_corner[1],
			CONFIG->window_corner[2],
			CONFIG->window_corner[3],
		},
		.size_corner =
		{
			CONFIG->window_size_corner[0],
			CONFIG->window_size_corner[1],
			CONFIG->window_size_corner[2],
			CONFIG->window_size_corner[3],
		},
	};

	/* select special border color if window is focused */

	if (!window->state.focused)
	{
		return box;
	}

	if (CONFIG->window_enable_disabled && window->state.disabled)
	{
		box.color_border = CONFIG->window_color_border_disabled;
	}

	if (CONFIG->window_enable_locked && window->state.locked_grid)
	{
		box.color_border = CONFIG->window_color_border_locked;
	}

	if (CONFIG->window_enable_focused)
	{
		box.color_border = CONFIG->window_color_border_focused;
	}

	/* end */

	return box;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static cgui_grid *
min_grid(const cgui_window *window)
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
refocus(cgui_window *window)
{
	struct grid_area area;
	struct cgui_cell_event event_foc;
	struct cgui_cell_event event_seek;
	struct cgui_cell_event event_info =
	{
		.type              = CGUI_CELL_EVENT_FOCUS_INFO,
		.focus_info_cell   = window->focus.cell,
		.focus_info_x      = window->focus.x,
		.focus_info_y      = window->focus.y,
		.focus_info_width  = window->focus.width,
		.focus_info_height = window->focus.height,
	};

	if (!window->focus.cell->valid)
	{
		return;
	}

	/* like in focus(), get data about potentially focused subcells in meta-cells */

	window_process_cell_event(window, window->focus, &event_info);

	/* find and focus the first cell that matches the previously focused cell. To do so, go throught */
	/* each cell the the shown grid has until a top-level non-meta cell is found or a meta-cell does */
	/* not reject a cell seek event. It should only accept said event if it hosts a matching         */
	/* subcell. This seeking event only matter for said meta-cells. Normal cells can reject it.      */
	/* Thanks to that seek event one can know which grid-level cell needs to be sent to the focus    */
	/* event. Finally, if no cell accepts the focus event that follows, the window should completely */
	/* loses focus.                                                                                  */

	CREF_FOR_EACH(window->shown_grid->areas, i)
	{
		area                 = *(struct grid_area*)cref_ptr(window->shown_grid->areas, i);
		event_foc.type       = CGUI_CELL_EVENT_FOCUS_GAIN_BY_REFERENCE;
		event_seek.type      = CGUI_CELL_EVENT_FOCUS_SEEK;
		event_foc.focus_cell = event_info.focus_info_cell;
		event_seek.seek_cell = event_info.focus_info_cell;

		if ((area.cell == event_foc.focus_cell
		 || window_process_cell_event(window, area, &event_seek)) 
		 && window_process_cell_event(window, area, &event_foc))
		{
			window_focus(window, area);
			return;
		}
	}

	window_focus(window, GRID_AREA_NONE);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
size_limits(const cgui_window *window, double *min_width, double *min_height, double *max_width, double *max_height)
{
	cgui_grid *grid;

	 grid       = min_grid(window);
	*min_width  = WIDTH(grid);
	*min_height = HEIGHT(grid);
	*max_width  = ((cgui_grid*)cref_ptr(window->grids, 0))->col_flex > 0.0 ? DBL_MAX : *min_width;
	*max_height = ((cgui_grid*)cref_ptr(window->grids, 0))->row_flex > 0.0 ? DBL_MAX : *min_height;
}	

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
update_shown_grid(cgui_window *window)
{
	cgui_grid *grid_old;
	cgui_grid *grid;

	if (window->state.locked_grid || cref_length(window->grids) < 2)
	{
		return;
	}

	/* find biggest grid that could fit in current window dimensions */

	grid_old           = window->shown_grid;
	window->shown_grid = min_grid(window);

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
	refocus(window);
}

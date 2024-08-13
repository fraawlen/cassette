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

#include "config.h"
#include "main.h"
#include "window.h"
#include "x11.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void _cairo_destroy  (cgui_window *)                              CGUI_NONNULL(1);
static bool _cairo_error    (const cgui_window *)                        CGUI_NONNULL(1);
static bool _cairo_setup    (cgui_window *, uint16_t, uint16_t)          CGUI_NONNULL(1);
static void _dummy_fn_close (cgui_window *)                              CGUI_NONNULL(1);
static void _dummy_fn_draw  (cgui_window *)                              CGUI_NONNULL(1);
static void _dummy_fn_state (cgui_window *, enum cgui_window_state_mask) CGUI_NONNULL(1);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static const struct cgui_window_state_flags _default_states = {false};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgui_window cgui_window_placeholder_instance =
{
	.x            = 0,
	.y            = 0,
	.width        = 0,
	.height       = 0,
	.x_serial     = 0,
	.x_id         = 0,
	.surface      = NULL,
	.drawable     = NULL,
	.fn_close     = _dummy_fn_close,
	.fn_draw      = _dummy_fn_draw,
	.fn_state     = _dummy_fn_state,
	.state        = _default_states,
	.draw         = WINDOW_DRAW_NONE,
	.wait_present = false,
	.valid        = false,
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cgui_window_activate(cgui_window *window)
{
	if (cgui_error() || !window->valid || window->state.active)
	{
		return;
	}

	x11_window_activate(window->x_id);
	window_update_state(window, CGUI_WINDOW_ACTIVE, true);
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

	window->x            = x;
	window->y            = y;
	window->width        = width;
	window->height       = height;
	window->x_serial     = 0;
	window->fn_close     = _dummy_fn_close;
	window->fn_draw      = _dummy_fn_draw;
	window->fn_state     = _dummy_fn_state;
	window->state        = _default_states;
	window->draw         = WINDOW_DRAW_NONE;
	window->wait_present = false;
	window->valid        = true;

	return window;

	/* errors */

fail_push:
	_cairo_destroy(window);
fail_cairo:
	x11_window_destroy(window->x_id);
fail_backend:
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
	if (cgui_error() || !window->valid)
	{
		return;
	}

	x11_window_rename(window->x_id, name);
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

	_cairo_destroy(window);
	main_pull_instance(main_windows(), window);
	x11_window_destroy(window->x_id);
	free(window);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_draw(cgui_window *window)
{
	struct ccolor cl;

	if (!window->state.mapped || window->draw == WINDOW_DRAW_NONE)
	{
		return;
	}

	cairo_set_operator(window->drawable, CAIRO_OPERATOR_SOURCE);

	/* draw background */

	if (window->draw == WINDOW_DRAW_FULL)
	{
		cl = CONFIG->window_color_background;
		cairo_set_source_rgba(window->drawable, cl.r, cl.g, cl.b, cl.a);
		cairo_paint(window->drawable);
	}

	/* draw border */

	if (window->draw != WINDOW_DRAW_CELLS)
	{
		// TODO
	}

	/* draw cells */

	if (window->draw != WINDOW_DRAW_BORDERS)
	{
		// TODO
	}

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

	window_set_draw_level(window, WINDOW_DRAW_FULL);
	window_resize(window, window->width, window->height);
	x11_window_update_state_hints(window->x_id, window->state);
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
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_set_draw_level(cgui_window *window, enum window_draw_level draw)
{
	if (window->draw == draw || window->draw == WINDOW_DRAW_FULL)
	{
		return;
	}

	switch (draw)
	{
		case WINDOW_DRAW_BORDERS:
		case WINDOW_DRAW_CELLS:
			window->draw = window->draw == WINDOW_DRAW_NONE ? draw : WINDOW_DRAW_BOTH;
			break;

		case WINDOW_DRAW_BOTH:
		case WINDOW_DRAW_FULL:
			window->draw = draw;
			break;

		case WINDOW_DRAW_NONE:
			break;
	}
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

	x11_window_update_state_hints(window->x_id, window->state);
	window->fn_state(window, mask);
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

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
_dummy_fn_state(cgui_window *window, enum cgui_window_state_mask mask)
{
	(void)window;
	(void)mask;
}

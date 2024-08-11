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

#include <cassette/cgui.h>
#include <cassette/cobj.h>
#include <stdbool.h>
#include <stdlib.h>

#include "main.h"
#include "window.h"
#include "x11.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

cgui_window cgui_window_placeholder_instance =
{
	.x            = 0,
	.y            = 0,
	.width        = 0,
	.height       = 0,
	.valid        = false,
	.active       = false,
	.mapped       = false,
	.obscured     = false,
	.focused      = false,
	.disabled     = false,
	.locked_grid  = false,
	.locked_focus = false,
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

cgui_window *
cgui_window_create(void)
{
	cgui_window *window;

	if (cgui_error())
	{
		goto fail_main;
	}

	if (!(window = malloc(sizeof(cgui_window))))
	{
		goto fail_alloc;
	}

	if (!main_push_instance(main_windows(), window))
	{
		goto fail_push;
	}

	window->x            = 0;
	window->y            = 0;
	window->width        = 400;
	window->height       = 400;
	window->valid        = true;
	window->active       = false;
	window->mapped       = false;
	window->obscured     = false;
	window->focused      = false;
	window->disabled     = false;
	window->locked_grid  = false;
	window->locked_focus = false;

	return window;

	/* errors */

fail_push:
	free(window);
fail_alloc:
	main_set_error(CERR_INSTANCE);
fail_main:
	return CGUI_WINDOW_PLACEHOLDER;
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

bool
cgui_window_is_active(const cgui_window *window)
{
	if (cgui_error() || !window->valid)
	{
		return false;
	}

	return window->active;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgui_window_is_disabled(const cgui_window *window)
{
	if (cgui_error() || !window->valid)
	{
		return false;
	}

	return window->disabled;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgui_window_is_focused(const cgui_window *window)
{
	if (cgui_error() || !window->valid)
	{
		return false;
	}

	return window->focused;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgui_window_is_locked_focus(const cgui_window *window)
{
	if (cgui_error() || !window->valid)
	{
		return false;
	}

	return window->locked_focus;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgui_window_is_locked_grid(const cgui_window *window)
{
	if (cgui_error() || !window->valid)
	{
		return false;
	}

	return window->locked_grid;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgui_window_is_mapped(const cgui_window *window)
{
	if (cgui_error() || !window->valid)
	{
		return false;
	}

	return window->mapped;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgui_window_is_obscured(const cgui_window *window)
{
	if (cgui_error() || !window->valid)
	{
		return false;
	}

	return window->obscured;
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

	main_pull_instance(main_windows(), window);
	free(window);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_present(cgui_window *window)
{
	(void)window;
}

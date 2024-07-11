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
	.to_destroy = false,
	.state      = CGUI_WINDOW_INITIAL,
	.err        = CERR_INVALID,
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

cgui_window *
cgui_window_create(void)
{
	cgui_window *window;

	if (!cgui_is_init() || cgui_error() || !(window = malloc(sizeof(cgui_window))))
	{
		return CGUI_WINDOW_PLACEHOLDER;
	}

	window->x          = 0;
	window->y          = 0;
	window->width      = 400;
	window->height     = 400;
	window->to_destroy = false;
	window->state      = CGUI_WINDOW_INITIAL;
	window->err        = CERR_NONE;

	main_push_instance(main_windows(), window);
	x11_window_create(window);

	return window;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_window_destroy(cgui_window *window)
{
	if (window == CGUI_WINDOW_PLACEHOLDER)
	{
		return;
	}

	window->to_destroy = true;
	if (!cgui_is_running())
	{
		window_destroy(window);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cerr
cgui_window_error(const cgui_window *window)
{
	return window->err;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_window_repair(cgui_window *window)
{
	window->err &= CERR_INVALID;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cgui_window_state
cgui_window_state(const cgui_window *window)
{
	if (window->err)
	{
		return CGUI_WINDOW_INITIAL;
	}

	return window->state;
}

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

void
window_destroy(cgui_window *window)
{
	if (!window->to_destroy)
	{
		return;
	}

	cref_pull(main_windows(), window);
	x11_window_destroy(window);
	free(window);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_present(cgui_window *window)
{
	(void)window;
}

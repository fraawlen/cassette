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

#include <assert.h>
#include <cassette/cgui.h>
#include <cassette/cobj.h>
#include <stdbool.h>
#include <stdlib.h>

#include "window.h"
#include "main.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static cgui_window_t _err_window =
{
	.id         = 0,
	.to_destroy = false,
	.failed     = true,
	.state      = CGUI_WINDOW_STATE_INITIAL,
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

cgui_window_t *
cgui_window_create(void)
{
	cgui_window_t *window;

	if (cgui_has_failed())
	{
		return &_err_window;
	}

	if (!(window = malloc(sizeof(cgui_window_t))))
	{
		return &_err_window;
	}

	window->id         = 0;
	window->to_destroy = false;
	window->failed     = false;
	window->state      = CGUI_WINDOW_STATE_INITIAL;

	cobj_tracker_push(main_get_windows(), window, &window->id);

	return window;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_window_destroy(cgui_window_t **window)
{
	assert(window && *window);

	if (*window == &_err_window)
	{
		return;
	}

	(*window)->to_destroy = true;
	if (!cgui_is_running())
	{
		window_destroy(*window);
	}

	*window = &_err_window;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgui_window_t *
cgui_window_get_placeholder(void)
{
	return &_err_window;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgui_window_has_failed(const cgui_window_t *window)
{
	assert(window);

	return window->failed;
}

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

void
window_destroy(cgui_window_t *window)
{
	if (!window->to_destroy)
	{
		return;
	}
	
	cobj_tracker_pull_pointer(main_get_windows(), window, window->id);
	free(window);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_present(cgui_window_t *window)
{
	(void)window;

	// TODO
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/


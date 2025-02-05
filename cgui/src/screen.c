/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Objects (COBJ) library.
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

#include <cassette/cgui.h>

#include "config.h"
#include "screen.h"
#include "x11.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static struct cgui_screen screen_zero = {0, 0.0, 0.0, 0.0, 0.0, false};
static double pointer_x = 0.0;
static double pointer_y = 0.0;

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

struct cgui_screen
cgui_screen_at_coordinates(double x, double y)
{
	if (cgui_error())
	{
		return screen_zero;
	}

	return x11_screen_at_coords(x, y);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
cgui_screen_numbers(void)
{
	size_t n;
	size_t p;

	if (cgui_error())
	{
		return 0;
	}

	x11_screen(0, &n, &p);

	return n;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_screen_pointer_position(double *x, double *y)
{
	if (cgui_error())
	{
		pointer_x = 0.0;
		pointer_y = 0.0;
	}
	else if (!cgui_is_running() || !CONFIG->shadows_reactive) /* see event.c -> pointer_raw() */
	{
		screen_pointer_update();
	}

	*x = pointer_x;
	*y = pointer_y;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct cgui_screen
cgui_screen_primary_specs(void)
{
	size_t n;
	size_t p;
	size_t q;

	if (cgui_error())
	{
		return screen_zero;
	}

	x11_screen(SIZE_MAX, &n, &p);

	return x11_screen(p, &n, &q);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct cgui_screen
cgui_screen_specs(size_t i)
{
	size_t n;
	size_t p;

	if (cgui_error())
	{
		return screen_zero;
	}

	return x11_screen(i, &n, &p);
}

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

void
screen_pointer_update(void)
{
	x11_pointer_position(&pointer_x, &pointer_y);
}

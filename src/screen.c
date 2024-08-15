/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Objects (COBJ) library.
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

#include "x11.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static struct cgui_screen _screen_zero = {0, 0, 0, 0, false};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

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

struct cgui_screen
cgui_screen_primary_specs(void)
{
	size_t n;
	size_t p;
	size_t q;

	if (cgui_error())
	{
		return _screen_zero;
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
		return _screen_zero;
	}

	return x11_screen(i, &n, &p);
}

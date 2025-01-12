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
#include <stdbool.h>
#include <stdlib.h>

#include "main.h"
#include "x11.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void dummy_fn_copy (int clipboard);
static void dummy_fn_lose (int clipboard);
static bool invalid       (int clipboard);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cgui_clipboard_copy(int clipboard, const char *str)
{
	if (invalid(clipboard))
	{
		return;
	}

	(void)str;
	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_clipboard_clear(int clipboard)
{
	if (invalid(clipboard))
	{
		return;
	}

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_clipboard_on_copy(int clipboard, void (*fn)(int clipboard))
{
	if (invalid(clipboard))
	{
		return;
	}

	(void)fn;

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_clipboard_on_lose(int clipboard, void (*fn)(int clipboard))
{
	if (invalid(clipboard))
	{
		return;
	}

	(void)fn;

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgui_clipboard_owned(int clipboard)
{
	if (invalid(clipboard))
	{
		return false;
	}

	// TODO

	return false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_clipboard_pair_cell(int clipboard, cgui_cell *cell)
{
	if (invalid(clipboard))
	{
		return;
	}

	(void)cell;

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgui_cell *
cgui_clipboard_paired_cell(int clipboard)
{
	if (invalid(clipboard))
	{
		return CGUI_CELL_PLACEHOLDER;
	}

	// TODO

	return CGUI_CELL_PLACEHOLDER;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const char *
cgui_clipboard_paste(int clipboard, size_t *length)
{
	if (invalid(clipboard))
	{
		if (length)
		{
			*length = 0;
		}
		return "";
	}

	// TODO

	if (length)
	{
		*length = 0;
	}

	return "";
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
dummy_fn_copy(int clipboard)
{
	(void)clipboard;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
dummy_fn_lose(int clipboard)
{
	(void)clipboard;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
invalid(int clipboard)
{
	if (clipboard == 0 || clipboard > CGUI_CLIPBOARDS)
	{
		main_set_error(CERR_PARAM);
	}

	return cgui_error();
}

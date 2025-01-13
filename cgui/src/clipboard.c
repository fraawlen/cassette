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
#include <string.h>
#include <xcb/xcb.h>

#include "clipboard.h"
#include "main.h"
#include "x11.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void dummy_fn_copy (int);
static void dummy_fn_lose (int);
static bool invalid       (int *);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static const struct clipboard empty =
{
	.time    = 0,
	.owned   = false,
	.data    = NULL,
	.data_n  = 0,
	.fn_copy = dummy_fn_copy,
	.fn_lose = dummy_fn_lose,
	.cell    = CGUI_CELL_PLACEHOLDER,
};

static struct clipboard clipboards[CGUI_CLIPBOARDS] =
{
	empty,
	empty,
	empty,
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cgui_clipboard_copy(int id, const char *str)
{
	xcb_timestamp_t time;
	size_t n;
	char *tmp;

	if (invalid(&id))
	{
		return;
	}

	/* copy data */

	n = strlen(str) + 1;
	if (!(tmp = malloc(n)))
	{
		main_set_error(CERR_MEMORY);
		return;
	}

	memcpy(tmp, str, n);

	/* update clipboard on backend */

	time = x11_timestamp();
	x11_selection_copy(id, time);
	if (cgui_error())
	{
		free(tmp);
		return;
	}

	/* update clipboard info */

	free(clipboards[id].data);

	clipboards[id]        = empty;
	clipboards[id].time   = time;
	clipboards[id].owned  = true;
	clipboards[id].data   = tmp;
	clipboards[id].data_n = n;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_clipboard_clear(int id)
{
	if (invalid(&id) || !clipboards[id].owned)
	{
		return;
	}

	x11_selection_clear(id);
	clipboard_clear(id);	
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_clipboard_on_copy(int id, void (*fn)(int id))
{
	if (invalid(&id))
	{
		return;
	}

	clipboards[id].fn_copy = fn ? fn : dummy_fn_copy;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_clipboard_on_lose(int id, void (*fn)(int id))
{
	if (invalid(&id))
	{
		return;
	}

	clipboards[id].fn_lose = fn ? fn : dummy_fn_lose;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgui_clipboard_owned(int id)
{
	if (invalid(&id))
	{
		return false;
	}

	return clipboards[id].owned;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_clipboard_pair_cell(int id, cgui_cell *cell)
{
	if (invalid(&id))
	{
		return;
	}

	clipboards[id].cell = cell;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgui_cell *
cgui_clipboard_paired_cell(int id)
{
	if (invalid(&id))
	{
		return CGUI_CELL_PLACEHOLDER;
	}

	return clipboards[id].cell;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const char *
cgui_clipboard_paste(int id, size_t *length)
{
	const char *data   = "";
	size_t      data_n = 0;

	if (!invalid(&id))
	{
		if (!clipboards[id].owned)
		{
			free(clipboards[id].data);
			clipboards[id].data = x11_selection_paste(id, &clipboards[id].data_n);
		}

		if (clipboards[id].data)
		{
			data   = clipboards[id].data;
			data_n = clipboards[id].data_n;
		}
	}

	if (length)
	{
		*length = data_n;
	}

	return data;
}

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

void
clipboard_clear(int id)
{
	if (id < 0 || id >= CGUI_CLIPBOARDS)
	{
		return;
	}

	free(clipboards[id].data);
	clipboards[id] = empty;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct clipboard
clipboard_get(int id)
{
	if (id < 0 || id >= CGUI_CLIPBOARDS)
	{
		return empty;
	}

	return clipboards[id];
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
dummy_fn_copy(int id)
{
	(void)id;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
dummy_fn_lose(int id)
{
	(void)id;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
invalid(int *id)
{
	if (*id == 0 || (*id)-- > CGUI_CLIPBOARDS)
	{
		main_set_error(CERR_PARAM);
	}

	return cgui_error();
}

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
#include "cell.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void _dummy_fn_destroy (cgui_cell *cell);
static void _dummy_fn_draw    (cgui_cell *cell, struct cgui_cell_context *context);
static void _dummy_fn_event   (cgui_cell *cell, struct cgui_cell_event *event);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

cgui_cell cgui_cell_placeholder_instance =
{
	.to_destroy = false,
	.data       = NULL,
	.fn_destroy = _dummy_fn_destroy,
	.fn_draw    = _dummy_fn_draw,
	.fn_event   = _dummy_fn_event,
	.err        = CERR_INVALID,
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

cgui_cell *
cgui_cell_create(void)
{
	cgui_cell *cell;

	if (!cgui_is_init() || cgui_error() || !(cell = malloc(sizeof(cgui_cell))))
	{
		return CGUI_CELL_PLACEHOLDER;
	}

	cell->data       = NULL,
	cell->fn_destroy = _dummy_fn_destroy,
	cell->fn_draw    = _dummy_fn_draw,
	cell->fn_event   = _dummy_fn_event,
	cell->to_destroy = false;
	cell->err        = CERR_NONE;

	cref_push(main_cells(), cell);

	return cell;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void *
cgui_cell_data(const cgui_cell *cell)
{
	if (cell->err)
	{
		return NULL;
	}

	return cell->data;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_cell_destroy(cgui_cell *cell)
{
	if (cell == CGUI_CELL_PLACEHOLDER)
	{
		return;
	}

	cell->to_destroy = true;
	if (!cgui_is_running())
	{
		cell_destroy(cell);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cerr
cgui_cell_error(const cgui_cell *cell)
{
	return cell->err;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
(*cgui_cell_fn_destroy(cgui_cell *cell))(cgui_cell *cell)
{
	if (cell->err)
	{
		return _dummy_fn_destroy;
	}

	return cell->fn_destroy;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/


void
(*cgui_cell_fn_draw(cgui_cell *cell))(cgui_cell *cell, struct cgui_cell_context *context)
{
	if (cell->err)
	{
		return _dummy_fn_draw;
	}

	return cell->fn_draw;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/


void
(*cgui_cell_fn_event(cgui_cell *cell))(cgui_cell *cell, struct cgui_cell_event *event)
{
	if (cell->err)
	{
		return _dummy_fn_event;
	}

	return cell->fn_event;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_cell_on_destroy(cgui_cell *cell, void (*fn)(cgui_cell *cell))
{
	if (cell->err)
	{
		return;
	}
	
	cell->fn_destroy = fn ? fn : _dummy_fn_destroy;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_cell_on_draw(cgui_cell *cell, void (*fn)(cgui_cell *cell, struct cgui_cell_context *context))
{
	if (cell->err)
	{
		return;
	}

	cell->fn_draw = fn ? fn : _dummy_fn_draw;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_cell_on_event(cgui_cell *cell, void (*fn)(cgui_cell *cell, struct cgui_cell_event *event))
{
	if (cell->err)
	{
		return;
	}

	cell->fn_event = fn ? fn : _dummy_fn_event;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_cell_redraw(cgui_cell *cell)
{
	if (cell->err)
	{
		return;
	}

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_cell_repair(cgui_cell *cell)
{
	cell->err &= CERR_INVALID;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgui_cell_send_custom_event(cgui_cell *cell, int id, void *data, size_t length)
{
	if (cell->err)
	{
		return false;
	}

	(void)id;
	(void)data;
	(void)length;

	// TODO

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_cell_set_data(cgui_cell *cell, void *data)
{
	if (cell->err)
	{
		return;
	}

	cell->data = data;
}

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

void
cell_destroy(cgui_cell *cell)
{
	if (!cell->to_destroy)
	{
		return;
	}

	cref_pull(main_cells(), cell);
	free(cell);
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static void
_dummy_fn_destroy(cgui_cell *cell)
{
	(void)cell;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_dummy_fn_draw(cgui_cell *cell, struct cgui_cell_context *context)
{
	(void)cell;
	(void)context;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_dummy_fn_event(cgui_cell *cell, struct cgui_cell_event *event)
{
	(void)cell;
	(void)event;
}


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
#include "window.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void dummy_fn_destroy (cgui_cell *)                           CGUI_NONNULL(1);
static void dummy_fn_draw    (cgui_cell *, struct cgui_cell_context) CGUI_NONNULL(1);
static void dummy_fn_event   (cgui_cell *, struct cgui_cell_event *) CGUI_NONNULL(1, 2);
static void dummy_fn_frame   (cgui_cell *, struct cgui_box *)        CGUI_NONNULL(1, 2);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

cgui_cell cgui_cell_placeholder_instance =
{
	.data       = NULL,
	.fn_destroy = dummy_fn_destroy,
	.fn_draw    = dummy_fn_draw,
	.fn_event   = dummy_fn_event,
	.fn_frame   = dummy_fn_frame,
	.valid      = false,
	.draw       = false,
	.serial     = CELL_INVALID,
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

cgui_cell *
cgui_cell_create(void)
{
	cgui_cell *cell;

	if (cgui_error())
	{
		goto fail_main;
	}

	if (!(cell = malloc(sizeof(cgui_cell))))
	{
		goto fail_alloc;
	}

	if (!main_push_instance(main_cells(), cell))
	{
		goto fail_push;
	}

	cell->data       = NULL;
	cell->fn_destroy = dummy_fn_destroy;
	cell->fn_draw    = dummy_fn_draw;
	cell->fn_event   = dummy_fn_event;
	cell->fn_frame   = dummy_fn_frame;
	cell->valid      = true;
	cell->draw       = false;
	cell->serial     = CELL_INVALID;

	return cell;

	/* errors */

fail_push:
	free(cell);
fail_alloc:
	main_set_error(CERR_INSTANCE);
fail_main:
	return CGUI_CELL_PLACEHOLDER;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void *
cgui_cell_data(const cgui_cell *cell)
{
	if (cgui_error() || !cell->valid)
	{
		return NULL;
	}

	return cell->data;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_cell_destroy(cgui_cell *cell)
{
	cell->valid = false;
	if (!cgui_is_running())
	{
		cell_destroy(cell);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
(*cgui_cell_fn_destroy(cgui_cell *cell))(cgui_cell *cell)
{
	if (cgui_error() || !cell->valid)
	{
		return dummy_fn_destroy;
	}

	return cell->fn_destroy;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
(*cgui_cell_fn_draw(cgui_cell *cell))(cgui_cell *cell, struct cgui_cell_context context)
{
	if (cgui_error() || !cell->valid)
	{
		return dummy_fn_draw;
	}

	return cell->fn_draw;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
(*cgui_cell_fn_event(cgui_cell *cell))(cgui_cell *cell, struct cgui_cell_event *event)
{
	if (cgui_error() || !cell->valid)
	{
		return dummy_fn_event;
	}

	return cell->fn_event;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
(*cgui_cell_fn_frame(cgui_cell *cell))(cgui_cell *cell, struct cgui_box *box)
{
	if (cgui_error() || !cell->valid)
	{
		return dummy_fn_frame;
	}

	return cell->fn_frame;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgui_cell_is_valid(const cgui_cell *cell)
{
	if (cgui_error())
	{
		return false;
	}

	return cell->valid;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_cell_on_destroy(cgui_cell *cell, void (*fn)(cgui_cell *cell))
{
	if (cgui_error() || !cell->valid)
	{
		return;
	}
	
	cell->fn_destroy = fn ? fn : dummy_fn_destroy;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_cell_on_draw(cgui_cell *cell, void (*fn)(cgui_cell *cell, struct cgui_cell_context context))
{
	if (cgui_error() || !cell->valid)
	{
		return;
	}

	cell->fn_draw = fn ? fn : dummy_fn_draw;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_cell_on_event(cgui_cell *cell, void (*fn)(cgui_cell *cell, struct cgui_cell_event *event))
{
	if (cgui_error() || !cell->valid)
	{
		return;
	}

	cell->fn_event = fn ? fn : dummy_fn_event;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_cell_on_frame(cgui_cell *cell, void (*fn)(cgui_cell *cell, struct cgui_box *box))
{
	if (cgui_error() || !cell->valid)
	{
		return;
	}

	cell->fn_frame = fn ? fn : dummy_fn_frame;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_cell_redraw(cgui_cell *cell)
{
	if (cgui_error() || !cell->valid)
	{
		return;
	}

	CREF_FOR_EACH(main_windows(), i)
	{
		window_set_draw_level((cgui_window*)cref_ptr(main_windows(), i), WINDOW_DRAW_PARTIAL);
	}

	cell->draw = true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

int
cgui_cell_serial(const cgui_cell *cell)
{
	if (cgui_error() || !cell->valid)
	{
		return CELL_INVALID;
	}

	return cell->serial;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_cell_set_data(cgui_cell *cell, void *data)
{
	if (cgui_error() || !cell->valid)
	{
		return;
	}

	cell->data = data;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_cell_set_serial(cgui_cell *cell, int serial)
{
	if (cgui_error() || !cell->valid)
	{
		return;
	}

	cell->serial = serial;
}

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

void
cell_destroy(cgui_cell *cell)
{
	if (cell == CGUI_CELL_PLACEHOLDER || cell->valid)
	{
		return;
	}

	main_pull_instance(main_cells(), cell);
	free(cell);
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
dummy_fn_destroy(cgui_cell *cell)
{
	(void)cell;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
dummy_fn_draw(cgui_cell *cell, struct cgui_cell_context context)
{
	(void)cell;
	(void)context;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
dummy_fn_event(cgui_cell *cell, struct cgui_cell_event *event)
{
	(void)cell;
	(void)event;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
dummy_fn_frame(cgui_cell *cell, struct cgui_box *box)
{
	(void)cell;
	(void)box;
}


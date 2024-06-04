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
#include <stdbool.h>
#include <stdlib.h>

#include <cassette/cgui.h>
#include <cassette/cobj.h>

#include "cell.h"
#include "main.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void _dummy_callback_destroy (cgui_cell_t *cell);
static void _dummy_callback_draw    (cgui_cell_t *cell, cgui_cell_drawing_context_t *context);
static void _dummy_callback_event   (cgui_cell_t *cell, cgui_cell_event_t *event);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static cgui_cell_t _err_cell =
{
	.id         = 0,
	.data       = NULL,
	.to_destroy = false,
	.enabled    = false,
	.failed     = true,
	.fn_destroy = _dummy_callback_destroy,
	.fn_draw    = _dummy_callback_draw,
	.fn_event   = _dummy_callback_event,
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

cgui_cell_t *
cgui_cell_create(void)
{
	cgui_cell_t *cell;

	assert(cgui_is_init());

	if (!(cell = malloc(sizeof(cgui_cell_t))))
	{
		return &_err_cell;
	}

	cell->id         = 0;
	cell->to_destroy = false;
	cell->failed     = false;
	cell->enabled    = true;
	cell->fn_destroy = _dummy_callback_destroy;
	cell->fn_draw    = _dummy_callback_draw;
	cell->fn_event   = _dummy_callback_event;

	cobj_tracker_push(main_get_cells(), cell, &cell->id);
	main_update_status();

	return cell;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_cell_destroy(cgui_cell_t **cell)
{
	assert(cgui_is_init());
	assert(cell && *cell);

	if (*cell == &_err_cell)
	{
		return;
	}

	(*cell)->to_destroy = true;
	if (!cgui_is_running())
	{
		cell_destroy(*cell);
	}

	*cell = &_err_cell;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_cell_disable(cgui_cell_t *cell)
{
	assert(cgui_is_init());
	assert(cell);

	if (cell->failed)
	{
		return;
	}

	cell->enabled = false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_cell_enable(cgui_cell_t *cell)
{
	assert(cgui_is_init());
	assert(cell);

	if (cell->failed)
	{
		return;
	}

	cell->enabled = true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgui_cell_callback_destroy_t
cgui_cell_get_callback_destroy(const cgui_cell_t *cell)
{
	assert(cgui_is_init());
	assert(cell);

	if (cell->failed)
	{
		return _dummy_callback_destroy;
	}

	return cell->fn_destroy;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgui_cell_callback_draw_t
cgui_cell_get_callback_draw(const cgui_cell_t *cell)

{
	assert(cgui_is_init());
	assert(cell);

	if (cell->failed)
	{
		return _dummy_callback_draw;
	}

	return cell->fn_draw;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgui_cell_callback_event_t
cgui_cell_get_callback_event(const cgui_cell_t *cell)
{
	assert(cgui_is_init());
	assert(cell);

	if (cell->failed)
	{
		return _dummy_callback_event;
	}

	return cell->fn_event;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void *
cgui_cell_get_data(const cgui_cell_t *cell)
{
	assert(cgui_is_init());
	assert(cell);
	
	if (cell->failed)
	{
		return NULL;
	}

	return cell->data;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgui_cell_t *
cgui_cell_get_placeholder(void)
{
	return &_err_cell;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgui_cell_is_enabled(const cgui_cell_t *cell)
{
	assert(cgui_is_init());
	assert(cell);

	return !cell->failed & cell->enabled;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgui_cell_has_failed(const cgui_cell_t *cell)
{
	assert(cgui_is_init());
	assert(cell);

	return cell->failed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_cell_redraw(cgui_cell_t *cell)
{
	assert(cgui_is_init());
	assert(cell);

	if (cell->failed)
	{
		return;
	}

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgui_cell_send_custom_event(cgui_cell_t *cell, int id, void *data, size_t data_n)
{
	assert(cgui_is_init());
	assert(cell);

	if (cell->failed)
	{
		return false;
	}

	(void)id;
	(void)data;
	(void)data_n;

	// TODO

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_cell_set_callback_destroy(cgui_cell_t *cell, cgui_cell_callback_destroy_t fn)
{
	assert(cgui_is_init());
	assert(cell);

	if (cell->failed)
	{
		return;
	}

	cell->fn_destroy = fn ? fn : _dummy_callback_destroy;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_cell_set_callback_draw(cgui_cell_t *cell, cgui_cell_callback_draw_t fn)

{
	assert(cgui_is_init());
	assert(cell);

	if (cell->failed)
	{
		return;
	}

	cell->fn_draw = fn ? fn : _dummy_callback_draw;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_cell_set_callback_event(cgui_cell_t *cell, cgui_cell_callback_event_t fn)
{
	assert(cgui_is_init());
	assert(cell);

	if (cell->failed)
	{
		return;
	}

	cell->fn_event = fn ? fn : _dummy_callback_event;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_cell_set_data(cgui_cell_t *cell, void *data)
{
	assert(cgui_is_init());
	assert(cell);

	if (cell->failed)
	{
		return;
	}

	cell->data = data;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_cell_toggle(cgui_cell_t *cell)
{
	assert(cgui_is_init());
	assert(cell);

	if (cell->failed)
	{
		return;
	}

	cell->enabled = !cell->enabled;
}

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

void
cell_destroy(cgui_cell_t *cell)
{
	if (!cell->to_destroy)
	{
		return;
	}

	cell->fn_destroy(cell);

	cobj_tracker_pull_pointer(main_get_cells(), cell, cell->id);

	free(cell);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cell_send_windowless_event(cgui_cell_t *cell, cgui_cell_event_t *event)
{
	(void)cell;
	(void)event;

	// TODO

	return true;
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static void
_dummy_callback_destroy(cgui_cell_t *cell)
{
	(void)cell;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_dummy_callback_draw(cgui_cell_t *cell, cgui_cell_drawing_context_t *context)
{
	(void)cell;
	(void)context;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_dummy_callback_event(cgui_cell_t *cell, cgui_cell_event_t *event)
{
	(void)cell;
	(void)event;
}

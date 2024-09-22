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

#include <cairo/cairo.h>
#include <cassette/cgui.h>
#include <cassette/cobj.h>
#include <stdbool.h>
#include <stdlib.h>

#include "cell.h"
#include "config.h"
#include "main.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define DATA ((struct data*)cgui_cell_data(cell))

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum state
{
	IDLE,
	FOCUSED,
	PRESSED,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct data
{
	void (*fn_click)(cgui_cell *);
	enum state state;
	bool enabled;
	cstr *label;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void destroy        (cgui_cell *)                           CGUI_NONNULL(1);
static void draw           (cgui_cell *, struct cgui_cell_context) CGUI_NONNULL(1);
static void dummy_fn_click (cgui_cell *)                           CGUI_NONNULL(1);
static void event          (cgui_cell *, struct cgui_cell_event *) CGUI_NONNULL(1, 2);
static void frame          (cgui_cell *, struct cgui_box *)        CGUI_NONNULL(1, 2);
static bool invalid        (const cgui_cell *)                     CGUI_NONNULL(1);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

cgui_cell *
cgui_button_create(void)
{
	cgui_cell   *cell;
	struct data *data;

	if (cgui_error())
	{
		goto fail_main;
	}

	if (!(data = malloc(sizeof(struct data))))
	{
		goto fail_data;
	}
	 
	if ((data->label = cstr_create()) == CSTR_PLACEHOLDER)
	{
		goto fail_label;
	}

	if ((cell = cgui_cell_create()) == CGUI_CELL_PLACEHOLDER)
	{
		goto fail_cell;
	}

	data->fn_click = dummy_fn_click;
	data->enabled  = true;
	data->state    = IDLE;

	cgui_cell_on_destroy (cell, destroy);
	cgui_cell_on_draw(cell, draw);
	cgui_cell_on_event(cell, event);
	cgui_cell_on_frame(cell, frame);
	cgui_cell_set_data(cell, data);
	cgui_cell_set_serial(cell, CELL_BUTTON);

	return cell;

	/* error */

fail_cell:
	cstr_destroy(data->label);
fail_label:
	free(data);
fail_data:
	main_set_error(CERR_INSTANCE);
fail_main:
	return CGUI_CELL_PLACEHOLDER;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_button_disable(cgui_cell *cell)
{
	if (invalid(cell))
	{
		return;
	}
	
	DATA->enabled = false;
	
	cgui_cell_redraw(cell);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_button_enable(cgui_cell *cell)
{
	if (invalid(cell))
	{
		return;
	}

	DATA->enabled = true;

	cgui_cell_redraw(cell);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/


void
cgui_button_on_click(cgui_cell *cell, void (*fn)(cgui_cell *cell))
{
	if (invalid(cell))
	{
		return;
	}

	DATA->fn_click = fn ? fn : dummy_fn_click;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_button_set_label(cgui_cell *cell, const char *label)
{
	if (invalid(cell))
	{
		return;
	}

	cstr_clear(DATA->label);
	cstr_append(DATA->label, label);

	cgui_cell_redraw(cell);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_button_toggle(cgui_cell *cell)
{
	if (invalid(cell))
	{
		return;
	}

	DATA->enabled = !DATA->enabled;

	cgui_cell_redraw(cell);
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
destroy(cgui_cell *cell)
{
	cstr_destroy(DATA->label);
	free(DATA);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
draw(cgui_cell *cell, struct cgui_cell_context context)
{
	(void)cell;

	cgui_cell_draw_frame(context);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
dummy_fn_click(cgui_cell *cell)
{
	(void)cell;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
event(cgui_cell *cell, struct cgui_cell_event *event)
{
	enum state old_state = DATA->state;
	bool trigger         = false;

	/* pre-filter */

	if (!DATA->enabled)
	{
		event->msg = CGUI_CELL_MSG_REJECT;
		return;
	}

	/* process event */

	switch (event->type)
	{
		case CGUI_CELL_EVENT_BUTTON_PRESS:
			DATA->state = event->button_id == 1 ? PRESSED : DATA->state;
			break;

		case CGUI_CELL_EVENT_BUTTON_RELEASE:
			DATA->state = event->button_id == 1 ? FOCUSED : DATA->state;
			trigger     = event->button_id == 1 && cgui_cell_is_event_in(event);
			break;

		case CGUI_CELL_EVENT_FOCUS_GAIN_BY_REFERENCE:
		case CGUI_CELL_EVENT_FOCUS_GAIN_BY_ACTION:
		case CGUI_CELL_EVENT_FOCUS_GAIN_BY_POINTER:
		case CGUI_CELL_EVENT_FOCUS_GAIN_BY_TOUCH:
			DATA->state = FOCUSED;
			break;

		case CGUI_CELL_EVENT_FOCUS_LOSE:
			DATA->state = IDLE;
			break;

		default:
			event->msg = CGUI_CELL_MSG_REJECT;
			return;
	}
	
	/* post-event */

	if (old_state != DATA->state)
	{
		cgui_cell_redraw(cell);
	}

	if (trigger)
	{
		DATA->fn_click(cell);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
frame(cgui_cell *cell, struct cgui_box *box)
{
	if (!DATA->enabled)
	{
		*box = CONFIG->button_frame_disabled;
		return;
	}

	switch (DATA->state)
	{
		case IDLE:
			*box = CONFIG->button_frame_idle;
			break;

		case FOCUSED:
			*box = CONFIG->button_frame_focused;
			break;

		case PRESSED:
			*box = CONFIG->button_frame_pressed;
			break;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
invalid(const cgui_cell *cell)
{
	if (cell->serial != CELL_BUTTON)
	{
		main_set_error(CERR_PARAM);
	}

	return cgui_error() || !cell->valid;
}

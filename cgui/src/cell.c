/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Graphics (CGUI) library.
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
#include <cassette/cobj.h>
#include <stdbool.h>
#include <stdlib.h>

#include "main.h"
#include "cell.h"
#include "config.h"
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

void
cgui_cell_clip_frame(struct cgui_cell_context context)
{
	cgui_box_move(context.x, context.y);
	cgui_box_resize(context.width, context.height);
	cgui_box_style(context.frame);
	cgui_box_clip(context.drawable, context.frame.size_border + context.frame.padding);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

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

void
cgui_cell_draw_frame(struct cgui_cell_context context)
{
	double x;
	double y;

	cgui_box_move(context.x, context.y);
	cgui_box_resize(context.width, context.height);
	cgui_box_style(context.frame);

	/* reactive shadows */

	if (CONFIG->shadows_follow_pointer)
	{
		cgui_screen_pointer_position(&x, &y);
		cgui_box_move_shadow(
			x - context.x_root,
			y - context.y_root,
			CONFIG->shadows_max_light_distance,
			CONFIG->shadows_max_offset);
	}

	/* draw */
	
	cgui_box_draw(context.drawable);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void *
cgui_cell_data(const cgui_cell *cell)
{
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
cgui_cell_event_inside(const struct cgui_cell_event *event)
{
	double x;
	double y;

	switch (event->type)
	{
		case CGUI_CELL_EVENT_BUTTON_PRESS:
		case CGUI_CELL_EVENT_BUTTON_RELEASE:
			x = event->button_x;
			y = event->button_y;
			break;

		case CGUI_CELL_EVENT_POINTER_MOTION:
			x = event->pointer_x;
			y = event->pointer_y;
			break;

		case CGUI_CELL_EVENT_TOUCH_BEGIN:
		case CGUI_CELL_EVENT_TOUCH_END:
		case CGUI_CELL_EVENT_TOUCH_UPDATE:
			x = event->touch_x;
			y = event->touch_y;
			break;

		case CGUI_CELL_EVENT_FOCUS_GAIN_BY_POINTER:
		case CGUI_CELL_EVENT_FOCUS_GAIN_BY_TOUCH:
			x = event->focus_x;
			y = event->focus_y;
			break;

		default:
			return false;
	}

	cgui_box_move(event->x, event->y);
	cgui_box_resize(event->width, event->height);
	cgui_box_style(event->frame);

	return cgui_box_inside(event->drawable, x, y);
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
	cgui_cell_redraw_delayed(cell, 0);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_cell_redraw_delayed(cgui_cell *cell, unsigned long delay)
{
	if (cgui_error() || !cell->valid)
	{
		return;
	}

	CREF_FOR_EACH(main_windows(), i)
	{
		window_schedule_draw((cgui_window*)cref_ptr(main_windows(), i), WINDOW_DRAW_PARTIAL, delay);
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

	cell->fn_destroy(cell);
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

	event->msg = CGUI_CELL_MSG_REJECT;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
dummy_fn_frame(cgui_cell *cell, struct cgui_box *box)
{
	(void)cell;
	(void)box;
}


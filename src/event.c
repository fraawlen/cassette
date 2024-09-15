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
#include <float.h>
#include <math.h>
#include <xcb/xcb.h>

#include "event.h"
#include "config.h"
#include "window.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void accelerate           (struct cgui_event *) CGUI_NONNULL(1);
static void button_press         (struct cgui_event *) CGUI_NONNULL(1);
static void button_release       (struct cgui_event *) CGUI_NONNULL(1);
static void close                (struct cgui_event *) CGUI_NONNULL(1);
static void dummy_callback_event (struct cgui_event *) CGUI_NONNULL(1);
static void focus                (struct cgui_event *) CGUI_NONNULL(1);
static void key_press            (struct cgui_event *) CGUI_NONNULL(1);
static void key_release          (struct cgui_event *) CGUI_NONNULL(1);
static void leave                (struct cgui_event *) CGUI_NONNULL(1);
static void map                  (struct cgui_event *) CGUI_NONNULL(1);
static void pointer              (struct cgui_event *) CGUI_NONNULL(1);
static void reconfig             (void);
static void redraw               (struct cgui_event *) CGUI_NONNULL(1);
static void touch_begin          (struct cgui_event *) CGUI_NONNULL(1);
static void touch_end            (struct cgui_event *) CGUI_NONNULL(1);
static void touch_update         (struct cgui_event *) CGUI_NONNULL(1);
static void transform            (struct cgui_event *) CGUI_NONNULL(1);
static void unfocus              (struct cgui_event *) CGUI_NONNULL(1);
static void unmap                (struct cgui_event *) CGUI_NONNULL(1);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void (*fn_event) (struct cgui_event *) = dummy_callback_event;

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cgui_event_on_event(void (*fn)(struct cgui_event *event))
{
	if (cgui_error())
	{
		return;
	}

	fn_event = fn ? fn : dummy_callback_event;
}

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

void
event_process(struct cgui_event *event)
{
	fn_event(event);

	switch (event->type)
	{
		case CGUI_EVENT_CLOSE:
			close(event);
			break;

		case CGUI_EVENT_ACCELERATOR:
			accelerate(event);
			break;

		case CGUI_EVENT_RECONFIG:
			reconfig();
			break;

		case CGUI_EVENT_TRANSFORM:
			transform(event);
			break;

		case CGUI_EVENT_FOCUS:
			focus(event);
			break;

		case CGUI_EVENT_UNFOCUS:
			unfocus(event);
			break;

		case CGUI_EVENT_MAP:
			map(event);
			break;

		case CGUI_EVENT_UNMAP:
			unmap(event);
			break;

		case CGUI_EVENT_TOUCH_BEGIN:
			touch_begin(event);
			break;

		case CGUI_EVENT_TOUCH_UPDATE:
			touch_update(event);
			break;

		case CGUI_EVENT_TOUCH_END:
			touch_end(event);
			break;

		case CGUI_EVENT_BUTTON_PRESS:
			button_press(event);
			break;

		case CGUI_EVENT_BUTTON_RELEASE:
			button_release(event);
			break;

		case CGUI_EVENT_KEY_PRESS:
			key_press(event);
			break;

		case CGUI_EVENT_KEY_RELEASE:
			key_release(event);
			break;

		case CGUI_EVENT_POINTER_MOTION:
			pointer(event);
			break;

		case CGUI_EVENT_LEAVE:
			leave(event);
			break;

		case CGUI_EVENT_REDRAW:
			redraw(event);
			break;

		case CGUI_EVENT_UNKNOWN_XCB:
		case CGUI_EVENT_ENTER:
		case CGUI_EVENT_NONE:
			break;
	}
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
accelerate(struct cgui_event *event)
{
	if (!event->window->valid)
	{
		return;
	}

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
button_press(struct cgui_event *event)
{
	if (!event->window->valid)
	{
		return;
	}

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
button_release(struct cgui_event *event)
{
	if (!event->window->valid)
	{
		return;
	}

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
close(struct cgui_event *event)
{
	event->window->fn_close(event->window);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
dummy_callback_event(struct cgui_event *event)
{
	(void)event;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
focus(struct cgui_event *event)
{
	if (!event->window->valid)
	{
		return;
	}

	window_update_state(event->window, CGUI_WINDOW_FOCUSED, true);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
key_press(struct cgui_event *event)
{
	if (!event->window->valid)
	{
		return;
	}

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
key_release(struct cgui_event *event)
{
	if (!event->window->valid)
	{
		return;
	}

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
leave(struct cgui_event *event)
{
	if (!event->window->valid)
	{
		return;
	}

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
map(struct cgui_event *event)
{
	if (!event->window->valid)
	{
		return;
	}

	window_update_state(event->window, CGUI_WINDOW_MAPPED, true);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
pointer(struct cgui_event *event)
{
	struct cgui_cell_event cell_event =
	{
		.type      = CGUI_CELL_EVENT_POINTER_MOTION,
		.pointer_x = event->pointer_x,
		.pointer_y = event->pointer_y,
	};

	if (!event->window->valid)
	{
		return;
	}

	window_focus_pointer(event->window, event->pointer_x, event->pointer_y);
	window_process_cell_event(event->window, event->window->focus, &cell_event);

	// TODO - drag / resize window if event is rejected
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
reconfig(void)
{
	cgui_reconfig();
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
redraw(struct cgui_event *event)
{
	if (!event->window->valid)
	{
		return;
	}

	if (event->redraw_all)
	{
		window_set_draw_level(event->window, WINDOW_DRAW_FULL);
	}

	window_draw(event->window);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
touch_begin(struct cgui_event *event)
{
	if (!event->window->valid)
	{
		return;
	}

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
touch_end(struct cgui_event *event)
{
	if (!event->window->valid)
	{
		return;
	}

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
touch_update(struct cgui_event *event)
{
	if (!event->window->valid)
	{
		return;
	}

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
transform(struct cgui_event *event)
{
	cgui_window *w = event->window;

	if (!event->window->valid)
	{
		return;
	}

	w->x = event->transform_x;
	w->y = event->transform_y;

	if (fabs(w->width  - event->transform_width)  < DBL_EPSILON
	 && fabs(w->height - event->transform_height) < DBL_EPSILON)
	{
		return;
	}
	
	window_resize(w, event->transform_width, event->transform_height);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
unfocus(struct cgui_event *event)
{
	if (!event->window->valid)
	{
		return;
	}

	window_update_state(event->window, CGUI_WINDOW_FOCUSED, false);

	// TODO clear all tracked inputs
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
unmap(struct cgui_event *event)
{
	if (!event->window->valid)
	{
		return;
	}

	window_update_state(event->window, CGUI_WINDOW_MAPPED, false);
}

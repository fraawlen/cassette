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
#include <xcb/xcb.h>

#include "event.h"
#include "config.h"
#include "window.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void _accelerate           (struct cgui_event *) CGUI_NONNULL(1);
static void _button_press         (struct cgui_event *) CGUI_NONNULL(1);
static void _button_release       (struct cgui_event *) CGUI_NONNULL(1);
static void _close                (struct cgui_event *) CGUI_NONNULL(1);
static void _dummy_callback_event (struct cgui_event *) CGUI_NONNULL(1);
static void _focus                (struct cgui_event *) CGUI_NONNULL(1);
static void _key_press            (struct cgui_event *) CGUI_NONNULL(1);
static void _key_release          (struct cgui_event *) CGUI_NONNULL(1);
static void _leave                (struct cgui_event *) CGUI_NONNULL(1);
static void _map                  (struct cgui_event *) CGUI_NONNULL(1);
static void _pointer              (struct cgui_event *) CGUI_NONNULL(1);
static void _reconfig             (void);
static void _redraw               (struct cgui_event *) CGUI_NONNULL(1);
static void _touch_begin          (struct cgui_event *) CGUI_NONNULL(1);
static void _touch_end            (struct cgui_event *) CGUI_NONNULL(1);
static void _touch_update         (struct cgui_event *) CGUI_NONNULL(1);
static void _transform            (struct cgui_event *) CGUI_NONNULL(1);
static void _unfocus              (struct cgui_event *) CGUI_NONNULL(1);
static void _unmap                (struct cgui_event *) CGUI_NONNULL(1);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void (*_fn_event) (struct cgui_event *) = _dummy_callback_event;

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

	_fn_event = fn ? fn : _dummy_callback_event;
}

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

void
event_process(struct cgui_event *event)
{
	_fn_event(event);

	switch (event->type)
	{
		case CGUI_EVENT_CLOSE:
			_close(event);
			break;

		case CGUI_EVENT_ACCELERATOR:
			_accelerate(event);
			break;

		case CGUI_EVENT_RECONFIG:
			_reconfig();
			break;

		case CGUI_EVENT_TRANSFORM:
			_transform(event);
			break;

		case CGUI_EVENT_FOCUS:
			_focus(event);
			break;

		case CGUI_EVENT_UNFOCUS:
			_unfocus(event);
			break;

		case CGUI_EVENT_MAP:
			_map(event);
			break;

		case CGUI_EVENT_UNMAP:
			_unmap(event);
			break;

		case CGUI_EVENT_TOUCH_BEGIN:
			_touch_begin(event);
			break;

		case CGUI_EVENT_TOUCH_UPDATE:
			_touch_update(event);
			break;

		case CGUI_EVENT_TOUCH_END:
			_touch_end(event);
			break;

		case CGUI_EVENT_BUTTON_PRESS:
			_button_press(event);
			break;

		case CGUI_EVENT_BUTTON_RELEASE:
			_button_release(event);
			break;

		case CGUI_EVENT_KEY_PRESS:
			_key_press(event);
			break;

		case CGUI_EVENT_KEY_RELEASE:
			_key_release(event);
			break;

		case CGUI_EVENT_POINTER_MOTION:
			_pointer(event);
			break;

		case CGUI_EVENT_LEAVE:
			_leave(event);
			break;

		case CGUI_EVENT_REDRAW:
			_redraw(event);

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
_accelerate(struct cgui_event *event)
{
	if (!event->window->valid)
	{
		return;
	}

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_button_press(struct cgui_event *event)
{
	if (!event->window->valid)
	{
		return;
	}

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_button_release(struct cgui_event *event)
{
	if (!event->window->valid)
	{
		return;
	}

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_close(struct cgui_event *event)
{
	event->window->fn_close(event->window);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_dummy_callback_event(struct cgui_event *event)
{
	(void)event;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_focus(struct cgui_event *event)
{
	if (!event->window->valid)
	{
		return;
	}

	window_update_state(event->window, CGUI_WINDOW_FOCUSED, true);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_key_press(struct cgui_event *event)
{
	if (!event->window->valid)
	{
		return;
	}

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_key_release(struct cgui_event *event)
{
	if (!event->window->valid)
	{
		return;
	}

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_leave(struct cgui_event *event)
{
	if (!event->window->valid)
	{
		return;
	}

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_map(struct cgui_event *event)
{
	if (!event->window->valid)
	{
		return;
	}

	window_update_state(event->window, CGUI_WINDOW_MAPPED, true);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_pointer(struct cgui_event *event)
{
	if (!event->window->valid)
	{
		return;
	}

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_reconfig(void)
{
	cgui_reconfig();
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_redraw(struct cgui_event *event)
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
_touch_begin(struct cgui_event *event)
{
	if (!event->window->valid)
	{
		return;
	}

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_touch_end(struct cgui_event *event)
{
	if (!event->window->valid)
	{
		return;
	}

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_touch_update(struct cgui_event *event)
{
	if (!event->window->valid)
	{
		return;
	}

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_transform(struct cgui_event *event)
{
	cgui_window *w = event->window;

	if (!event->window->valid)
	{
		return;
	}

	w->x = event->transform_x;
	w->y = event->transform_y;

	if (w->width  == event->transform_width
	 && w->height == event->transform_height)
	{
		return;
	}
	
	window_resize(w, event->transform_width, event->transform_height);

	// TODO update current grid
	// TODO update wm focus hints
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_unfocus(struct cgui_event *event)
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
_unmap(struct cgui_event *event)
{
	if (!event->window->valid)
	{
		return;
	}

	window_update_state(event->window, CGUI_WINDOW_MAPPED, false);

	// TODO propagate event to cells
}

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
#include "x11.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/* event handlers */

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
static void present              (struct cgui_event *) CGUI_NONNULL(1);
static void reconfig             (void);
static void redraw               (struct cgui_event *) CGUI_NONNULL(1);
static void touch_begin          (struct cgui_event *) CGUI_NONNULL(1);
static void touch_end            (struct cgui_event *) CGUI_NONNULL(1);
static void touch_update         (struct cgui_event *) CGUI_NONNULL(1);
static void transform            (struct cgui_event *) CGUI_NONNULL(1);
static void unfocus              (struct cgui_event *) CGUI_NONNULL(1);
static void unmap                (struct cgui_event *) CGUI_NONNULL(1);

/* other functions */

//static bool swap_input {struct cgui_event *} CGUI_NONNULL(1);

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

		case CGUI_EVENT_PRESENT:
			present(event);
			break;

		case CGUI_EVENT_UNKNOWN_XCB:
		case CGUI_EVENT_ENTER:
		case CGUI_EVENT_NONE:
		default:
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
	bool accepted;
	struct cgui_swap input;
	struct cgui_cell_event cell_event =
	{
		.type      = CGUI_CELL_EVENT_BUTTON_PRESS,
		.button_id = event->button_id,
		.button_x  = event->button_x,
		.button_y  = event->button_y,
	};

	if (!event->window->valid || event->button_id == 0)
	{
		return;
	}

	/* swap input */

	input = config_swap_input(event->button_id, event->button_mods, CONFIG_SWAP_BUTTONS);
	switch (input.type)
	{
		case CGUI_SWAP_TO_DEFAULT:
			break;

		case CGUI_SWAP_TO_VALUE:
			event->button_id = input.value;
			break;

		case CGUI_SWAP_TO_ACCELERATOR:
			event->window->accels[input.value - 1].fn(event->window, input.value);
			return;

		default:
			//action_process(input);
			return;
	}

	/* first update focus in case it was changed my an other input mean */

	window_focus_pointer(event->window, event->button_x, event->button_y);

	/* send cell event */

	cinputs_push(event->window->buttons, event->button_id, event->button_x, event->button_y, NULL);
	cell_event.button_n = cinputs_load(event->window->buttons);
	accepted = window_process_cell_event(event->window, event->window->focus, &cell_event);

	/* allow wm functions if the event is rejected with a matching button id */

	if (event->button_id == CONFIG->wm_button_move)
	{
		event->window->wm_move = !accepted;
	}
	else if (event->button_id == CONFIG->wm_button_resize)
	{
		event->window->wm_resize  = !accepted;
		event->window->old_width  = event->window->width;
		event->window->old_height = event->window->height;
	}
	else if (event->button_id == CONFIG->wm_button_fullscreen)
	{
		x11_window_toggle_fullscreen(event->window->x_id);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
button_release(struct cgui_event *event)
{
	struct cgui_swap input;
	struct cgui_cell_event cell_event =
	{
		.type        = CGUI_CELL_EVENT_BUTTON_RELEASE,
		.button_id   = event->button_id,
		.button_x    = event->button_x,
		.button_y    = event->button_y,
		.button_mods = event->button_mods,
	};

	if (!event->window->valid || event->button_id == 0)
	{
		return;
	}

	/* swap input */

	input = config_swap_input(event->button_id, event->button_mods, CONFIG_SWAP_BUTTONS);
	switch (input.type)
	{
		case CGUI_SWAP_TO_DEFAULT:
			break;

		case CGUI_SWAP_TO_VALUE:
			event->button_id = input.value;
			break;

		default:
			return;
	}

	/* send cell event */

	cinputs_pull_id(event->window->buttons, event->button_id);
	cell_event.button_n = cinputs_load(event->window->buttons);
	window_process_cell_event(event->window, event->window->focus, &cell_event);

	/* update focus in case of a drag action that ended up out of bounds of the cell */

	window_focus_pointer(event->window, event->button_x, event->button_y);
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
	size_t i;
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
	if (window_process_cell_event(event->window, event->window->focus, &cell_event))
	{
		return;
	}

	/* drag or resize window if event is rejected */

	if (event->window->wm_move && cinputs_find(event->window->buttons, CONFIG->wm_button_move, &i))
	{
		cgui_window_move(
			event->window,
			event->pointer_x - cinputs_x(event->window->buttons, i) + event->window->x,
			event->pointer_y - cinputs_y(event->window->buttons, i) + event->window->y
		);
	}
	else if ( !event->window->wait_resize
	 && event->window->wm_resize && cinputs_find(event->window->buttons, CONFIG->wm_button_resize, &i))
	{
		event->window->wait_resize = true; /* this is to avoid spamming resizes that are slow */
		cgui_window_resize(
			event->window,
			event->pointer_x - cinputs_x(event->window->buttons, i) + event->window->old_width,
			event->pointer_y - cinputs_y(event->window->buttons, i) + event->window->old_height);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
present(struct cgui_event *event)
{
	if (!event->window->valid)
	{
		return;
	}

	if (!CONFIG->alt_present)
	{
		window_draw(event->window);
	}
	
	event->window->wait_present = false;
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

	window_set_draw_level(event->window, WINDOW_DRAW_FULL);
	window_set_async_present(event->window);
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
	cgui_window *window = event->window;

	if (!window->valid)
	{
		return;
	}

	window->wait_resize = false;
	window->x           = event->transform_x;
	window->y           = event->transform_y;

	if (fabs(window->width  - event->transform_width)  < DBL_EPSILON
	 && fabs(window->height - event->transform_height) < DBL_EPSILON)
	{
		return;
	}

	/* extra redraw because an expose event is not received when resizing down */

	if (event->transform_width  < window->width
	 || event->transform_height < window->height)
	{
		window_set_draw_level(window, WINDOW_DRAW_FULL);
		window_set_async_present(window);
	}

	window_update_size(window, event->transform_width, event->transform_height);
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

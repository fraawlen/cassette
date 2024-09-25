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

#include "grid.h"
#include "event.h"
#include "cell.h"
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

static void   action_cell   (uint8_t, cgui_window *) CGUI_NONNULL(2);
static void   action_focus  (uint8_t, cgui_window *) CGUI_NONNULL(2);
static void   action_misc   (uint8_t);
static void   action_window (uint8_t, cgui_window *) CGUI_NONNULL(2);
static size_t swap_input    (struct cgui_event *)    CGUI_NONNULL(1);

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
	if (!event->window->valid || event->accelerator == 0 || event->accelerator > CGUI_CONFIG_ACCELS)
	{
		return;
	}

	event->window->accels[event->accelerator - 1].fn(event->window, event->accelerator);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
action_cell(uint8_t type, cgui_window *window)
{
	struct cgui_cell_event event;

	if (!window->focus.cell->valid)
	{
		return;
	}

	switch (type)
	{
		case CGUI_SWAP_CELL_SELECT_LESS:
			event.type = CGUI_CELL_EVENT_SELECT_LESS;
			break;

		case CGUI_SWAP_CELL_SELECT_MORE:
			event.type = CGUI_CELL_EVENT_SELECT_MORE;
			break;

		case CGUI_SWAP_CELL_SELECT_NONE:
			event.type = CGUI_CELL_EVENT_SELECT_NONE;
			break;

		case CGUI_SWAP_CELL_SELECT_ALL:
			event.type = CGUI_CELL_EVENT_SELECT_ALL;
			break;

		case CGUI_SWAP_CELL_REDRAW:
			cgui_cell_redraw(window->focus.cell);
			return;

		default:
			return;
	}

	window_process_cell_event(window, window->focus, &event);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
action_focus(uint8_t type, cgui_window *window)
{
	(void)window;

	switch (type)
	{
		// TODO

		default:
			break;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
action_misc(uint8_t type)
{
	switch (type)
	{
		case CGUI_SWAP_RECONFIG:
			cgui_reconfig();
			break;

		case CGUI_SWAP_EXIT:
			cgui_exit();
			break;

		default:
			break;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
action_window(uint8_t type, cgui_window *window)
{
	(void)window;

	switch (type)
	{
		case CGUI_SWAP_WINDOW_LOCK_GRID:
			window_update_state(window, CGUI_WINDOW_LOCKED_GRID, !window->state.locked_grid);
			window_set_draw_level(window, WINDOW_DRAW_FULL);
			window_update_size_hints(window);
			break;

		case CGUI_SWAP_WINDOW_LOCK_FOCUS:
			window_focus_lock(window, !window->state.locked_focus);
			break;

		case CGUI_SWAP_WINDOW_REDRAW:
			window_set_draw_level(window, WINDOW_DRAW_FULL);
			break;

		default:
			break;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
button_press(struct cgui_event *event)
{
	bool accepted;
	struct cgui_cell_event cell_event =
	{
		.type      = CGUI_CELL_EVENT_BUTTON_PRESS,
		.button_id = event->button_id,
		.button_x  = event->button_x,
		.button_y  = event->button_y,
	};

	if (!event->window->valid || (cell_event.button_id = swap_input(event)) == 0)
	{
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
	else if (!accepted && event->button_id == CONFIG->wm_button_fullscreen)
	{
		x11_window_toggle_fullscreen(event->window->x_id);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
button_release(struct cgui_event *event)
{
	struct cgui_cell_event cell_event =
	{
		.type        = CGUI_CELL_EVENT_BUTTON_RELEASE,
		.button_x    = event->button_x,
		.button_y    = event->button_y,
		.button_mods = event->button_mods,
	};

	if (!event->window->valid || (cell_event.button_id = swap_input(event)) == 0)
	{
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
	struct cgui_cell_event cell_event =
	{
		.type     = CGUI_CELL_EVENT_KEY_PRESS,
		.key_mods = event->key_mods,
	};

	if (!event->window->valid || (cell_event.key_code = swap_input(event)) == 0)
	{
		return;
	}

	x11_key(event->key_code, event->key_mods, &cell_event.key_sym, &cell_event.utf32, cell_event.utf8);
	window_process_cell_event(event->window, event->window->focus, &cell_event);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
key_release(struct cgui_event *event)
{
	struct cgui_cell_event cell_event =
	{
		.type     = CGUI_CELL_EVENT_KEY_RELEASE,
		.key_mods = event->key_mods,
	};

	if (!event->window->valid || (cell_event.key_code = swap_input(event)) == 0)
	{
		return;
	}

	x11_key(event->key_code, event->key_mods, &cell_event.key_sym, &cell_event.utf32, cell_event.utf8);
	window_process_cell_event(event->window, event->window->focus, &cell_event);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
leave(struct cgui_event *event)
{
	if (!event->window->valid)
	{
		return;
	}

	/* useful when compact themes are used as sometime pointer motion may not be detected when */
	/* leaving the window quickly.                                                             */

	window_focus_pointer(event->window, -1.0, -1.0);
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
			event->pointer_y - cinputs_y(event->window->buttons, i) + event->window->y);
	}
	else if (!event->window->wait_resize
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

static size_t
swap_input(struct cgui_event *event)
{
	size_t value;
	struct cgui_mods mods;
	struct cgui_swap swap;
	enum config_swap group;
	bool press;

	if (event->button_id == 0)
	{
		return 0;
	}

	/* select modes */

	switch (event->type)
	{
		case CGUI_EVENT_BUTTON_PRESS:
			mods  = event->button_mods;
			value = event->button_id;
			press = true;
			group = CONFIG_SWAP_BUTTONS;
			break;

		case CGUI_EVENT_BUTTON_RELEASE:
			mods  = event->button_mods;
			value = event->button_id;
			group = CONFIG_SWAP_BUTTONS;
			press = false;
			break;

		case CGUI_EVENT_KEY_PRESS:
			mods  = event->key_mods;
			value = event->key_code;
			group = CONFIG_SWAP_KEYS;
			press = true;
			break;

		case CGUI_EVENT_KEY_RELEASE:
			mods  = event->key_mods;
			value = event->key_code;
			group = CONFIG_SWAP_KEYS;
			press = false;
			break;

		default:
			return false;
	}

	/* press and release swaps */

	swap = config_swap_input(value, mods, group);
	switch (swap.type)
	{
		case CGUI_SWAP_TO_NONE:
			return 0;

		case CGUI_SWAP_TO_DEFAULT:
			return value;

		case CGUI_SWAP_TO_VALUE:
			return swap.value;

		default:
			break;
	}

	/* press only swaps */

	if (!press)
	{
		return 0;
	}

	switch (swap.type)
	{
		case CGUI_SWAP_TO_ACCELERATOR:
			event->window->accels[swap.value - 1].fn(event->window, swap.value);
			break;

		case CGUI_SWAP_TO_CLIPBOARD_CUT:
		case CGUI_SWAP_TO_CLIPBOARD_COPY:
		case CGUI_SWAP_TO_CLIPBOARD_PASTE:
			// TODO
			break;

		case CGUI_SWAP_TO_ACTION_CELL:
			action_cell(swap.value, event->window);
			break;

		case CGUI_SWAP_TO_ACTION_FOCUS:
			action_focus(swap.value, event->window);
			break;

		case CGUI_SWAP_TO_ACTION_WINDOW:
			action_window(swap.value, event->window);
			break;

		case CGUI_SWAP_TO_ACTION_MISC:
			action_misc(swap.value);
			break;

		default:
			break;
	}

	return 0;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
touch_begin(struct cgui_event *event)
{
	struct grid_area area;
	struct cgui_cell_event cell_event_touch =
	{
		.type     = CGUI_CELL_EVENT_TOUCH_BEGIN,
		.touch_id = event->touch_id,
		.touch_x  = event->touch_y,
		.touch_y  = event->touch_x,
	};

	struct cgui_cell_event cell_event_focus =
	{
		.type    = CGUI_CELL_EVENT_FOCUS_GAIN_BY_TOUCH,
		.focus_x = event->touch_x,
		.focus_y = event->touch_y,
	};

	if (!event->window->valid)
	{
		return;
	}

	area = window_area_at_coords(event->window, event->touch_x, event->touch_y);

	/* if it's the first touch event, update focus */

	if (cinputs_load(event->window->touches) == 0
	 && window_process_cell_event(event->window, area, &cell_event_focus))
	{
		window_focus(event->window, area);
	}

	/* save event to tracker and send it to cell */

	cinputs_push(event->window->touches, event->touch_id, event->touch_x, event->touch_y, area.cell);
	cell_event_touch.touch_n = window_cell_touches(event->window, area.cell);
	window_process_cell_event(event->window, area, &cell_event_touch);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
touch_end(struct cgui_event *event)
{
	size_t n;
	struct grid_area area;
	struct cgui_cell_event cell_event =
	{
		.type     = CGUI_CELL_EVENT_TOUCH_END,
		.touch_id = event->touch_id,
		.touch_x  = event->touch_y,
		.touch_y  = event->touch_x,
	};

	if (!event->window->valid)
	{
		return;
	}

	area = window_touch_area(event->window, event->touch_id);

	/* update tracker and send event to cell */

	cinputs_pull_id(event->window->touches, event->touch_id);
	n = window_cell_touches(event->window, area.cell);
	cell_event.touch_n = n;
	window_process_cell_event(event->window, area, &cell_event);

	/* if it's the last touch on the focused cell, unfocus */

	if (n == 0 
	 && !CONFIG->persistent_touch
	 && !event->window->state.locked_focus
	 &&  event->window->focus.cell == area.cell)
	{
		window_focus(event->window, GRID_AREA_NONE);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
touch_update(struct cgui_event *event)
{
	struct grid_area area;
	struct cgui_cell_event cell_event =
	{
		.type     = CGUI_CELL_EVENT_TOUCH_UPDATE,
		.touch_id = event->touch_id,
		.touch_x  = event->touch_y,
		.touch_y  = event->touch_x,
	};

	if (!event->window->valid)
	{
		return;
	}
	
	area = window_touch_area(event->window, event->touch_id);
	cell_event.touch_n = window_cell_touches(event->window, area.cell);
	window_process_cell_event(event->window, area, &cell_event);
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

	if ((!CONFIG->persistent_pointer && cinputs_load(event->window->buttons) > 0)
	 || (!CONFIG->persistent_touch   && cinputs_load(event->window->touches) > 0))
	{
		window_focus(event->window, GRID_AREA_NONE);
	}

	cinputs_clear(event->window->buttons);
	cinputs_clear(event->window->touches);
	window_update_state(event->window, CGUI_WINDOW_FOCUSED, false);
	window_cancel_cell_events(event->window);
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

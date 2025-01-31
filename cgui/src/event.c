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
#include <float.h>
#include <math.h>
#include <xcb/xcb.h>

#include "grid.h"
#include "event.h"
#include "cell.h"
#include "config.h"
#include "main.h"
#include "screen.h"
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
static void focus_window         (struct cgui_event *) CGUI_NONNULL(1);
static void key_press            (struct cgui_event *) CGUI_NONNULL(1);
static void key_release          (struct cgui_event *) CGUI_NONNULL(1);
static void leave                (struct cgui_event *) CGUI_NONNULL(1);
static void map                  (struct cgui_event *) CGUI_NONNULL(1);
static void pointer              (struct cgui_event *) CGUI_NONNULL(1);
static void pointer_raw          (void);
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

static void   action_app     (uint8_t);
static void   action_cell    (uint8_t, cgui_window *)                            CGUI_NONNULL(2);
static void   action_window  (uint8_t, cgui_window *)                            CGUI_NONNULL(2);
static void   clipboard      (enum cgui_cell_event_type, uint8_t, cgui_window *) CGUI_NONNULL(3);
static void   focus_cell     (uint8_t, cgui_window *)                            CGUI_NONNULL(2);
static void   popup_redirect (struct cgui_event *)                               CGUI_NONNULL(1);
static size_t swap_input     (struct cgui_event *)                               CGUI_NONNULL(1);

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
	popup_redirect(event);

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
			focus_window(event);
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

		case CGUI_EVENT_POINTER_MOTION_RAW:
			pointer_raw();
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

	fn_event(event);
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
action_app(uint8_t type)
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
action_cell(uint8_t type, cgui_window *window)
{
	struct cgui_cell_event event;

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
action_window(uint8_t type, cgui_window *window)
{
	(void)window;
	
	switch (type)
	{
		case CGUI_SWAP_WINDOW_LOCK_GRID:
			window_update_state(window, CGUI_WINDOW_LOCKED_GRID, !window->state.locked_grid);
			window_update_size_hints(window);
			break;

		case CGUI_SWAP_WINDOW_LOCK_FOCUS:
			window_focus_lock(window, !window->state.locked_focus);
			break;

		case CGUI_SWAP_WINDOW_REDRAW:
			window_schedule_draw(window, WINDOW_DRAW_FULL, 0);
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

	if (event->button_id == CONFIG->input_wm_move)
	{
		event->window->wm_move = !accepted;
	}
	else if (event->button_id == CONFIG->input_wm_resize)
	{
		event->window->wm_resize  = !accepted;
		event->window->old_width  = event->window->width;
		event->window->old_height = event->window->height;
	}
	else if (!accepted && event->button_id == CONFIG->input_wm_fullscreen)
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
clipboard(enum cgui_cell_event_type type, uint8_t id, cgui_window *window)
{
	struct cgui_cell_event event =
	{
		.type      = type,
		.clipboard = id,
	};

	window_process_cell_event(window, window->focus, &event);
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
focus_cell(uint8_t type, cgui_window *window)
{
	size_t n;
	enum cgui_focus focus;
	struct grid_area area;
	struct cgui_cell_event event =
	{
		.type     = CGUI_CELL_EVENT_SUBFOCUS,
		.subfocus = type,
	};

	/* explicit focus change through a swap input always break focus lock */

	window_focus_lock(window, false);

	/* focus removal                                                              */
	/* special case : if the window did not have a focused area, and the window   */
	/* happend to be a popup, then deactivate that window and any popup childrens */

	if (type == CGUI_FOCUS_NONE)
	{
		if (window->focus.cell->valid)
		{
			window_focus(window, GRID_AREA_NONE);
		}
		else if (window->type == CGUI_WINDOW_POPUP)
		{
			cgui_window_deactivate(window);
		}
		return;
	}

	/* in case the focused area hosts a meta-cell with its own subfocus, send a subfocus event first */
	/* if it is accepted, then it means that the focused cell updated its subfocus and therefore the */
	/* window-level focus should not be modified.                                                    */
	/* note : only the focus values that are entirely relative to the current top-level focus        */
	/* position are relevant to this event. (For example, CGUI_FOCUS_LAST is relative to the grid    */
	/* and the position of the focused area doesn't matter.)                                         */

	switch (type)
	{
		case CGUI_FOCUS_NEXT:
		case CGUI_FOCUS_PREV:
			if (window_process_cell_event(window, window->focus, &event))
			{
				return;
			}
			break;

		default:
			break;
	};

	/* query the current grid to find the next area that matches the new focus direction   */
	/* continue to seek until the focus event is accepted                                  */
	/* if the query does not returns any valid area, nor any cell accepts the focus event, */
	/* exit without updating the current focus.                                            */

	area  = window->focus;
	focus = type;
	n     = 0;

	do
	{
		grid_find_focus(window->shown_grid, &area, &focus);
		event.type  = CGUI_CELL_EVENT_FOCUS_GAIN_BY_ACTION;
		event.focus = type;
		n++;
	}
	while (!window_process_cell_event(window, area, &event) && n < cref_length(window->shown_grid->areas));

	window_focus(window, area);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
focus_window(struct cgui_event *event)
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
	/* unless the window is a popup               */

	if (event->window->type != CGUI_WINDOW_NORMAL
	 && event->window->type != CGUI_WINDOW_DIALOG)
	{
		return;
	}

	if (
	   !event->window->wait_move
	 && event->window->wm_move
	 && cinputs_find(event->window->buttons, CONFIG->input_wm_move, &i))
	{
		cgui_window_move(
			event->window,
			event->pointer_x - cinputs_x(event->window->buttons, i) + event->window->x,
			event->pointer_y - cinputs_y(event->window->buttons, i) + event->window->y);
	}
	else if (
	   !event->window->wait_resize
	 && event->window->wm_resize
	 && cinputs_find(event->window->buttons, CONFIG->input_wm_resize, &i))
	{
		cgui_window_resize(
			event->window,
			event->pointer_x - cinputs_x(event->window->buttons, i) + event->window->old_width,
			event->pointer_y - cinputs_y(event->window->buttons, i) + event->window->old_height);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
pointer_raw(void)
{
	/* update the stored pointer position only when the following option is enabled to avoid spamming  */
	/* X pointer requests. This is the only option that requires constant pointer tracking inside CGUI */
	/* If the user needs constant pointer tracking while that option is disabled, he can call          */
	/* cgui_screen_pointer_position() manually from within a custom event callback.                    */

	if (!CONFIG->shadows_reactive)
	{
		return;
	}

	CREF_FOR_EACH(main_windows(), i)
	{
		cgui_window_redraw((cgui_window *)cref_ptr(main_windows(), i));
	}

	screen_pointer_update();
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
popup_redirect(struct cgui_event *event)
{
	cgui_window *popup;
	cinputs *inputs;
	bool motion;
	double *x;
	double *y;

	if ((popup = window_popup_last()) == CGUI_WINDOW_PLACEHOLDER)
	{
		return;
	}

	/* get input parameters, other events do not matter      */
	/* keyboard events, because they don't have coordinates, */
	/* they always target the last popup                     */

	switch (event->type)
	{
		case CGUI_EVENT_BUTTON_PRESS:
		case CGUI_EVENT_BUTTON_RELEASE:
			x = &event->button_x;
			y = &event->button_y;
			inputs = popup->buttons;
			motion = false;
			break;

		case CGUI_EVENT_POINTER_MOTION:
			x = &event->pointer_x;
			y = &event->pointer_y;
			inputs = popup->buttons;
			motion = true;
			break;

		case CGUI_EVENT_TOUCH_BEGIN:
		case CGUI_EVENT_TOUCH_END:
			x = &event->touch_x;
			y = &event->touch_y;
			inputs = popup->touches;
			motion = false;
			break;

		case CGUI_EVENT_TOUCH_UPDATE:
			x = &event->touch_x;
			y = &event->touch_y;
			inputs = popup->touches;
			motion = true;
			break;

		case CGUI_EVENT_KEY_PRESS:
		case CGUI_EVENT_KEY_RELEASE:
			event->window = window_popup_last();
			return;
	
		default:
			return;
	}

	/* redirect input only if there's no already ongoing input */

	if (cinputs_load(inputs) == 0)
	{
		popup = window_popup_at_coords(*x, *y);
		if (!motion)
		{
			if (popup == CGUI_WINDOW_PLACEHOLDER)
			{
				cgui_window_deactivate_all_popups();
			}
			else
			{
				cgui_window_deactivate_children_popups(popup);
			}
		}
	}

	/* transform input coordinates (that are relative to the root window) to popup's */

	*x -= popup->x;
	*y -= popup->y;
	event->window = popup;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
present(struct cgui_event *event)
{
	if (!event->window->valid)
	{
		return;
	}

	if (CONFIG->render_mode == CGUI_RENDER_DEFERRED)
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

	window_schedule_draw(event->window, WINDOW_DRAW_FULL_ASYNC, 0);
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
			return 0;
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
			clipboard(CGUI_CELL_EVENT_CLIPBOARD_CUT,   swap.value, event->window);
			break;

		case CGUI_SWAP_TO_CLIPBOARD_COPY:
			clipboard(CGUI_CELL_EVENT_CLIPBOARD_COPY,  swap.value, event->window);
			break;

		case CGUI_SWAP_TO_CLIPBOARD_PASTE:
			clipboard(CGUI_CELL_EVENT_CLIPBOARD_PASTE, swap.value, event->window);
			break;

		case CGUI_SWAP_TO_FOCUS:
			focus_cell(swap.value, event->window);
			break;

		case CGUI_SWAP_TO_ACTION_APP:
			action_app(swap.value);
			break;

		case CGUI_SWAP_TO_ACTION_CELL:
			action_cell(swap.value, event->window);
			break;

		case CGUI_SWAP_TO_ACTION_WINDOW:
			action_window(swap.value, event->window);
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
	 && !CONFIG->input_sticky_touch
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
	window->wait_move   = false;
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
		window_schedule_draw(window, WINDOW_DRAW_FULL_ASYNC, 0);
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

	if ((!CONFIG->input_sticky_pointer && cinputs_load(event->window->buttons) > 0)
	 || (!CONFIG->input_sticky_touch   && cinputs_load(event->window->touches) > 0))
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

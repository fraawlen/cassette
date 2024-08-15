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
#include <stdio.h>
#include <string.h>
#include <unistd.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define MSG "Hello World!"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void _on_accel (cgui_window *, int);
static void _on_close (cgui_window *);
static void _on_draw  (cgui_window *);
static void _on_state (cgui_window *, enum cgui_window_state_mask);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static cgui_cell   *_cell   = CGUI_CELL_PLACEHOLDER;
static cgui_grid   *_grid   = CGUI_GRID_PLACEHOLDER;
static cgui_window *_window = CGUI_WINDOW_PLACEHOLDER;

/************************************************************************************************************/
/* MAIN *****************************************************************************************************/
/************************************************************************************************************/

/**
 * Standard hello world example.
 */

 int
 main(int argc, char **argv)
 {
	/* Setup */

	cgui_init(argc, argv);

	_window = cgui_window_create();
	_grid   = cgui_grid_create(1, 1);
	_cell   = cgui_cell_create();
	
	/* Cell setup */

	// TODO

	/* Grid setup */

	cgui_grid_resize_col(_grid, 0, strlen(MSG));
	cgui_grid_set_col_flex(_grid, 0, 1.0);
	cgui_grid_set_row_flex(_grid, 0, 1.0);
	cgui_grid_assign_cell(_grid, _cell, 0, 0, 1, 1);

	/* Window setup */

	cgui_window_push_grid(_window, _grid);
	cgui_window_rename(_window, "Hi");
	cgui_window_set_accelerator(_window, 1, "Hello", _on_accel);
	cgui_window_set_accelerator(_window, 2, "World", _on_accel);
	cgui_window_on_draw(_window, _on_draw);
	cgui_window_on_close(_window, _on_close);
	cgui_window_on_state(_window, _on_state);
	cgui_window_activate(_window);

	/* Run */

	cgui_run();

	/* End */

	if (cgui_error())
	{
		printf("Gui has failed during operation.\n");
	}

	cgui_window_destroy(_window);
	cgui_grid_destroy(_grid);
	cgui_cell_destroy(_cell);

	cgui_reset();

	return 0;
 }

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
_on_accel(cgui_window *window, int id)
{
	(void)window;

	printf("accelerator %i triggered\n", id);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_on_close(cgui_window *window)
{
	cgui_window_deactivate(window);
	
	printf("window closed\n");
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_on_draw(cgui_window *window)
{
	(void)window;

	printf("window redrawn\n");
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_on_state(cgui_window *window, enum cgui_window_state_mask mask)
{
	struct cgui_window_state_flags state;

	state = cgui_window_state(window);

	switch (mask)
	{
		case CGUI_WINDOW_ACTIVE:
			printf("window %s\n", state.active ? "activated" : "deactivated");
			break;

		case CGUI_WINDOW_MAPPED:
			printf("window %s\n", state.mapped ? "mapped" : "unmapped");
			break;

		case CGUI_WINDOW_FOCUSED:
			printf("window %s\n", state.focused ? "focused" : "unfocused");
			break;

		case CGUI_WINDOW_DISABLED:
			printf("window %s\n", state.disabled ? "disabled" : "enabled");
			break;

		case CGUI_WINDOW_LOCKED_GRID:
			printf("window %s\n", state.locked_grid ? "grid locked" : "grid unlocked");
			break;

		case CGUI_WINDOW_LOCKED_FOCUS:
			printf("window %s\n", state.locked_focus ? "focus locked" : "focus unlocked");
			break;

		default:
			return;
	}
}

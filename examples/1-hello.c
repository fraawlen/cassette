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

static void on_accel (cgui_window *, int);
static void on_click (cgui_cell   *);
static void on_close (cgui_window *);
static void on_draw  (cgui_window *, unsigned long, unsigned long);
static void on_state (cgui_window *, enum cgui_window_state_mask);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static cgui_cell   *filler   = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *stripes  = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *button_1 = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *button_2 = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *button_3 = CGUI_CELL_PLACEHOLDER;
static cgui_grid   *grid_1   = CGUI_GRID_PLACEHOLDER;
static cgui_grid   *grid_2   = CGUI_GRID_PLACEHOLDER;
static cgui_window *window   = CGUI_WINDOW_PLACEHOLDER;

static struct cgui_screen screen;

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

	window   = cgui_window_create();
	grid_1   = cgui_grid_create(1, 1);
	grid_2   = cgui_grid_create(2, 5);
	filler   = cgui_filler_create();
	stripes  = cgui_stripes_create();
	button_1 = cgui_button_create();
	button_2 = cgui_button_create();
	button_3 = cgui_button_create();
	screen   = cgui_screen_primary_specs();

	/* Cell setup */

	cgui_button_on_click(button_1, on_click);
	cgui_button_on_click(button_2, on_click);
	cgui_button_on_click(button_3, on_click);

	cgui_button_set_label(button_1, "button");
	cgui_button_set_label(button_2, "button");
	cgui_button_set_label(button_3, "button");

	cgui_button_disable(button_3);

	/* Grid 1 setup */

	cgui_grid_resize_col(grid_1, 0, strlen(MSG));
	cgui_grid_set_col_flex(grid_1, 0, 1.0);
	cgui_grid_set_row_flex(grid_1, 0, 1.0);
	cgui_grid_assign_cell(grid_1, stripes, 0, 0, 1, 1);
	
	/* Grid 2 setup */

	cgui_grid_resize_col(grid_2, 0, strlen(MSG));
	cgui_grid_resize_col(grid_2, 1, 10);
	cgui_grid_set_col_flex(grid_2, 1, 1.0);
	cgui_grid_set_row_flex(grid_2, 0, 1.0);

	cgui_grid_assign_cell(grid_2, filler,   0, 0, 1, 1);
	cgui_grid_assign_cell(grid_2, button_1, 0, 1, 1, 1);
	cgui_grid_assign_cell(grid_2, button_2, 0, 2, 1, 1);
	cgui_grid_assign_cell(grid_2, button_3, 0, 3, 1, 1);
	cgui_grid_assign_cell(grid_2, filler,   1, 0, 1, 4);
	cgui_grid_assign_cell(grid_2, stripes,  0, 4, 2, 1);
	
	/* Window setup */

	cgui_window_push_grid(window, grid_1);
	cgui_window_push_grid(window, grid_2);
	cgui_window_rename(window, "Hi");
	cgui_window_set_accelerator(window, 1, "Hello", on_accel);
	cgui_window_set_accelerator(window, 2, "World", on_accel);
	cgui_window_on_draw(window, on_draw);
	cgui_window_on_close(window, on_close);
	cgui_window_on_state(window, on_state);
	cgui_window_resize(window, 358, 358);
	cgui_window_activate(window);

	/* Run */

	cgui_run();

	/* End */

	if (cgui_error())
	{
		printf("Gui has failed during operation.\n");
	}

	cgui_window_destroy(window);
	cgui_grid_destroy(grid_1);
	cgui_grid_destroy(grid_2);
	cgui_cell_destroy(filler);
	cgui_cell_destroy(stripes);
	cgui_cell_destroy(button_1);
	cgui_cell_destroy(button_2);
	cgui_cell_destroy(button_3);

	cgui_reset();

	return 0;
 }

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
on_accel(cgui_window *w, int id)
{
	(void)w;

	printf("accelerator %i triggered\n", id);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
on_click(cgui_cell *c)
{
	(void)c;

	printf("button clicked\n");
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
on_close(cgui_window *w)
{
	cgui_window_deactivate(w);
	
	printf("window closed\n");
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
on_draw(cgui_window *w, unsigned long delay_1, unsigned long delay_2)
{
	(void)w;

	printf("window redrawn (%lu / %lu)\n", delay_1, delay_2);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
on_state(cgui_window *w, enum cgui_window_state_mask mask)
{
	struct cgui_window_state_flags state;

	state = cgui_window_state(w);

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

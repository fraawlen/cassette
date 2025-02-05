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
#include <string.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void on_click (cgui_cell *);
static void on_close (cgui_window *);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static cgui_cell   *filler  = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *label   = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *close   = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *proceed = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *cancel  = CGUI_CELL_PLACEHOLDER;
static cgui_grid   *grid_w  = CGUI_GRID_PLACEHOLDER;
static cgui_grid   *grid_d  = CGUI_GRID_PLACEHOLDER;
static cgui_window *window  = CGUI_WINDOW_PLACEHOLDER;
static cgui_window *dialog  = CGUI_WINDOW_PLACEHOLDER;

/************************************************************************************************************/
/* MAIN *****************************************************************************************************/
/************************************************************************************************************/

/**
 * Dialog example creates a primary window with a button that, when clicked,
 * opens a modal dialog asking the user for exit confirmation.
 */

 int
 main(int argc, char **argv)
 {
	/* Instantiation */

	cgui_init(argc, argv);

	window  = cgui_window_create();
	dialog  = cgui_window_create();
	grid_w  = cgui_grid_create(1, 1);
	grid_d  = cgui_grid_create(3, 2);
	filler  = cgui_stripes_create();
	label   = cgui_label_create();
	proceed = cgui_button_create();
	cancel  = cgui_button_create();
	close   = cgui_button_create();

	/* Cell setup */

	cgui_label_set(label, "Are you sure you want to exit ?");

	cgui_button_set_label(proceed, "PROCEED");
	cgui_button_set_label(cancel,  "CANCEL");
	cgui_button_set_label(close,   "Close\nthis\nwindow");

	cgui_button_on_click(proceed, on_click);
	cgui_button_on_click(cancel,  on_click);
	cgui_button_on_click(close,   on_click);

	/* Main grid setup */

	cgui_grid_resize_col(grid_w, 0, 6);
	cgui_grid_resize_row(grid_w, 0, 3);

	cgui_grid_set_col_flex(grid_w, 0, 1.0);
	cgui_grid_set_row_flex(grid_w, 0, 1.0);

	cgui_grid_assign_cell(grid_w, close, 0, 0, 1, 1);
	
	/* Dialog grid setup */

	cgui_grid_resize_col(grid_d, 0, 17);
	cgui_grid_resize_col(grid_d, 1, 7);
	cgui_grid_resize_col(grid_d, 2, 7);

	cgui_grid_set_col_flex(grid_d, 0, 1.0);

	cgui_grid_assign_cell(grid_d, label,   0, 0, 3, 1);	
	cgui_grid_assign_cell(grid_d, filler,  0, 1, 1, 1);	
	cgui_grid_assign_cell(grid_d, proceed, 1, 1, 1, 1);	
	cgui_grid_assign_cell(grid_d, cancel,  2, 1, 1, 1);	

	/* Window & dialog setup */

	cgui_window_push_grid(window, grid_w);
	cgui_window_on_close(window, on_close);
	cgui_window_rename(window, "Close ?");
	cgui_window_activate(window);

	cgui_window_push_grid(dialog, grid_d);
	cgui_window_on_close(dialog, on_close);
	cgui_window_rename(dialog, "Dialog");
	cgui_window_tack(dialog, window);

	/* Run */

	cgui_run();

	/* End & cleanup */

	if (cgui_error())
	{
		printf("Gui has failed during operation (%i).\n", cgui_error());
	}

	cgui_window_destroy(window);
	cgui_window_destroy(dialog);
	cgui_grid_destroy(grid_w);
	cgui_grid_destroy(grid_d);
	cgui_cell_destroy(proceed);
	cgui_cell_destroy(cancel);
	cgui_cell_destroy(close);
	cgui_cell_destroy(label);
	cgui_cell_destroy(filler);

	cgui_reset();

	return 0;
 }

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
on_click(cgui_cell *c)
{
	if (c == close)
	{
		cgui_window_disable(window);
		cgui_window_activate(dialog);
	}
	else if (c == cancel)
	{
		cgui_window_deactivate(dialog);
		cgui_window_enable(window);
	}
	else
	{
		cgui_exit();
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
on_close(cgui_window *w)
{
	(void)w;

	on_click(w == window ? close : cancel);
}

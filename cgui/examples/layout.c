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
#include <cassette/ccfg.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void on_click (cgui_cell *);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static cgui_cell   *filler   = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *button_1 = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *button_2 = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *button_3 = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *button_4 = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *button_5 = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *button_m = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *status   = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *view     = CGUI_CELL_PLACEHOLDER;
static cgui_grid   *grid_d   = CGUI_GRID_PLACEHOLDER;
static cgui_grid   *grid_m   = CGUI_GRID_PLACEHOLDER;
static cgui_grid   *grid_mm  = CGUI_GRID_PLACEHOLDER;
static cgui_window *window   = CGUI_WINDOW_PLACEHOLDER;

/************************************************************************************************************/
/* MAIN *****************************************************************************************************/
/************************************************************************************************************/

/**
 * Example featuring responsive layouts : a desktop and mobile version.
 * The mobile layout has 2 grids, a default one, and one that has the menu that's normally
 * visible as a side-bar in the desktop layout.
 */

 int
 main(int argc, char **argv)
 {
	/* Instantiation */

	cgui_init(argc, argv);

	window   = cgui_window_create();
	grid_d   = cgui_grid_create(2, 7);
	grid_m   = cgui_grid_create(1, 7);
	grid_mm  = cgui_grid_create(1, 7);
	button_1 = cgui_button_create();
	button_2 = cgui_button_create();
	button_3 = cgui_button_create();
	button_4 = cgui_button_create();
	button_5 = cgui_button_create();
	button_m = cgui_button_create();
	filler   = cgui_filler_create();
	status   = cgui_label_create();
	view     = cgui_placeholder_create();

	/* Cell setup */

	cgui_label_set  (status, "Status bar");
	cgui_label_align(status, CGUI_ALIGN_RIGHT);

	cgui_button_set_label(button_1, "Button 1");
	cgui_button_set_label(button_2, "Button 2");
	cgui_button_set_label(button_3, "Button 3");
	cgui_button_set_label(button_4, "Button 4");
	cgui_button_set_label(button_5, "Button 5");
	cgui_button_set_label(button_m, "Toggle Menu");
	cgui_button_on_click (button_m, on_click);

	/* Desktop grid setup */

	cgui_grid_resize_col  (grid_d, 0,   8);
	cgui_grid_resize_col  (grid_d, 1,  20);
	cgui_grid_set_col_flex(grid_d, 1, 1.0);
	cgui_grid_set_row_flex(grid_d, 5, 1.0);

	cgui_grid_assign_cell(grid_d, button_1, 0, 0, 1, 1);
	cgui_grid_assign_cell(grid_d, button_2, 0, 1, 1, 1);
	cgui_grid_assign_cell(grid_d, button_3, 0, 2, 1, 1);
	cgui_grid_assign_cell(grid_d, button_4, 0, 3, 1, 1);
	cgui_grid_assign_cell(grid_d, button_5, 0, 4, 1, 1);
	cgui_grid_assign_cell(grid_d, filler,   0, 5, 1, 2);
	cgui_grid_assign_cell(grid_d, view,     1, 0, 1, 6);
	cgui_grid_assign_cell(grid_d, status,   1, 6, 1, 1);
	
	/* Mobile grid setup */

	cgui_grid_resize_col  (grid_m, 0,  11);
	cgui_grid_set_col_flex(grid_m, 0, 1.0);
	cgui_grid_set_row_flex(grid_m, 1, 1.0);

	cgui_grid_assign_cell(grid_m, status,   0, 0, 1, 1);
	cgui_grid_assign_cell(grid_m, view,     0, 1, 1, 5);
	cgui_grid_assign_cell(grid_m, button_m, 0, 6, 1, 1);
	
	/* Mobile menu grid setup */

	cgui_grid_resize_col   (grid_mm, 0,  11);
	cgui_grid_set_col_flex (grid_mm, 0, 1.0);
	cgui_grid_set_row_flex (grid_mm, 0, 1.0);
	cgui_grid_set_reference(grid_mm, grid_m);

	cgui_grid_assign_cell(grid_mm, filler,   0, 0, 1, 1);
	cgui_grid_assign_cell(grid_mm, button_1, 0, 1, 1, 1);
	cgui_grid_assign_cell(grid_mm, button_2, 0, 2, 1, 1);
	cgui_grid_assign_cell(grid_mm, button_3, 0, 3, 1, 1);
	cgui_grid_assign_cell(grid_mm, button_4, 0, 4, 1, 1);
	cgui_grid_assign_cell(grid_mm, button_5, 0, 5, 1, 1);
	cgui_grid_assign_cell(grid_mm, button_m, 0, 6, 1, 1);
	
	/* Window setup */

	cgui_window_push_grid(window, grid_d);
	cgui_window_push_grid(window, grid_m);
	cgui_window_rename(window, "responsive layouts");
	cgui_window_activate(window);

	/* Run */

	cgui_run();

	/* End & cleanup */

	if (cgui_error())
	{
		printf("Gui has failed during operation.\n");
	}

	cgui_window_destroy(window);
	cgui_grid_destroy(grid_d);
	cgui_grid_destroy(grid_m);
	cgui_grid_destroy(grid_mm);
	cgui_cell_destroy(button_1);
	cgui_cell_destroy(button_2);
	cgui_cell_destroy(button_3);
	cgui_cell_destroy(button_4);
	cgui_cell_destroy(button_5);
	cgui_cell_destroy(button_m);
	cgui_cell_destroy(filler);
	cgui_cell_destroy(status);
	cgui_cell_destroy(view);

	cgui_reset();

	return 0;
 }

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
on_click(cgui_cell *c)
{
	(void)c;

	cgui_window_swap_grid(window, grid_m, grid_mm);
}

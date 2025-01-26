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

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define INI    0.0
#define MIN    0.0
#define MAX  100.0
#define STEP   5.0

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void on_click (cgui_cell *);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static cgui_cell   *stripes  = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *gauge_1  = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *gauge_2  = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *gauge_3  = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *gauge_4  = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *button_i = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *button_d = CGUI_CELL_PLACEHOLDER;
static cgui_grid   *grid     = CGUI_GRID_PLACEHOLDER;
static cgui_window *window   = CGUI_WINDOW_PLACEHOLDER;

double val = INI;

/************************************************************************************************************/
/* MAIN *****************************************************************************************************/
/************************************************************************************************************/

int
main(int argc, char **argv)
{
	/* Instantiation */

	cgui_init(argc, argv);

	window   = cgui_window_create();
	grid     = cgui_grid_create(4, 4);
	gauge_1  = cgui_gauge_create();
	gauge_2  = cgui_gauge_create();
	gauge_3  = cgui_gauge_create();
	gauge_4  = cgui_gauge_create();
	button_i = cgui_button_create();
	button_d = cgui_button_create();
	stripes  = cgui_stripes_create();

	/* Cell setup */

	cgui_gauge_set_value(gauge_1, 100);

	cgui_gauge_style_percent(gauge_2, 1);
	cgui_gauge_set_value(gauge_2, 15.23);

	cgui_gauge_resize_label(gauge_3, 7);
	cgui_gauge_style_label(gauge_3, 2, "GB");
	cgui_gauge_limit_value(gauge_3, 0.0, 32.0);
	cgui_gauge_set_value(gauge_3, 21.28627);

	cgui_gauge_resize_label(gauge_4, 0);
	cgui_gauge_set_value(gauge_4, val);
	cgui_gauge_limit_value(gauge_4, MIN, MAX);

	cgui_button_set_label(button_i, "INC");
	cgui_button_on_click(button_i, on_click);

	cgui_button_set_label(button_d, "DEC");
	cgui_button_on_click(button_d, on_click);

	/* Grid setup */

	cgui_grid_resize_col(grid, 0, 1);
	cgui_grid_resize_col(grid, 1, 3);
	cgui_grid_resize_col(grid, 2, 3);
	cgui_grid_resize_col(grid, 3, 1);

	cgui_grid_set_col_flex(grid, 0, 1.0);
	cgui_grid_set_row_flex(grid, 0, 1.0);
	cgui_grid_set_row_flex(grid, 1, 1.0);
	cgui_grid_set_row_flex(grid, 2, 1.0);
	cgui_grid_set_row_flex(grid, 3, 1.0);

	cgui_grid_assign_cell(grid, gauge_1,  0, 0, 3, 1);
	cgui_grid_assign_cell(grid, gauge_2,  0, 1, 3, 1);
	cgui_grid_assign_cell(grid, gauge_3,  0, 2, 3, 1);
	cgui_grid_assign_cell(grid, gauge_4,  0, 3, 1, 1);
	cgui_grid_assign_cell(grid, button_d, 1, 3, 1, 1);
	cgui_grid_assign_cell(grid, button_i, 2, 3, 1, 1);
	cgui_grid_assign_cell(grid, stripes,  3, 0, 1, 4);

	/* Window setup */

	cgui_window_push_grid(window, grid);
	cgui_window_rename(window, "gauges");
	cgui_window_activate(window);

	/* Run */
	
	cgui_run();

	/* End */

	if (cgui_error())
	{
		printf("Gui has failed during operation.\n");
	}

	cgui_window_destroy(window);
	cgui_grid_destroy(grid);
	cgui_cell_destroy(gauge_1);
	cgui_cell_destroy(gauge_2);
	cgui_cell_destroy(gauge_3);
	cgui_cell_destroy(gauge_4);
	cgui_cell_destroy(button_i);
	cgui_cell_destroy(button_d);
	cgui_cell_destroy(stripes);

	cgui_reset();

	return 0;
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
on_click(cgui_cell *c)
{
	val += STEP * (c == button_i ? 1 : -1);
	val  = val < MIN ? MIN : (val > MAX ? MAX : val);

	cgui_gauge_set_value(gauge_4, val);
}

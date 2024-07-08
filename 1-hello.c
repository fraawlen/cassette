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

#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include <cassette/cgui.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/**
 * Standard hello world example.
 */

 int
 main(int argc, char **argv)
 {
 	cgui_grid_t   *grid;
	cgui_cell_t   *cell;
	cgui_window_t *window;

	const char *str = "Hello World!";
	bool fail = false;

	/* setup */

	cgui_init(argc, argv);

	/* object instantiation */

	grid   = cgui_grid_create(1, 1);
	cell   = cgui_cell_create();
	window = cgui_window_create();

	/* cell setup */

	cgui_cell_enable(cell);

	/* grid setup */

	cgui_grid_set_col_width(grid, 0, strlen(str));
	cgui_grid_set_col_flex(grid, 0, 1.0);
	cgui_grid_set_row_flex(grid, 0, 1.0);
	cgui_grid_assign_cell(grid, cell, 0, 0, 1, 1);

	/* window setup */

	/* run */

	cgui_run();

	/* end */

	fail |= cgui_has_failed();
	fail |= cgui_window_has_failed(window);
	fail |= cgui_grid_has_failed(grid);
	fail |= cgui_cell_has_failed(cell);

	if (fail)
	{
		printf("gui has failed during operation.\n");
	}

	cgui_window_destroy(&window);
	cgui_grid_destroy(&grid);
	cgui_cell_destroy(&cell);

	cgui_reset();

	return 0;
 }

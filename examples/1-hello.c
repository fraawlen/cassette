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

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define MSG "Hello World!"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static cgui_cell   *_cell   = CGUI_CELL_PLACEHOLDER;
static cgui_grid   *_grid   = CGUI_GRID_PLACEHOLDER;
static cgui_window *_window = CGUI_WINDOW_PLACEHOLDER;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/**
 * Standard hello world example.
 */

 int
 main(int argc, char **argv)
 {
	/* Setup */

	cgui_init(argc, argv);

	_grid   = cgui_grid_create(1, 1);
	_cell   = cgui_cell_create();
	_window = cgui_window_create();

	/* Cell setup */

	/* Grid setup */

	cgui_grid_resize_col(_grid, 0, strlen(MSG));
	cgui_grid_set_col_flex(_grid, 0, 1.0);
	cgui_grid_set_row_flex(_grid, 0, 1.0);
	cgui_grid_assign_cell(_grid, _cell, 0, 0, 1, 1);

	/* Window setup */

	/* Run */

	cgui_run();

	/* End */

	if (cgui_error()
	 || cgui_window_error(_window)
	 || cgui_grid_error(_grid)
	 || cgui_cell_error(_cell))
	{
		printf("Gui has failed during operation.\n");
	}

	cgui_window_destroy(_window);
	cgui_grid_destroy(_grid);
	cgui_cell_destroy(_cell);

	cgui_reset();

	return 0;
 }

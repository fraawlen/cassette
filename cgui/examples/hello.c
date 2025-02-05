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

#define MSG "Hello World!"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static cgui_cell   *label  = CGUI_CELL_PLACEHOLDER;
static cgui_grid   *grid   = CGUI_GRID_PLACEHOLDER;
static cgui_window *window = CGUI_WINDOW_PLACEHOLDER;

/************************************************************************************************************/
/* MAIN *****************************************************************************************************/
/************************************************************************************************************/

/**
 * Standard hello world example.
 * As per the WGC UI model, grid dimensions are specifed in number of glyphs.
 */

 int
 main(int argc, char **argv)
 {
	/* Instantiation */

	cgui_init(argc, argv);

	window = cgui_window_create();
	grid   = cgui_grid_create(1, 1);
	label  = cgui_label_create();

	/* Cell setup */

	cgui_label_set(label, MSG);

	/* Grid setup */

	cgui_grid_resize_col(grid, 0,  strlen(MSG));
	cgui_grid_resize_row(grid, 0,  1);

	cgui_grid_set_col_flex(grid, 0, 1.0);
	cgui_grid_set_row_flex(grid, 0, 1.0);

	cgui_grid_assign_cell(grid, label, 0, 0, 1, 1);
	
	/* Window setup */

	cgui_window_push_grid(window, grid);
	cgui_window_rename(window, "Hi");
	cgui_window_activate(window);

	/* Run */

	cgui_run();

	/* End & cleanup */

	if (cgui_error())
	{
		printf("Gui has failed during operation (%i).\n", cgui_error());
	}

	cgui_window_destroy(window);
	cgui_grid_destroy(grid);
	cgui_cell_destroy(label);

	cgui_reset();

	return 0;
 }

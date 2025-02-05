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

static cgui_window *window    = CGUI_WINDOW_PLACEHOLDER;
static cgui_grid   *grid      = CGUI_GRID_PLACEHOLDER;
static cgui_cell   *cells[11];

/************************************************************************************************************/
/* MAIN *****************************************************************************************************/
/************************************************************************************************************/

/**
 * A bunch of different cells spread out.
 */

 int
 main(int argc, char **argv)
 {
	/* Instantiation */

	cgui_init(argc, argv);

	window = cgui_window_create();
	grid   = cgui_grid_create(4, 6);

	cells[ 0] = cgui_stripes_create();
	cells[ 1] = cgui_placeholder_create();
	cells[ 2] = cgui_filler_create();
	cells[ 3] = cgui_label_create();
	cells[ 4] = cgui_beacon_create();
	cells[ 5] = cgui_beacon_create();
	cells[ 6] = cgui_beacon_create();
	cells[ 7] = cgui_button_create();
	cells[ 8] = cgui_button_create();
	cells[ 9] = cgui_gauge_create();
	cells[10] = cgui_gauge_create();
	
	/* Cell setup */

	cgui_label_set(cells[3], "Vertical text");
	cgui_label_rotate(cells[3], CGUI_ROTATION_RIGHT);

	cgui_beacon_set_label(cells[4], "Beacon - CRITICAL");
	cgui_beacon_set_label(cells[5], "Beacon - ON");
	cgui_beacon_set_label(cells[6], "Beacon - OFF");

	cgui_beacon_align_label(cells[4], CGUI_ALIGN_LEFT);
	cgui_beacon_align_label(cells[5], CGUI_ALIGN_LEFT);
	cgui_beacon_align_label(cells[6], CGUI_ALIGN_LEFT);

	cgui_beacon_set_state(cells[4], CGUI_BEACON_CRITICAL);
	cgui_beacon_set_state(cells[5], CGUI_BEACON_ON);
	cgui_beacon_set_state(cells[6], CGUI_BEACON_OFF);

	cgui_button_set_label(cells[7], "Button");
	cgui_button_set_label(cells[8], "Disabled button");
	cgui_button_align_label(cells[7], CGUI_ALIGN_RIGHT);
	cgui_button_disable(cells[8]);	

	cgui_gauge_set_value(cells[ 9], 82);
	cgui_gauge_set_value(cells[10], 0.32);
	cgui_gauge_clamp_value(cells[10], 0.0, 1.0);
	cgui_gauge_set_precision(cells[10], 2);
	cgui_gauge_set_units(cells[10], "");
	cgui_gauge_rotate(cells[10], CGUI_ROTATION_INVERTED);

	/* Grid setup */

	cgui_grid_resize_col(grid, 1, 17);
	cgui_grid_resize_col(grid, 2, 10);

	cgui_grid_set_col_flex(grid, 2, 1.0);
	cgui_grid_set_row_flex(grid, 0, 1.0);

	cgui_grid_assign_cell(grid, cells[ 0], 3, 0, 1, 6);
	cgui_grid_assign_cell(grid, cells[ 1], 2, 0, 1, 4);
	cgui_grid_assign_cell(grid, cells[ 2], 1, 0, 1, 1);
	cgui_grid_assign_cell(grid, cells[ 3], 0, 0, 1, 6);
	cgui_grid_assign_cell(grid, cells[ 4], 1, 1, 1, 1);
	cgui_grid_assign_cell(grid, cells[ 5], 1, 2, 1, 1);
	cgui_grid_assign_cell(grid, cells[ 6], 1, 3, 1, 1);
	cgui_grid_assign_cell(grid, cells[ 7], 1, 4, 1, 1);
	cgui_grid_assign_cell(grid, cells[ 8], 1, 5, 1, 1);
	cgui_grid_assign_cell(grid, cells[ 9], 2, 4, 1, 1);
	cgui_grid_assign_cell(grid, cells[10], 2, 5, 1, 1);

	/* Window setup */

	cgui_window_push_grid(window, grid);
	cgui_window_rename(window, "Showcase");
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
	for (int i = 0; i < 11; i++)
	{
		cgui_cell_destroy(cells[i]);
	}

	cgui_reset();

	return 0;
 }

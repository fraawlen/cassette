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
#include <cassette/ccfg.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define MAX_DIST 200.0

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void on_draw (cgui_window *, unsigned long, unsigned long);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static cgui_cell   *filler   = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *stripes  = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *button_1 = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *button_2 = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *button_3 = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *button_4 = CGUI_CELL_PLACEHOLDER;
static cgui_grid   *grid     = CGUI_GRID_PLACEHOLDER;
static cgui_window *window   = CGUI_WINDOW_PLACEHOLDER;

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

	cgui_setup_app_name("shadow_test");
	cgui_init(argc, argv);

	window   = cgui_window_create();
	grid     = cgui_grid_create(3, 3);
	filler   = cgui_filler_create();
	stripes  = cgui_stripes_create();
	button_1 = cgui_button_create();
	button_2 = cgui_button_create();
	button_3 = cgui_button_create();
	button_4 = cgui_button_create();

	/* Cell setup */

	cgui_button_disable(button_4);

	/* Grid 1 setup */

	cgui_grid_assign_cell(grid, stripes,  1, 1, 1, 1);
	cgui_grid_assign_cell(grid, filler,   2, 0, 1, 2);
	cgui_grid_assign_cell(grid, filler,   0, 2, 2, 1);
	cgui_grid_assign_cell(grid, button_1, 0, 0, 1, 1);
	cgui_grid_assign_cell(grid, button_2, 1, 0, 1, 1);
	cgui_grid_assign_cell(grid, button_3, 0, 1, 1, 1);
	cgui_grid_assign_cell(grid, button_4, 2, 2, 1, 1);
	
	/* Window setup */

	cgui_window_push_grid(window, grid);
	cgui_window_rename(window, "shadows");
	cgui_window_on_draw(window, on_draw);
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
	cgui_cell_destroy(filler);
	cgui_cell_destroy(stripes);
	cgui_cell_destroy(button_1);
	cgui_cell_destroy(button_2);
	cgui_cell_destroy(button_3);
	cgui_cell_destroy(button_4);

	cgui_reset();

	return 0;
 }

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
on_draw(cgui_window *w, unsigned long delay_1, unsigned long delay_2)
{
	double x;
	double y;
	double d;
	double a;
	double m;
	double r;

	(void)delay_1;
	(void)delay_2;

	/* pointer position relative to window's center */

	cgui_screen_pointer_position(&x, &y);

	x -= cgui_window_x(w) + cgui_window_width(w)  / 2;
	y -= cgui_window_y(w) + cgui_window_height(w) / 2;
	
	/* calculate shadow offset ratios */

	m = cgui_window_width(w) / 2 + MAX_DIST;
	a = atan(y / x) + (x > 0.0 ? 3.14159 : 0.0);
	d = sqrt(pow(x, 2) + pow(y, 2));
	r = d > m ? 1.0 : d / m;

	/* apply offset ratios */

	ccfg_push_param(cgui_config_get_parser(), "shadow_x", r * cos(a));
	ccfg_push_param(cgui_config_get_parser(), "shadow_y", r * sin(a));
	cgui_reconfig();
}

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
#include <cassette/cobj.h>
#include <string.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define WIDTH 24
#define PAD   "."

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void on_click    (cgui_cell *);
static void update_info (void);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define BOOL(B) (B ? "ON" : "OFF")
#define CONFIG cgui_config_get()
#define COMPOSE(I, NAME, VAL) \
	cstr_clear(str); \
	cstr_append(str, NAME); \
	cstr_append(str, VAL); \
	cstr_pad(str, PAD, strlen(NAME), WIDTH); \
	cgui_label_set(info[I], cstr_chars(str));

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static cstr        *str     = CSTR_PLACEHOLDER;
static cgui_window *window  = CGUI_WINDOW_PLACEHOLDER;
static cgui_grid   *grid    = CGUI_GRID_PLACEHOLDER;
static cgui_cell   *reload  = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *quit    = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *info[8];

static struct cgui_screen screen; 

/************************************************************************************************************/
/* MAIN *****************************************************************************************************/
/************************************************************************************************************/

/**
 * This example shows how to make a fixed window, with a position relative to the primary monitor. It also
 * shows how to get the configuration details and how to broadcast a reconfiguration signal to all CGUI
 * clients.
 */

 int
 main(int argc, char **argv)
 {
	/* Instantiation */

	cgui_init(argc, argv);

	str    = cstr_create();
	screen = cgui_screen_primary_specs();
	window = cgui_window_create();
	grid   = cgui_grid_create(1, 10);
	reload = cgui_button_create();
	quit   = cgui_button_create();
	for (int i = 0; i < 8; i++)
	{
		info[i] = cgui_label_create();
	}

	cstr_set_precision(str, 2);

	/* Cell setup */

	cgui_button_set_label(reload, "Broadcast reconfig");
	cgui_button_align_label(reload, CGUI_ALIGN_LEFT);
	cgui_button_on_click(reload, on_click);

	cgui_button_set_label(quit, "Quit");
	cgui_button_align_label(quit, CGUI_ALIGN_LEFT);
	cgui_button_on_click(quit, on_click);

	update_info();

	/* Grid setup */

	cgui_grid_resize_col(grid, 0,  WIDTH);

	cgui_grid_assign_cell(grid, info[0], 0, 0, 1, 1);
	cgui_grid_assign_cell(grid, info[1], 0, 1, 1, 1);
	cgui_grid_assign_cell(grid, info[2], 0, 2, 1, 1);
	cgui_grid_assign_cell(grid, info[3], 0, 3, 1, 1);
	cgui_grid_assign_cell(grid, info[4], 0, 4, 1, 1);
	cgui_grid_assign_cell(grid, info[5], 0, 5, 1, 1);
	cgui_grid_assign_cell(grid, info[6], 0, 6, 1, 1);
	cgui_grid_assign_cell(grid, info[7], 0, 7, 1, 1);
	cgui_grid_assign_cell(grid, reload,  0, 8, 1, 1);
	cgui_grid_assign_cell(grid, quit,    0, 9, 1, 1);
	
	/* Window setup */

	cgui_window_push_grid(window, grid);
	cgui_window_set_type(window, CGUI_WINDOW_OVERLAY);
	cgui_window_move(window, screen.x + 20, screen.y + 20);
	cgui_window_activate(window);

	/* Run */

	cgui_config_on_load(update_info);
	cgui_run();

	/* End & cleanup */

	if (cgui_error())
	{
		printf("Gui has failed during operation (%i).\n", cgui_error());
	}

	cstr_destroy(str);
	cgui_window_destroy(window);
	cgui_grid_destroy(grid);
	cgui_cell_destroy(reload);
	cgui_cell_destroy(quit);
	for (int i = 0; i < 8; i++)
	{
		cgui_cell_destroy(info[i]);
	}

	cgui_reset();

	return 0;
 }

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void
on_click(cgui_cell *c)
{
	if (c == reload)
	{
		cgui_broadcast_reconfig();
	}
	else
	{
		cgui_exit();
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
update_info(void)
{
	COMPOSE(0, "Mode",             CONFIG->render_mode == CGUI_RENDER_FORWARD ? "FORWARD" : "DEFERRED")
	COMPOSE(1, "Sync vblank",      BOOL(CONFIG->render_sync_vblank))
	COMPOSE(2, "Sync bypass",      BOOL(CONFIG->render_sync_bypass))
	COMPOSE(3, "Partal redraw",    BOOL(CONFIG->render_partial))
	COMPOSE(4, "Focus overlaps",   BOOL(CONFIG->render_overlap))
	COMPOSE(5, "Scale",            CONFIG->render_scale)
	COMPOSE(6, "FPS async cap",    CONFIG->render_fps_async_cap)
	COMPOSE(7, "FPS sync divider", CONFIG->render_fps_sync_div)
}

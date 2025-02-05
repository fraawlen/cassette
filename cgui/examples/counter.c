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
#include <pthread.h>
#include <stdlib.h>
#include <unistd.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void *increment    (void *);
static void  on_click     (cgui_cell *);
static void  on_exit      (void);
static void  on_run       (void);
static void  update_label (void);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static cstr        *str      = CSTR_PLACEHOLDER;
static cgui_cell   *label    = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *button_1 = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *button_2 = CGUI_CELL_PLACEHOLDER;
static cgui_grid   *grid     = CGUI_GRID_PLACEHOLDER;
static cgui_window *window   = CGUI_WINDOW_PLACEHOLDER;

static long count = 0;
static pthread_t thread;

/************************************************************************************************************/
/* MAIN *****************************************************************************************************/
/************************************************************************************************************/

/**
 * Basic seconds counter using multithreading. cgui_init(), cgui_run() and cgui_reset() should be on the
 * same thread. The counter thread is created and joined from a callback that gets called when CGUI enters
 * and exits its main event loop. The counter thread is created inside these callbacks to simplify its CGUI
 * state checking. cgui_lock() and cgui_unlock() help safely access CGUI methods from different threads.
 * sleep is called outside lock and unlock to not keep the main CGUI thread waiting.
 */

int
main(int argc, char **argv)
{
	/* Instantiation */

	cgui_init(argc, argv);

	str      = cstr_create();
	window   = cgui_window_create();
	grid     = cgui_grid_create(2, 2);
	label    = cgui_label_create();
	button_1 = cgui_button_create();
	button_2 = cgui_button_create();

	/* Cell setup */

	update_label();
	cgui_label_align(label, CGUI_ALIGN_RIGHT);

	cgui_button_set_label(button_1, "Reset");
	cgui_button_set_label(button_2, "Quit");
	cgui_button_on_click(button_1, on_click);
	cgui_button_on_click(button_2, on_click);

	/* Grid setup */

	cgui_grid_resize_col(grid, 0, 5);
	cgui_grid_resize_col(grid, 1, 5);

	cgui_grid_assign_cell(grid, label,    0, 0, 2, 1);
	cgui_grid_assign_cell(grid, button_1, 0, 1, 1, 1);
	cgui_grid_assign_cell(grid, button_2, 1, 1, 1, 1);

	/* Window setup */

	cgui_window_push_grid(window, grid);
	cgui_window_rename(window, "counter");
	cgui_window_activate(window);

	/* Run */
	
	cgui_on_run(on_run);
	cgui_on_exit(on_exit);

	cgui_run();

	/* End */

	if (cgui_error())
	{
		printf("Gui has failed during operation.\n");
	}

	cgui_window_destroy(window);
	cgui_grid_destroy(grid);
	cgui_cell_destroy(label);
	cgui_cell_destroy(button_1);
	cgui_cell_destroy(button_2);
	cstr_destroy(str);

	cgui_reset();

	return 0;
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void *
increment(void *params)
{
	bool run = true;

	(void)params;

	while (run)
	{
		sleep(1);
		cgui_lock();

		count++;
		update_label();
		run = cgui_is_running();

		cgui_unlock();
	}

	pthread_exit(NULL);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
on_click(cgui_cell *c)
{
	if (c == button_1)
	{
		count = 0;
		update_label();
	}
	else
	{
		cgui_window_deactivate(window);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
on_exit(void)
{
	pthread_join(thread, NULL);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
on_run(void)
{
	pthread_create(&thread, NULL, increment, NULL);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
update_label(void)
{
	cstr_clear(str);
	cstr_append(str, count);
	cgui_label_set(label, cstr_chars(str));
}


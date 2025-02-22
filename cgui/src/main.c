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

#include <cairo/cairo.h>
#include <cassette/cgui.h>
#include <cassette/cobj.h>
#include <errno.h>
#include <fontconfig/fontconfig.h>
#include <pthread.h>
#include <xcb/xcb.h>
#include <stdbool.h>
#include <stdlib.h>

#include "cell.h"
#include "config.h"
#include "env.h"
#include "event.h"
#include "grid.h"
#include "main.h"
#include "screen.h"
#include "window.h"
#include "util.h"
#include "x11.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define SET_ERR(ERR) if (err == CERR_NONE) { err = ERR; }
#define WINDOW(I)    ((cgui_window *)cref_ptr(windows, I))
#define GRID(I)      ((cgui_grid   *)cref_ptr(grids,   I))
#define CELL(I)      ((cgui_cell   *)cref_ptr(cells,   I))

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void dummy_fn_exit           (void);
static void dummy_fn_run            (void);
static bool is_any_window_activated (void) CGUI_PURE;
static void mutex_init              (void);
static void mutex_reset             (void);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/* pre-init args */

static xcb_connection_t *ext_connection = NULL;
static const char       *app_name       = NULL;
static const char       *app_class      = NULL;

/* objects trackers */

static cref *cells   = CREF_PLACEHOLDER;
static cref *grids   = CREF_PLACEHOLDER;
static cref *windows = CREF_PLACEHOLDER;

/* session states */

static bool running  = false;
static bool usr_exit = true;
static enum cerr err = CERR_INVALID;

/* callbacks */

static void (*fn_run)  (void) = dummy_fn_run;
static void (*fn_exit) (void) = dummy_fn_exit;

/* misc */

static pthread_mutex_t mutex;

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cgui_allow_user_exit(void)
{
	if (err)
	{
		return;
	}

	usr_exit = true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_block_user_exit(void)
{
	if (err)
	{
		return;
	}

	usr_exit = false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_broadcast_reconfig(void)
{
	if (err)
	{
		return;
	}

	x11_broadcast_reconfig();
}	

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cerr
cgui_error(void)
{
	return err;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_exit(void)
{
	if (err)
	{
		return;
	}

	running = false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_init(int argc, char **argv)
{
	if (err != CERR_INVALID)
	{
		return;
	}

	mutex_init();
	main_lock();

	if (!app_class)
	{
		app_class = argc > 0 && argv ? argv[0] : "cgui";
	}

	if (!app_name)
	{
		app_name = app_class;
	}

	cells   = cref_create();
	grids   = cref_create();
	windows = cref_create();
	err     = CERR_NONE;

	cref_set_default_ptr(cells,   CGUI_CELL_PLACEHOLDER);
	cref_set_default_ptr(grids,   CGUI_GRID_PLACEHOLDER);
	cref_set_default_ptr(windows, CGUI_WINDOW_PLACEHOLDER);

	SET_ERR(cref_error(cells));
	SET_ERR(cref_error(grids));
	SET_ERR(cref_error(windows));

	config_init(app_name, app_class);
	config_load();
	x11_init(argc, argv, app_name, app_class, ext_connection);
	if (!err)
	{
		screen_pointer_update();
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgui_is_init(void)
{
	return err == CERR_NONE;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgui_is_running(void)
{
	return running;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_lock(void)
{
	if (err)
	{
		return;
	}

	main_lock();
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_on_exit(void (*fn)(void))
{
	if (err)
	{
		return;
	}

	fn_exit = fn ? fn : dummy_fn_exit;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_on_run(void (*fn)(void))
{
	if (err)
	{
		return;
	}

	fn_run = fn ? fn : dummy_fn_run;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_reconfig(void)
{
	if (err)
	{
		return;
	}

	config_load();
	x11_update_pointer_tracking();

	CREF_FOR_EACH(windows, i)
	{
		if (!WINDOW(i)->valid)
		{
			return;
		}

		cgui_window_resize(WINDOW(i), WINDOW(i)->width, WINDOW(i)->height);
		cgui_window_redraw_async(WINDOW(i));
		window_update_size_hints(WINDOW(i));
		window_update_shown_grid(WINDOW(i));
		window_update_geometry(WINDOW(i));
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_repair(void)
{
	if (err == CERR_INVALID)
	{
		return;
	}

	cref_repair(cells);
	cref_repair(grids);
	cref_repair(windows);
	config_repair();

	err = CERR_NONE;

	CREF_FOR_EACH(windows, i)
	{
		window_repair(WINDOW(i));
	}

	CREF_FOR_EACH(grids, i)
	{
		grid_repair(GRID(i));
	}

	CREF_FOR_EACH(cells, i)
	{
		cell_repair(CELL(i));
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_reset(void)
{
	CREF_FOR_EACH(windows, i)
	{
		WINDOW(i)->valid = false;
	}

	CREF_FOR_EACH(grids, i)
	{
		GRID(i)->valid = false;
	}

	CREF_FOR_EACH(cells, i)
	{
		CELL(i)->valid = false;
	}

	for (int i = 1; i <= CGUI_CLIPBOARDS; i++)
	{
		cgui_clipboard_clear(i);
	}

	if (!ext_connection || util_env_exists(ENV_FORCE_CLEAN))
	{
		cairo_debug_reset_static_data();
		FcFini();
	}

	cref_destroy(cells);
	cref_destroy(grids);
	cref_destroy(windows);
	x11_reset(!ext_connection);
	config_reset();

	cells          = CREF_PLACEHOLDER;
	grids          = CREF_PLACEHOLDER;
	windows        = CREF_PLACEHOLDER;
	ext_connection = NULL;
	app_class      = NULL;
	app_name       = NULL;
	usr_exit       = true;
	running        = false;	
	fn_run         = dummy_fn_run;
	fn_exit        = dummy_fn_exit;
	err            = CERR_INVALID;

	main_unlock();
	mutex_reset();
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_run(void)
{
	if (err || running)
	{
		return;
	}

	running = true;

	main_unlock();
	fn_run();
	main_lock();

	while (!err && running && is_any_window_activated())
	{
		 x11_update();
	}

	running = false;

	main_unlock();
	fn_exit();
	main_lock();
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_setup_app_class(const char *class_name)
{
	if (err != CERR_INVALID)
	{
		return;
	}

	app_class = class_name;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_setup_app_name(const char *name)
{
	if (err != CERR_INVALID)
	{
		return;
	}

	app_name = name;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_setup_x11_connection(xcb_connection_t *connection)
{
	if (err != CERR_INVALID)
	{
		return;
	}

	ext_connection = connection;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

xcb_connection_t *
cgui_x11_connection(void)
{
	return x11_connection();
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

xcb_window_t
cgui_x11_leader_window(void)
{
	return  x11_leader_window();
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

xcb_screen_t *
cgui_x11_screen(void)
{
	return x11_root_screen();
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_unlock(void)
{
	struct cgui_event event =
	{
		.window = CGUI_WINDOW_PLACEHOLDER,
		.type   = CGUI_EVENT_NONE,
	};

	if (err)
	{
		return;
	}

	main_update(&event);
	main_unlock();
}

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

cref *
main_cells(void)
{
	return cells;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cref *
main_grids(void)
{
	return grids;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
main_lock(void)
{
	pthread_mutex_lock(&mutex);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
main_pull_instance(cref *ref, void *ptr)
{
	 cref_pull(ref, ptr);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
main_push_instance(cref *ref, void *ptr)
{
	cref_push(ref, ptr);

	return cref_error(ref) == CERR_NONE;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
main_set_error(enum cerr error)
{
	SET_ERR(error);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cref *
main_windows(void)
{
	return windows;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
main_unlock(void)
{
	pthread_mutex_unlock(&mutex);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
main_update(struct cgui_event *event)
{
	event_process(event);

	CREF_FOR_EACH_REV(windows, i)
	{
		window_present(WINDOW(i));
		window_destroy(WINDOW(i));
	}

	CREF_FOR_EACH_REV(grids, i)
	{
		grid_destroy(GRID(i));
	}

	CREF_FOR_EACH_REV(cells, i)
	{
		cell_destroy(CELL(i));
	}
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
dummy_fn_exit(void)
{
	/* nothing */
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
dummy_fn_run(void)
{
	/* nothing */
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
is_any_window_activated(void)
{
	CREF_FOR_EACH(windows, i)
	{
		if (WINDOW(i)->state.active)
		{
			return true;
		}
	}

	return false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
mutex_init(void)
{
	pthread_mutexattr_t mut_attr;

	if (pthread_mutexattr_init(&mut_attr) != 0
	 || pthread_mutexattr_settype(&mut_attr, PTHREAD_MUTEX_RECURSIVE) != 0)
	{
		goto fail_attrib;
	}
	
	if (pthread_mutex_init(&mutex, &mut_attr))
	{
		goto fail_mutex;
	}

	pthread_mutexattr_destroy(&mut_attr);

	return;

fail_mutex:
	pthread_mutex_destroy(&mutex);
fail_attrib:
	pthread_mutexattr_destroy(&mut_attr);
	SET_ERR(CERR_MUTEX);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
mutex_reset(void)
{
	pthread_mutex_destroy(&mutex);
}

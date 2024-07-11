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

#include <assert.h>

#include <cairo/cairo.h>
#include <cassette/cgui.h>
#include <cassette/cobj.h>
#include <fontconfig/fontconfig.h>
#include <xcb/xcb.h>
#include <stdbool.h>
#include <stdlib.h>

#include "cell.h"
#include "config.h"
#include "env.h"
#include "event.h"
#include "grid.h"
#include "main.h"
#include "mutex.h"
#include "window.h"
#include "util.h"
#include "x11.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static bool _is_any_window_activated (void);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/* objects trackers */

static cref *_cells   = CREF_PLACEHOLDER;
static cref *_grids   = CREF_PLACEHOLDER;
static cref *_windows = CREF_PLACEHOLDER;

/* x11 init args */

static xcb_connection_t *_ext_connection = NULL;

static const char *_app_name  = NULL;
static const char *_app_class = NULL;

/* session states */

static bool _running  = false;
static bool _usr_exit = true;
static enum cerr _err = CERR_NOT_INIT;

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cgui_allow_user_exit(void)
{
	if (_err)
	{
		return;
	}

	_usr_exit = true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_block_user_exit(void)
{
	if (_err)
	{
		return;
	}

	_usr_exit = false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cerr
cgui_error(void)
{
	return _err;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_exit(void)
{
	if (_err)
	{
		return;
	}

	_running = false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_init(int argc, char **argv)
{
	if (_err != CERR_NOT_INIT)
	{
		return;
	}

	if (!_app_class)
	{
		_app_class = argv ? argv[0] : "cgui";
	}

	if (!_app_name)
	{
		_app_name = _app_class;
	}

	_cells   = cref_create();
	_grids   = cref_create();
	_windows = cref_create();

	cref_set_default_ptr(_cells,   CGUI_CELL_PLACEHOLDER);
	cref_set_default_ptr(_grids,   CGUI_GRID_PLACEHOLDER);
	cref_set_default_ptr(_windows, CGUI_WINDOW_PLACEHOLDER);

	_err  = CERR_NONE;
	_err |= cref_error(_cells);
	_err |= cref_error(_grids);
	_err |= cref_error(_windows);
	_err |= x11_init(argc, argv, _app_name, _app_class, _ext_connection);
	_err |= config_init(_app_name, _app_class);
	_err |= config_load();
	_err |= mutex_init();

	if (_err)
	{
		_err |= CERR_NOT_INIT;
	}

	mutex_lock();
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgui_is_init(void)
{
	return !(_err & CERR_NOT_INIT);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgui_is_running(void)
{
	return _running;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgui_lock(void)
{
	if (_err)
	{
		return false;
	}

	return mutex_lock();
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_reconfig(void)
{
	cgui_window *window;
	cgui_grid *grid;
	cgui_grid *grid_min;

	if ((_err |= config_load()))
	{
		return;
	}

	CREF_FOR_EACH(_windows, i)
	{
		window = (cgui_window*)cref_ptr(_windows, i);
		if (window->err)
		{
			continue;
		}

		(void)grid;
		(void)grid_min;

		// TODO
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_repair(void)
{
	cref_repair(_cells);
	cref_repair(_grids);
	cref_repair(_windows);
	config_repair();

	_err &= CERR_NOT_INIT;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_reset(void)
{
	CREF_FOR_EACH(_windows, i)
	{
		((cgui_window*)cref_ptr(_windows, i))->err |= CERR_INVALID;
	}

	CREF_FOR_EACH(_grids, i)
	{
		((cgui_grid*)cref_ptr(_grids, i))->err |= CERR_INVALID;
	}

	CREF_FOR_EACH(_cells, i)
	{
		((cgui_cell*)cref_ptr(_cells, i))->err |= CERR_INVALID;
	}

	if (!_ext_connection || util_env_exists(ENV_FORCE_CLEAN))
	{
		cairo_debug_reset_static_data();
		FcFini();
	}

	x11_reset(!_ext_connection);
	config_reset();

	cref_destroy(_cells);
	cref_destroy(_grids);
	cref_destroy(_windows);

	_cells          = CREF_PLACEHOLDER;
	_grids          = CREF_PLACEHOLDER;
	_windows        = CREF_PLACEHOLDER;
	_ext_connection = NULL;
	_app_class      = NULL;
	_app_name       = NULL;
	_usr_exit       = true;
	_running        = false;	
	_err            = CERR_NOT_INIT;

	mutex_unlock();
	mutex_reset();
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_run(void)
{
	if (_running)
	{
		return;
	}

	_running = true;

	while (!_err && _running && _is_any_window_activated())
	{
		_err |= x11_update();
	}

	_running = false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_setup_app_class(const char *class_name)
{
	if (_err != CERR_NOT_INIT)
	{
		return;
	}

	_app_class = class_name;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_setup_app_name(const char *name)
{
	if (_err != CERR_NOT_INIT)
	{
		return;
	}

	_app_name = name;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_setup_x11_connection(xcb_connection_t *connection)
{
	if (_err != CERR_NOT_INIT)
	{
		return;
	}

	_ext_connection = connection;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgui_unlock(void)
{
	if (_err)
	{
		return false;
	}

	return mutex_unlock();
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

xcb_connection_t *
cgui_x11_connection(void)
{
	return x11_get_connection();
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

xcb_window_t
cgui_x11_leader_window(void)
{
	return  x11_get_leader_window();
}

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

cref *
main_cells(void)
{
	return _cells;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cref *
main_grids(void)
{
	return _grids;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
main_push_instance(cref *ref, void *ptr)
{
	cref_push(ref, ptr);

	_err |= cref_error(ref);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cref *
main_windows(void)
{
	return _windows;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
main_update(struct cgui_event *event)
{
	cgui_lock();

	event_process(event);

	CREF_FOR_EACH_REV(_windows, i)
	{
		window_present((cgui_window*)cref_ptr(_windows, i));
		window_destroy((cgui_window*)cref_ptr(_windows, i));
	}

	CREF_FOR_EACH_REV(_grids, i)
	{
		grid_destroy((cgui_grid*)cref_ptr(_grids, i));
	}

	CREF_FOR_EACH_REV(_cells, i)
	{
		cell_destroy((cgui_cell*)cref_ptr(_cells, i));
	}
	
	cgui_unlock();
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static bool
_is_any_window_activated(void)
{
	CREF_FOR_EACH(_windows, i)
	{
		if (((cgui_window*)cref_ptr(_windows, i))->state & CGUI_WINDOW_ACTIVE)
		{
			return true;
		}
	}

	return false;
}

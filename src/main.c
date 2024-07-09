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

static bool _init     = false;
static bool _running  = false;
static bool _usr_exit = true;
static bool _failed   = true;

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cgui_allow_user_exit(void)
{
	assert(_init);

	_usr_exit = true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_block_user_exit(void)
{
	assert(_init);

	_usr_exit = false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cgui_err
cgui_error(void)
{
	if (_init)
	{
		_failed |= cref_error(_cells);
		_failed |= cref_error(_grids);
		_failed |= cref_error(_windows);
		_failed |= x11_has_failed();
	}

	return _failed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_exit(void)
{
	assert(_init);

	_running = false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_init(int argc, char **argv)
{
	assert(!_init);

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

	_failed  = false;
	_failed |= !x11_init(argc, argv, _app_name, _app_class, _ext_connection);
	_failed |= !config_init(_app_name, _app_class);
	_failed |= !config_load();
	_failed |= !mutex_init();

	_init = true;

	cgui_lock();
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgui_is_init(void)
{
	return _init;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgui_is_running(void)
{
	return _running;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_lock(void)
{
	assert(_init);

	mutex_lock();
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_reconfig(void)
{
	cgui_window *window;
	cgui_grid *grid;
	cgui_grid *grid_min;

	(void)grid;
	(void)grid_min;

	assert(_init);

	if (_failed)
	{
		return;
	}

	_failed |= !config_load();

	CREF_FOR_EACH(_windows, i)
	{
		window = (cgui_window*)cref_ptr(_windows, i);
		if (window->err)
		{
			continue;
		}

		// TODO
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_reset(void)
{
	assert(_init);

	CREF_FOR_EACH(_windows, i)
	{
		((cgui_window*)cref_ptr(_windows, i))->err |= CGUI_WINDOW_INVALID;
	}

	CREF_FOR_EACH(_grids, i)
	{
		((cgui_grid*)cref_ptr(_grids, i))->err |= CGUI_GRID_INVALID;
	}

	CREF_FOR_EACH(_cells, i)
	{
		((cgui_cell*)cref_ptr(_cells, i))->err |= CGUI_CELL_INVALID;
	}

	if (!_ext_connection || util_env_exists(ENV_FORCE_CLEAN))
	{
		cairo_debug_reset_static_data();
		FcFini();
	}

	x11_reset(!_ext_connection);
	config_reset();
	mutex_reset();

	cref_destroy(_cells);
	cref_destroy(_grids);
	cref_destroy(_windows);

	_cells   = CREF_PLACEHOLDER;
	_grids   = CREF_PLACEHOLDER;
	_windows = CREF_PLACEHOLDER;

	_ext_connection = NULL;
	_app_class      = NULL;
	_app_name       = NULL;
	_usr_exit       = true;
	_failed         = true;
	_running        = false;

	cgui_unlock();
	
	_init = false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_run(void)
{
	bool fail = false;

	assert(_init);

	if (_failed || _running)
	{
		return;
	}

	_running = true;

	while (_init && _running && _is_any_window_activated() && !fail)
	{
		cgui_unlock();

		fail = !x11_update();

		cgui_lock();
	}

	_failed |= fail;
	_running = false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_setup_app_class(const char *class_name)
{
	assert(!_init);

	_app_class = class_name;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_setup_app_name(const char *name)
{
	assert(!_init);

	_app_name = name;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_setup_x11_connection(xcb_connection_t *connection)
{
	assert(!_init);

	_ext_connection = connection;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_unlock(void)
{
	assert(_init);

	mutex_unlock();
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

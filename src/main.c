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
#include <stdbool.h>
#include <stdlib.h>

#include <xcb/xcb.h>
#include <xcb/xcb_keysyms.h>

#include <cassette/cgui.h>

#include "x11.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/* xcb globals */

static xcb_connection_t *_x_con   = NULL;
static xcb_screen_t      *_x_scr  = NULL;
static xcb_depth_t       *_x_dph  = NULL;
static xcb_visualtype_t  *_x_vis  = NULL;
static xcb_key_symbols_t *_x_ksm  = NULL;
static xcb_colormap_t     _x_clm  = 0;
static xcb_window_t       _x_lead = 0;

/* program startup args for ICCCM properties */

static char  const *_class_name  = NULL;
static char  const *_class_class = NULL;
static char *const *_argv = NULL;
static int          _argc = 0;

/* session states */

static bool _ext_con  = false;
static bool _init     = false;
static bool _running  = false;
static bool _usr_exit = true;
static bool _failed   = false;

/* callbacks */

static bool (*_fn_event)  (xcb_generic_event_t *x_ev) = NULL;
static void (*_fn_signal) (uint32_t serial) = NULL;

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cgui_allow_user_exit(void)
{
	_usr_exit = true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_block_user_exit(void)
{
	_usr_exit = false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_exit(void)
{
	assert(_init);

	_running = false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

xcb_connection_t *
cgui_get_xcb_connection(void)
{
	if (_failed)
	{
		return NULL;
	}

	return _x_con;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

xcb_window_t
cgui_get_xcb_leader_window(void)
{
	if (_failed || !_init)
	{
		return 0;
	}

	return _x_lead;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgui_has_failed(void)
{
	return _failed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_init(int argc, char **argv)
{
	assert(!_init);

	(void)(argc);
	(void)(argv);
	
	// TODO

	/* init structs */

	/* setup class and cmd args */

	/* setup x11 main components */

	_x_con = x11_create_connection();
	_x_scr = x11_get_screen(_x_con);
	_x_dph = x11_get_depth(_x_scr);
	_x_vis = x11_get_visual(_x_dph);
	_x_ksm = x11_create_keysym_table(_x_con);
	_x_clm = x11_create_colormap(_x_con, _x_scr, _x_vis);

	/* setup x11_atoms */

}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgui_is_init(void)
{
	return _init;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_reconfig(void)
{
	assert(_init);

	if (_failed)
	{
		return;
	}

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_reset(void)
{
	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_run(void)
{
	assert(_init);

	if (_failed)
	{
		return;
	}

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_send_signal(uint32_t serial)
{
	assert(_init);

	if (_failed)
	{
		return;
	}

	(void)(serial);
	
	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_set_callback_signal(void (*fn)(uint32_t serial))
{
	_fn_signal = fn;	
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_set_callback_xcb_events(bool (*fn)(xcb_generic_event_t *x_ev))
{
	_fn_event = fn;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_setup_x11_class(const char *class_name, const char *class_class)
{
	assert(!_init);

	_class_name  = class_name;
	_class_class = class_class;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_setup_x11_connection(xcb_connection_t *connection)
{
	assert(!_init);

	_x_con   =   connection;
	_ext_con = !!connection;
}


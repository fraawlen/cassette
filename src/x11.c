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
#include <cassette/cobj.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <xcb/xcb.h>
#include <xcb/xcb_keysyms.h>

#include "main.h"
#include "x11.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/* custom atoms names */

#define _ATOM_VERSION           "_CGUI_VERSION"
#define _ATOM_SIGNALS           "_CGUI_SIGNALS"
#define _ATOM_RECONFIG          "_CGUI_RECONFIG"
#define _ATOM_ACCEL             "_CGUI_ACCEL"
#define _ATOM_WINDOW_STATES     "_CGUI_WINDOW_STATE"
#define _ATOM_WINDOW_ACTIVE     "_CGUI_STATE_WIN_ACTIVE"
#define _ATOM_WINDOW_DISABLED   "_CGUI_STATE_WIN_DISABLED"
#define _ATOM_WINDOW_GRID_LOCK  "_CGUI_STATE_GRID_LOCK"
#define _ATOM_WINDOW_FOCUS_LOCK "_CGUI_STATE_FOCUS_LOCK"
#define _ATOM_WINDOW_FOCUS      "_CGUI_WINDOW_FOCUS"
#define _ATOM_PASTE_TMP_1       "_CGUI_PASTE_TMP_1"
#define _ATOM_PASTE_TMP_2       "_CGUI_PASTE_TMP_2"
#define _ATOM_PASTE_TMP_3       "_CGUI_PASTE_TMP_3"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static xcb_atom_t _get_atom             (const char *name);
static uint8_t    _get_extension_opcode (const char *name);
static bool       _prop_append          (xcb_window_t win, xcb_atom_t prop, xcb_atom_t type, uint32_t data_n, const void *data);
static bool       _prop_set             (xcb_window_t win, xcb_atom_t prop, xcb_atom_t type, uint32_t data_n, const void *data);
static bool       _test_cookie          (xcb_void_cookie_t xc);

/* event handlers */

static void _event_unknown (xcb_generic_event_t *xcb_event);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static bool _failed = true;

/* ICCCM properties */

static char  const *_class_name  = NULL;
static char  const *_class_class = NULL;
static char *const *_argv        = NULL;
static int          _argc        = 0;

/* xcb globals */

static xcb_connection_t  *_connection = NULL;
static xcb_screen_t      *_screen     = NULL;
static xcb_depth_t       *_depth      = NULL;
static xcb_visualtype_t  *_visual     = NULL;
static xcb_key_symbols_t *_keysyms    = NULL;
static xcb_colormap_t     _colormap   = 0;
static xcb_window_t       _win_leader = 0;

/* common atoms */

static xcb_atom_t _atom_clip = 0; /* "CLIPBOARD"         */
static xcb_atom_t _atom_time = 0; /* "TIMESTAMP"         */
static xcb_atom_t _atom_mult = 0; /* "MULTIPLE"          */
static xcb_atom_t _atom_trgt = 0; /* "TARGETS"           */
static xcb_atom_t _atom_utf8 = 0; /* "UTF8_STRING"       */
static xcb_atom_t _atom_prot = 0; /* "WM_PROTOCOLS"      */
static xcb_atom_t _atom_del  = 0; /* "WM_DELETE_WINDOW"  */
static xcb_atom_t _atom_foc  = 0; /* "WM_TAKE_FOCUS"     */
static xcb_atom_t _atom_nam  = 0; /* "WM_NAME"           */
static xcb_atom_t _atom_ico  = 0; /* "WM_ICON_MANE"      */
static xcb_atom_t _atom_cls  = 0; /* "WM_CLASS"          */
static xcb_atom_t _atom_cmd  = 0; /* "WM_COMMAND"        */
static xcb_atom_t _atom_host = 0; /* "WM_CLIENT_MACHINE" */
static xcb_atom_t _atom_lead = 0; /* "WM_CLIENT_LEADER"  */
static xcb_atom_t _atom_ping = 0; /* "_NET_WM_PING"      */
static xcb_atom_t _atom_pid  = 0; /* "_NET_WM_PID"       */
static xcb_atom_t _atom_nnam = 0; /* "_NET_WM_NAME"      */
static xcb_atom_t _atom_nico = 0; /* "_NET_WM_ICON_NAME" */

/* CGUI custom atoms */

static xcb_atom_t _atom_sig  = 0; /* _ATOM_SIGNALS           */
static xcb_atom_t _atom_vers = 0; /* _ATOM_VERSION           */
static xcb_atom_t _atom_stt  = 0; /* _ATOM_WINDOW_STATES     */
static xcb_atom_t _atom_dfoc = 0; /* _ATOM_WINDOW_FOCUS      */
static xcb_atom_t _atom_tmp1 = 0; /* _ATOM_PASTE_TMP_1       */
static xcb_atom_t _atom_tmp2 = 0; /* _ATOM_PASTE_TMP_2       */
static xcb_atom_t _atom_tmp3 = 0; /* _ATOM_PASTE_TMP_3       */
static xcb_atom_t _atom_won  = 0; /* _ATOM_WINDOW_ACTIVE     */
static xcb_atom_t _atom_wena = 0; /* _ATOM_WINDOW_DISABLED   */
static xcb_atom_t _atom_plck = 0; /* _ATOM_WINDOW_GRID_LOCK  */
static xcb_atom_t _atom_flck = 0; /* _ATOM_WINDOW_FOCUS_LOCK */
static xcb_atom_t _atom_conf = 0; /* _ATOM_RECONFIG          */
static xcb_atom_t _atom_acl  = 0; /* _ATOM_ACCEL             */

static xcb_atom_t _atom_isig = 0;                       /* "_INTERNAL_LOOP_SIGNAL"          */
static xcb_atom_t _atom_aclx[CGUI_CONFIG_ACCELS] = {0}; /* "_CGUI_WINDOW_ACCEL_x" x = 1..12 */

/* extensions op codes */

static uint8_t _opcode_present = 0;
static uint8_t _opcode_xinput  = 0;

/* events buffer */

static cobj_tracker_t *_events = NULL;

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

xcb_connection_t *
x11_get_connection(void)
{
	if (_failed)
	{
		return NULL;
	}

	return _connection;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

xcb_key_symbols_t *
x11_get_keysyms(void)
{
	if (_failed)
	{
		return NULL;
	}

	return _keysyms;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

xcb_window_t
x11_get_leader_window(void)
{
	if (_failed)
	{
		return 0;
	}

	return _win_leader;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
x11_has_failed(void)
{
	return _failed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
x11_init(int argc, char **argv, const char *class_name, const char *class_class, xcb_connection_t *connection)
{
	xcb_visualtype_iterator_t visual_it;
	xcb_depth_iterator_t depth_it;
	xcb_void_cookie_t xc;

	char   host[256] = "";
	size_t host_n;
	size_t name_n;
	size_t vers_n;
	size_t pid;

	const uint32_t leader_mask_vals[] =
	{
		XCB_EVENT_MASK_SUBSTRUCTURE_NOTIFY   |
		XCB_EVENT_MASK_SUBSTRUCTURE_REDIRECT |
		XCB_EVENT_MASK_PROPERTY_CHANGE,
	};

	/* setup class and cmd args */

	_argc = argc;
	_argv = argv;

	_class_class = class_class;
	_class_name  = class_name;

	/* init event buffer */

	_events = cobj_tracker_create(0);
	if (cobj_tracker_has_failed(_events))
	{
		goto fail_events;
	}

	/* open connection */

	if (!(_connection = connection ? connection : xcb_connect(NULL, NULL)))
	{
		goto fail_xserver;
	}

	/* find screen */

	if (!(_screen = xcb_setup_roots_iterator(xcb_get_setup(_connection)).data))
	{
		goto fail_xserver;
	}

	/* get depth */

	depth_it = xcb_screen_allowed_depths_iterator(_screen);
	for (; depth_it.rem; xcb_depth_next(&depth_it))
	{
		if (depth_it.data->depth == 32)
		{
			_depth = depth_it.data;
			break;
		}
	}
	if (!_depth)
	{
		goto fail_xserver;
	}

	/* get visual */

	visual_it = xcb_depth_visuals_iterator(_depth);
	for (; visual_it.rem; xcb_visualtype_next(&visual_it))
	{
		if (visual_it.data->_class == XCB_VISUAL_CLASS_TRUE_COLOR)
		{
			_visual = visual_it.data;
			break;
		}
	}
	if (!_visual)
	{
		goto fail_xserver;
	}

	/* create colormap */

	_colormap = xcb_generate_id(_connection);
	xc = xcb_create_colormap_checked(
		_connection,
		 XCB_COLORMAP_ALLOC_NONE,
		_colormap,
		_screen->root,
		_visual->visual_id);

	if (!_test_cookie(xc))
	{
		goto fail_xserver;
	}

	/* create leader window */

	_win_leader = xcb_generate_id(_connection);
	xc = xcb_create_window_checked(
		_connection,
		XCB_COPY_FROM_PARENT,
		_win_leader,
		_screen->root,
		0, 0,
		1, 1,
		0,
		XCB_WINDOW_CLASS_INPUT_OUTPUT,
		_screen->root_visual,
		XCB_CW_EVENT_MASK,
		leader_mask_vals);

	if (!_test_cookie(xc))
	{
		goto fail_leader;
	}

	/* create keysym map */

	if (!(_keysyms = xcb_key_symbols_alloc(_connection)))
	{
		goto fail_keysyms;
	}

	/* get atoms */

	_atom_clip = _get_atom("CLIPBOARD");
	_atom_time = _get_atom("TIMESTAMP");
	_atom_mult = _get_atom("MULTIPLE");
	_atom_trgt = _get_atom("TARGETS");
	_atom_utf8 = _get_atom("UTF8_STRING");
	_atom_prot = _get_atom("WM_PROTOCOLS");
	_atom_del  = _get_atom("WM_DELETE_WINDOW");
	_atom_foc  = _get_atom("WM_TAKE_FOCUS");
	_atom_nam  = _get_atom("WM_NAME");
	_atom_ico  = _get_atom("WM_ICON_NAME");
	_atom_cls  = _get_atom("WM_CLASS");
	_atom_cmd  = _get_atom("WM_COMMAND");
	_atom_host = _get_atom("WM_CLIENT_MACHINE");
	_atom_lead = _get_atom("WM_CLIENT_LEADER");
	_atom_ping = _get_atom("_NET_WM_PING");
	_atom_pid  = _get_atom("_NET_WM_PID");
	_atom_nnam = _get_atom("_NET_WM_NAME");
	_atom_nico = _get_atom("_NET_WM_ICON_NAME");
	_atom_isig = _get_atom("_INTERNAL_LOOP_SIGNAL");

	_atom_sig  = _get_atom(_ATOM_SIGNALS);
	_atom_vers = _get_atom(_ATOM_VERSION);
	_atom_stt  = _get_atom(_ATOM_WINDOW_STATES);
	_atom_dfoc = _get_atom(_ATOM_WINDOW_FOCUS);
	_atom_tmp1 = _get_atom(_ATOM_PASTE_TMP_1);
	_atom_tmp2 = _get_atom(_ATOM_PASTE_TMP_2);
	_atom_tmp3 = _get_atom(_ATOM_PASTE_TMP_3);
	_atom_won  = _get_atom(_ATOM_WINDOW_ACTIVE);
	_atom_wena = _get_atom(_ATOM_WINDOW_DISABLED);
	_atom_plck = _get_atom(_ATOM_WINDOW_GRID_LOCK);
	_atom_flck = _get_atom(_ATOM_WINDOW_FOCUS_LOCK);
	_atom_conf = _get_atom(_ATOM_RECONFIG);
	_atom_acl  = _get_atom(_ATOM_ACCEL);

	char s[20];
	for (int i = 0; i < CGUI_CONFIG_ACCELS; i++) {
		sprintf(s, _ATOM_ACCEL "_%i", i + 1);
		_atom_aclx[i] = _get_atom(s);
	}

	/* get extensions opcodes */

	_opcode_present = _get_extension_opcode("Present");
	_opcode_xinput  = _get_extension_opcode("XInputExtension");

	/* set leader window ICCCM properties */

	gethostname(host, 256);

	host_n = strlen(host);
	vers_n = strlen(CGUI_VERSION);
	name_n = strlen(_class_class);
	pid    = getpid();

	_prop_set(_win_leader, _atom_lead, XCB_ATOM_WINDOW,   1,      &_win_leader);
	_prop_set(_win_leader, _atom_pid,  XCB_ATOM_CARDINAL, 1,      &pid);
	_prop_set(_win_leader, _atom_nam,  XCB_ATOM_STRING,   host_n, host);
	_prop_set(_win_leader, _atom_vers, XCB_ATOM_STRING,   vers_n, CGUI_VERSION);
	_prop_set(_win_leader, _atom_nam,  XCB_ATOM_STRING,   name_n, _class_class);
	_prop_set(_win_leader, _atom_nnam, _atom_utf8,        name_n, _class_class);
	_prop_set(_win_leader, _atom_prot, XCB_ATOM_ATOM,     1,      &_atom_sig);

	for (int i = 0; i < _argc; i++)
	{
		_prop_append(_win_leader, _atom_cmd, XCB_ATOM_STRING, strlen(_argv[i]) + 1, _argv[i]);
	}

	/* end */
	
	xcb_flush(_connection);

	_failed = false;

	return true;

	/* errors */

fail_keysyms:
	xcb_destroy_window(_connection, _win_leader);
fail_leader:
	xcb_free_colormap(_connection, _colormap);
fail_xserver:
	cobj_tracker_destroy(&_events);
fail_events:
	_failed = true;

	return false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_reset(bool kill_connection)
{
	if (_failed)
	{
		return;
	}

	cobj_tracker_reset_iterator(_events);
	while (cobj_tracker_increment_iterator(_events))
	{
		free((void*)cobj_tracker_get_iteration(_events));
	}
	cobj_tracker_destroy(&_events);

	xcb_key_symbols_free(_keysyms);
	xcb_destroy_window(_connection, _win_leader);
	xcb_free_colormap(_connection, _colormap);
	if (kill_connection)
	{
		xcb_disconnect(_connection);
	}
	
	_failed = true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
x11_update(void)
{
	xcb_generic_event_t *event;

	/* grab next event from stack buffer if any  */
	/* otherwhise retrieve new event from server */

	if (cobj_tracker_get_size(_events) > 0)
	{
		event = (xcb_generic_event_t*)cobj_tracker_get_index(_events, 0);
		cobj_tracker_pull_index(_events, 0);
	}
	else
	{
		event = xcb_wait_for_event(_connection);
	}

	if (!event)
	{
		return false;
	}

	/* dispatch raw event to handlers to convert it into a CGUI event */

	switch (event->response_type & ~ 0x80)
	{
		// TODO
		
		default:
			_event_unknown(event);
			break;
	}

	/* end */

	free(event);

	xcb_flush(_connection);

	return true;
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static void
_event_unknown(xcb_generic_event_t *xcb_event)
{
	cgui_event_t cgui_event;

	cgui_event.kind      = CGUI_EVENT_UNKNOWN_XCB;
	cgui_event.xcb_event = xcb_event;

	main_update(&cgui_event);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static xcb_atom_t
_get_atom(const char *name)
{
	xcb_intern_atom_cookie_t xc;
	xcb_intern_atom_reply_t *xr;
	xcb_atom_t xa;

	xc = xcb_intern_atom(_connection, 0, strlen(name), name);
	xr = xcb_intern_atom_reply(_connection, xc, NULL);
	if (!xr)
	{
		return 0;
	}

	xa = xr->atom;
	free(xr);

	return xa;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static uint8_t
_get_extension_opcode(const char *name)
{
	xcb_query_extension_cookie_t xc;
	xcb_query_extension_reply_t *xr;

	uint8_t opcode;

	xc = xcb_query_extension(_connection, strlen(name), name);
	xr = xcb_query_extension_reply(_connection, xc, NULL);
	if (!xr)
	{
		return 0;
	}

	opcode = xr->major_opcode;
	free(xr);

	return opcode;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
_prop_append(xcb_window_t win, xcb_atom_t prop, xcb_atom_t type, uint32_t data_n, const void *data)
{
	xcb_void_cookie_t xc;

	xc = xcb_change_property_checked(
		_connection,
		XCB_PROP_MODE_APPEND,
		win,
		prop,
		type,
		type == _atom_utf8 || type == _atom_time || type == XCB_ATOM_STRING ? 8 : 32,
		data_n,
		data);

	return _test_cookie(xc);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
_prop_set(xcb_window_t win, xcb_atom_t prop, xcb_atom_t type, uint32_t data_n, const void *data)
{
	xcb_void_cookie_t xc;

	xc = xcb_change_property_checked(
		_connection,
		XCB_PROP_MODE_REPLACE,
		win,
		prop,
		type,
		type == _atom_utf8 || type == _atom_time || type == XCB_ATOM_STRING ? 8 : 32,
		data_n,
		data);

	return _test_cookie(xc);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
_test_cookie(xcb_void_cookie_t xc)
{
	xcb_generic_error_t *x_err;

	if ((x_err = xcb_request_check(_connection, xc)))
	{
		free(x_err);
		return false;
	}

	return true;
}

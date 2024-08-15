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
#include <xcb/xcb_icccm.h>
#include <xcb/xcb_keysyms.h>
#include <xcb/present.h>
#include <xcb/randr.h>
#include <xcb/xinput.h>
#include <xkbcommon/xkbcommon.h>

#include "config.h"
#include "main.h"
#include "window.h"
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

struct xi_input_mask
{
    	xcb_input_event_mask_t    head;
    	xcb_input_xi_event_mask_t mask;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/* helpers */

static cgui_window *_find_window          (xcb_window_t);
static xcb_atom_t   _get_atom             (const char *) CGUI_NONNULL(1);
static uint8_t      _get_extension_opcode (const char *) CGUI_NONNULL(1);
static bool         _prop_add             (xcb_window_t, xcb_atom_t, xcb_atom_t, uint32_t, const void *);
static bool         _prop_set             (xcb_window_t, xcb_atom_t, xcb_atom_t, uint32_t, const void *);
static bool         _test_cookie          (xcb_void_cookie_t);

/* event handlers */

static void _event_button            (xcb_button_press_event_t *)      CGUI_NONNULL(1);
static void _event_client_message    (xcb_client_message_event_t *)    CGUI_NONNULL(1);
static void _event_configure         (xcb_configure_notify_event_t *)  CGUI_NONNULL(1);
static void _event_enter             (xcb_enter_notify_event_t *)      CGUI_NONNULL(1);
static void _event_expose            (xcb_expose_event_t *)            CGUI_NONNULL(1);
static void _event_focus_in          (xcb_focus_in_event_t *)          CGUI_NONNULL(1);
static void _event_focus_out         (xcb_focus_out_event_t *)         CGUI_NONNULL(1);
static void _event_key               (xcb_key_press_event_t *)         CGUI_NONNULL(1);
static void _event_keymap            (xcb_mapping_notify_event_t *)    CGUI_NONNULL(1);
static void _event_leave             (xcb_leave_notify_event_t *)      CGUI_NONNULL(1);
static void _event_map               (xcb_map_notify_event_t *)        CGUI_NONNULL(1);
static void _event_motion            (xcb_motion_notify_event_t *)     CGUI_NONNULL(1);
static void _event_present           (xcb_present_generic_event_t *)   CGUI_NONNULL(1);
static void _event_selection_clear   (xcb_selection_clear_event_t *)   CGUI_NONNULL(1);
static void _event_selection_request (xcb_selection_request_event_t *) CGUI_NONNULL(1);
static void _event_unknown           (xcb_generic_event_t *)           CGUI_NONNULL(1);
static void _event_unmap             (xcb_unmap_notify_event_t *)      CGUI_NONNULL(1);
static void _event_xinput_touch      (xcb_input_touch_begin_event_t *) CGUI_NONNULL(1);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

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

static cref *_events = CREF_PLACEHOLDER;

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

xcb_connection_t *
x11_connection(void)
{
	return _connection;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_init(int argc, char **argv, const char *class_name, const char *class_class, xcb_connection_t *connection)
{
	xcb_visualtype_iterator_t visual_it;
	xcb_depth_iterator_t depth_it;
	xcb_void_cookie_t xc;
	char   s[20];
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

	_argc        = argc;
	_argv        = argv;
	_class_class = class_class;
	_class_name  = class_name;

	/* init event buffer */

	if ((_events = cref_create()) == CREF_PLACEHOLDER)
	{	
		main_set_error(CERR_MEMORY);
		return;
	}

	/* open connection */

	if (!(_connection = connection ? connection : xcb_connect(NULL, NULL)))
	{
		goto fail_server;
	}

	/* find screen */

	if (!(_screen = xcb_setup_roots_iterator(xcb_get_setup(_connection)).data))
	{
		goto fail_setup;
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
		goto fail_setup;
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
		goto fail_setup;
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
		goto fail_setup;
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

	for (int i = 0; i < CGUI_CONFIG_ACCELS; i++) {
		sprintf(s, _ATOM_ACCEL "_%i", i + 1);
		_atom_aclx[i] = _get_atom(s);
	}

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
		_prop_add(_win_leader, _atom_cmd, XCB_ATOM_STRING, strlen(_argv[i]) + 1, _argv[i]);
	}

	if (cgui_error())
	{
		goto fail_props;
	}

	/* get extensions opcodes */

	_opcode_present = _get_extension_opcode("Present");
	_opcode_xinput  = _get_extension_opcode("XInputExtension");

	/* end */
	
	xcb_flush(_connection);

	return;

	/* errors */

fail_props:
	xcb_key_symbols_free(_keysyms);
fail_keysyms:
	xcb_destroy_window(_connection, _win_leader);
fail_leader:
	xcb_free_colormap(_connection, _colormap);
fail_setup:
	if (!connection)
	{
		xcb_disconnect(_connection);
	}
	_connection = NULL;
fail_server:
	main_set_error(CERR_XCB);
	cref_destroy(_events);
	_events = CREF_PLACEHOLDER;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

xcb_window_t
x11_leader_window(void)
{
	if (cgui_error())
	{
		return 0;
	}

	return _win_leader;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_reset(bool kill_connection)
{
	CREF_FOR_EACH(_events, i)
	{
		free((void*)cref_ptr(_events, i));
	}
	cref_destroy(_events);
	_events = CREF_PLACEHOLDER;

	if (_connection)
	{
		xcb_key_symbols_free(_keysyms);
		xcb_destroy_window(_connection, _win_leader);
		xcb_free_colormap(_connection, _colormap);
		if (kill_connection)
		{
			xcb_disconnect(_connection);
		}
		_connection = NULL;
	}	
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_update(void)
{
	xcb_generic_event_t *event;

	if (cgui_error())
	{
		return;
	}

	/* grab next event from stack buffer if any  */
	/* otherwhise retrieve new event from server */

	if (cref_length(_events) > 0)
	{
		event = (xcb_generic_event_t*)cref_ptr(_events, 0);
		cref_pull(_events, 0);
	}
	else
	{
		main_unlock();
		event = xcb_wait_for_event(_connection);
		main_lock();
	}

	if (!event)
	{
		main_set_error(CERR_XCB);
		return;
	}

	/* dispatch raw event to handlers to convert it into a CGUI event */

	switch (event->response_type & ~ 0x80)
	{
		/* can mix key and button_press events because their structs have the same fields */

		case XCB_BUTTON_PRESS:
		case XCB_BUTTON_RELEASE:
			_event_button((xcb_button_press_event_t*)event);
			break;

		case XCB_KEY_PRESS:
		case XCB_KEY_RELEASE:
			_event_key((xcb_key_press_event_t*)event);
			break;

		case XCB_ENTER_NOTIFY:
			_event_enter((xcb_leave_notify_event_t*)event);
			break;

		case XCB_LEAVE_NOTIFY:
			_event_leave((xcb_leave_notify_event_t*)event);
			break;

		case XCB_UNMAP_NOTIFY:
			_event_unmap((xcb_unmap_notify_event_t*)event);
			break;

		case XCB_MAP_NOTIFY:
			_event_map((xcb_map_notify_event_t*)event);
			break;

		case XCB_EXPOSE:
			_event_expose((xcb_expose_event_t*)event);
			break;

		case XCB_MOTION_NOTIFY:
			_event_motion((xcb_motion_notify_event_t*)event);
			break;

		case XCB_FOCUS_IN:
			_event_focus_in((xcb_focus_in_event_t*)event);
			break;

		case XCB_FOCUS_OUT:
			_event_focus_out((xcb_focus_out_event_t*)event);
			break;

		case XCB_CLIENT_MESSAGE:
			_event_client_message((xcb_client_message_event_t*)event);
			break;

		case XCB_CONFIGURE_NOTIFY:
			_event_configure((xcb_configure_notify_event_t*)event);
			break;

		case XCB_MAPPING_NOTIFY:
			_event_keymap((xcb_mapping_notify_event_t*)event);
			break;

		case XCB_SELECTION_CLEAR:
			_event_selection_clear((xcb_selection_clear_event_t*)event);
			break;

		case XCB_SELECTION_REQUEST:
			_event_selection_request((xcb_selection_request_event_t*)event);
			break;

		case XCB_GE_GENERIC:
			if (((xcb_ge_generic_event_t*)event)->extension == _opcode_present)
			{
				_event_present((xcb_present_generic_event_t*)event);
			}
			else if (((xcb_ge_generic_event_t*)event)->extension == _opcode_xinput)
			{
				_event_xinput_touch((xcb_input_touch_begin_event_t*)event);
			}
			break;

		default:
			_event_unknown(event);
			break;
	}

	/* end */

	free(event);
	xcb_flush(_connection);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

xcb_visualtype_t *
x11_visual(void)
{
	return _visual;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_activate(xcb_window_t id)
{
	_test_cookie(xcb_map_window_checked(_connection, id));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
x11_window_create(xcb_window_t *id, int16_t x, int16_t y, uint16_t width, uint16_t height)
{
	xcb_void_cookie_t xc;
	char   host[256] = "";
	size_t host_n;
	size_t vers_n;
	size_t cls0_n;
	size_t cls1_n;
	size_t pid;

	struct xi_input_mask xi_mask =
	{
		.head.deviceid = XCB_INPUT_DEVICE_ALL_MASTER,
		.head.mask_len = 1,
		.mask          = XCB_INPUT_XI_EVENT_MASK_TOUCH_BEGIN
		               | XCB_INPUT_XI_EVENT_MASK_TOUCH_END
		               | XCB_INPUT_XI_EVENT_MASK_TOUCH_UPDATE,
	};

	const uint32_t ev_mask[] =
	{
		0x00000000,
		0x00000000,
		false,
		XCB_EVENT_MASK_EXPOSURE
		| XCB_EVENT_MASK_POINTER_MOTION
		| XCB_EVENT_MASK_BUTTON_PRESS
		| XCB_EVENT_MASK_BUTTON_RELEASE
		| XCB_EVENT_MASK_ENTER_WINDOW
		| XCB_EVENT_MASK_LEAVE_WINDOW
		| XCB_EVENT_MASK_KEYMAP_STATE
		| XCB_EVENT_MASK_KEY_PRESS
		| XCB_EVENT_MASK_KEY_RELEASE
		| XCB_EVENT_MASK_FOCUS_CHANGE
		| XCB_EVENT_MASK_VISIBILITY_CHANGE
		| XCB_EVENT_MASK_PROPERTY_CHANGE
		| XCB_EVENT_MASK_STRUCTURE_NOTIFY,
		_colormap,
	};

	/* create X window */

	*id = xcb_generate_id(_connection);
	xc = xcb_create_window_checked(
		_connection,
		_depth->depth,
		*id,
		_screen->root,
		x,
		y,
		width,
		height,
		0,
		XCB_WINDOW_CLASS_INPUT_OUTPUT,
		_visual->visual_id,
		XCB_CW_BACK_PIXEL
		| XCB_CW_BORDER_PIXEL
		| XCB_CW_OVERRIDE_REDIRECT
		| XCB_CW_EVENT_MASK
		| XCB_CW_COLORMAP,
		ev_mask);

	if (!_test_cookie(xc))
	{
		goto fail_id;
	}

	/* indicate that the window should receive Present extension events */

	xc = xcb_present_select_input_checked(
		_connection,
		xcb_generate_id(_connection),
		*id,
		XCB_PRESENT_EVENT_MASK_COMPLETE_NOTIFY);

	if (!_test_cookie(xc))
	{
		goto fail_present;
	}

	/* indicate that the window should receive XI touch extension events */


	xc = xcb_input_xi_select_events(
		_connection,
		*id,
		1,
		(xcb_input_event_mask_t*)(&xi_mask));
	
	if (!_test_cookie(xc))
	{
		goto fail_xi;
	}

	/* set window's X properties */

	gethostname(host, 256);

	host_n = strlen(host);
	vers_n = strlen(CGUI_VERSION);
	cls0_n = strlen(_class_name) + 1;
	cls1_n = strlen(_class_class);
	pid    = getpid();

	_prop_set(*id, _atom_host, XCB_ATOM_STRING,   host_n, host);
	_prop_set(*id, _atom_vers, XCB_ATOM_STRING,   vers_n, CGUI_VERSION);
	_prop_set(*id, _atom_lead, XCB_ATOM_WINDOW,   1,      &_win_leader);
	_prop_set(*id, _atom_pid,  XCB_ATOM_CARDINAL, 1,      &pid);

	_prop_set(*id, _atom_cls,  XCB_ATOM_STRING,   cls0_n, _class_name);
	_prop_add(*id, _atom_cls,  XCB_ATOM_STRING,   cls1_n, _class_class);

	_prop_set(*id, _atom_prot, XCB_ATOM_ATOM,     1,      &_atom_del);
	_prop_add(*id, _atom_prot, XCB_ATOM_ATOM,     1,      &_atom_foc);
	_prop_add(*id, _atom_prot, XCB_ATOM_ATOM,     1,      &_atom_ping);
	_prop_add(*id, _atom_prot, XCB_ATOM_ATOM,     1,      &_atom_sig);

	x11_window_rename(*id, NULL);
	x11_window_update_state_hints(*id, (struct cgui_window_state_flags){false});

	if (cgui_error())
	{
		goto fail_prop;
	}

	/* end */

	xcb_flush(_connection);

	return true;

	/* errors */

fail_prop:
fail_xi:
fail_present:
	xcb_destroy_window(_connection, *id);
fail_id:
	main_set_error(CERR_XCB);
	return false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_deactivate(xcb_window_t id)
{
	_test_cookie(xcb_unmap_window_checked(_connection, id));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_destroy(xcb_window_t id)
{
	_test_cookie(xcb_unmap_window_checked(_connection, id));
	_test_cookie(xcb_destroy_window_checked(_connection, id));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_present(xcb_window_t id, uint32_t serial)
{
	xcb_present_notify_msc(_connection, id, serial, 0, CONFIG->anim_divider, 0);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_rename(xcb_window_t id, const char *name)
{
	size_t n;

	if (!name)
	{
		name = _class_name;
	}
	n = strlen(name);

	_prop_set(id, _atom_nam,  XCB_ATOM_STRING, n, name);
	_prop_set(id, _atom_ico,  XCB_ATOM_STRING, n, name);
	_prop_set(id, _atom_nnam, _atom_utf8,      n, name);
	_prop_set(id, _atom_nnam, _atom_utf8,      n, name);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_set_accel(xcb_window_t id, int accel, const char *name)
{
	_test_cookie(xcb_delete_property_checked(_connection, id, _atom_aclx[accel]));
	if (name)
	{
		_prop_set(id, _atom_aclx[accel], _atom_utf8, strlen(name), name);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_set_transient(xcb_window_t id, xcb_window_t id_under)
{
	_prop_set(id, XCB_ATOM_WM_TRANSIENT_FOR, XCB_ATOM_WINDOW, 1, &id_under);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_set_urgency(xcb_window_t id, bool set_on)
{
	xcb_icccm_wm_hints_t *xhints;
	xcb_get_property_reply_t *xr;
	xcb_get_property_cookie_t xc;

	xc = xcb_get_property(_connection, 0, id, XCB_ATOM_WM_HINTS, XCB_ATOM_WM_HINTS, 0, UINT32_MAX);
	xr = xcb_get_property_reply(_connection, xc, NULL);
	if (!xr)
	{
		main_set_error(CERR_XCB);
		return;
	}

	xhints = xcb_get_property_value(xr);
	if (set_on)
	{
		xhints->flags |= XCB_ICCCM_WM_HINT_X_URGENCY;
	}
	else
	{
		xhints->flags &= ~XCB_ICCCM_WM_HINT_X_URGENCY;
	}

	_prop_set(id, XCB_ATOM_WM_HINTS, XCB_ATOM_WM_HINTS, sizeof(xcb_icccm_wm_hints_t), xhints);

	free(xr);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_update_size_hints(xcb_window_t id, uint16_t min_width, uint16_t min_height, uint16_t max_width, uint16_t max_height)
{
	const xcb_size_hints_t xhints =
	{
		.flags      = XCB_ICCCM_SIZE_HINT_P_MIN_SIZE | XCB_ICCCM_SIZE_HINT_P_MAX_SIZE,
		.min_width  = min_width,
		.min_height = min_height,
		.max_width  = max_width,
		.max_height = max_height,
	};

	_prop_set(id, XCB_ATOM_WM_NORMAL_HINTS, XCB_ATOM_WM_SIZE_HINTS, sizeof(xcb_size_hints_t), &xhints);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_update_state_hints(xcb_window_t id, struct cgui_window_state_flags state)
{
	_test_cookie(xcb_delete_property_checked(_connection, id, _atom_stt));

	if (state.active)
	{
		_prop_add(id, _atom_stt, XCB_ATOM_ATOM, 1, &_atom_won);
	}

	if (state.disabled)
	{
		_prop_add(id, _atom_stt, XCB_ATOM_ATOM, 1, &_atom_wena);
	}

	if (state.locked_grid)
	{
		_prop_add(id, _atom_stt, XCB_ATOM_ATOM, 1, &_atom_plck);
	}

	if (state.locked_focus)
	{
		_prop_add(id, _atom_stt, XCB_ATOM_ATOM, 1, &_atom_flck);
	}
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
_event_button(xcb_button_press_event_t *xcb_event)
{
	struct cgui_event event = {0};

	(void)xcb_event; // TODO

	main_update(&event);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_event_client_message(xcb_client_message_event_t *xcb_event)
{
	struct cgui_event event = {0};

	const xcb_atom_t a1 = xcb_event->data.data32[0];
	const xcb_atom_t a2 = xcb_event->data.data32[1];
	const xcb_atom_t a3 = xcb_event->data.data32[2];

	if (xcb_event->type != _atom_prot)
	{
		return;
	}

	/* window independent message */

	if (a1 == _atom_sig && a2 == _atom_conf)
	{
		event.type   = CGUI_EVENT_RECONFIG;
		event.window = CGUI_WINDOW_PLACEHOLDER;
		goto update;
	}

	/* window dependent messages */

	if (a1 == _atom_sig && a2 == _atom_acl)
	{
		event.type        = CGUI_EVENT_ACCELERATOR;
		event.window      = _find_window(xcb_event->window);
		event.accelerator = a3;
		goto update;
	}

	if (a1 == _atom_del)
	{
		event.type   = CGUI_EVENT_CLOSE;
		event.window = _find_window(xcb_event->window);
		goto update;
	}

	if (a1 == _atom_foc)
	{
		xcb_set_input_focus(_connection, XCB_INPUT_FOCUS_PARENT, xcb_event->window, XCB_CURRENT_TIME);
		return;
	}

	if (a1 == _atom_ping)
	{
		xcb_event->window = _screen->root;
		xcb_send_event(_connection, 0, _screen->root, XCB_EVENT_MASK_NO_EVENT, (char*)xcb_event);
		xcb_flush(_connection);
		return;
	}

	/* send CGUI event */

update:

	main_update(&event);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_event_configure(xcb_configure_notify_event_t *xcb_event)
{
	struct cgui_event event =
	{
		.type             = CGUI_EVENT_TRANSFORM,
		.window           = _find_window(xcb_event->window),
		.transform_x      = xcb_event->x,
		.transform_y      = xcb_event->y,
		.transform_width  = xcb_event->width,
		.transform_height = xcb_event->height,
	};

	main_update(&event);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_event_enter(xcb_enter_notify_event_t *xcb_event)
{
	struct cgui_event event = {0};

	(void)xcb_event; // TODO

	main_update(&event);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_event_expose(xcb_expose_event_t *xcb_event)
{
	struct cgui_event event =
	{
		.type       = CGUI_EVENT_REDRAW,
		.window     = _find_window(xcb_event->window),
		.redraw_all = true,
	};

	main_update(&event);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_event_focus_in(xcb_focus_in_event_t *xcb_event)
{
	struct cgui_event event =
	{
		.type   = CGUI_EVENT_FOCUS,
		.window = _find_window(xcb_event->event),
	};

	main_update(&event);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_event_focus_out(xcb_focus_out_event_t *xcb_event)
{
	struct cgui_event event =
	{
		.type   = CGUI_EVENT_UNFOCUS,
		.window = _find_window(xcb_event->event),
	};

	/* mode values significations (maybe) (common for both focus in and out) :                     */
	/* 0 = explictit focus change by the end-user                                                  */
	/* 1 = focus temporary lost when window is resized or moved      (never happens for focus in ) */
	/* 2 = focus gain after expose event after move or resize action (never happens for focus out) */
	/* 3 = focus change due to a workspace change when the window is focused                       */

	if (xcb_event->mode != 1)
	{
		main_update(&event);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_event_key(xcb_key_press_event_t *xcb_event)
{
	struct cgui_event event = {0};

	(void)xcb_event; // TODO

	main_update(&event);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_event_keymap(xcb_mapping_notify_event_t *xcb_event)
{
	xcb_refresh_keyboard_mapping(_keysyms, xcb_event);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_event_leave(xcb_leave_notify_event_t *xcb_event)
{
	struct cgui_event event = {0};

	(void)xcb_event; // TODO

	main_update(&event);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_event_map(xcb_map_notify_event_t *xcb_event)
{
	struct cgui_event event =
	{
		.type   = CGUI_EVENT_MAP,
		.window = _find_window(xcb_event->window),
	};

	main_update(&event);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_event_motion(xcb_motion_notify_event_t *xcb_event)
{
	struct cgui_event event = {0};

	(void)xcb_event; // TODO

	main_update(&event);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_event_present(xcb_present_generic_event_t *xcb_event)
{
	xcb_present_complete_notify_event_t *present = (xcb_present_complete_notify_event_t*)xcb_event;

	struct cgui_event event =
	{
		.type       = CGUI_EVENT_REDRAW,
		.window     = _find_window(present->window),
		.redraw_all = false,
	};

	if (xcb_event->evtype != XCB_PRESENT_EVENT_COMPLETE_NOTIFY
	 || present->kind     != XCB_PRESENT_COMPLETE_KIND_NOTIFY_MSC
	 || present->serial   != event.window->x_serial)
	{
		return;
	}

	main_update(&event);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_event_selection_clear(xcb_selection_clear_event_t *xcb_event)
{
	struct cgui_event event = {0};

	(void)xcb_event; // TODO

	main_update(&event);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_event_selection_request(xcb_selection_request_event_t *xcb_event)
{
	struct cgui_event event = {0};

	(void)xcb_event; // TODO

	main_update(&event);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_event_unknown(xcb_generic_event_t *xcb_event)
{
	struct cgui_event event =
	{
		.type      = CGUI_EVENT_UNKNOWN_XCB,
		.window    = CGUI_WINDOW_PLACEHOLDER,
		.xcb_event = xcb_event,
	};

	main_update(&event);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_event_unmap(xcb_unmap_notify_event_t *xcb_event)
{
	struct cgui_event event =
	{
		.type   = CGUI_EVENT_UNMAP,
		.window = _find_window(xcb_event->window),
	};

	main_update(&event);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_event_xinput_touch(xcb_input_touch_begin_event_t *xcb_event)
{
	struct cgui_event event =
	{
		.window   = _find_window(xcb_event->event),
		.touch_x  = xcb_event->event_x >> 16,
		.touch_y  = xcb_event->event_y >> 16,
		.touch_id = xcb_event->detail,
	};

	switch (xcb_event->event_type)
	{
		case XCB_INPUT_TOUCH_BEGIN:
			event.type = CGUI_EVENT_TOUCH_BEGIN;
			break;

		case XCB_INPUT_TOUCH_UPDATE:
			event.type = CGUI_EVENT_TOUCH_UPDATE;
			break;

		case XCB_INPUT_TOUCH_END:
			event.type = CGUI_EVENT_TOUCH_END;
			break;

		default:
			return;
	}

	main_update(&event);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static cgui_window *
_find_window(xcb_window_t id)
{
	const cref *windows = main_windows();

	CREF_FOR_EACH(windows, i)
	{
		if (((cgui_window*)cref_ptr(windows, i))->x_id == id)
		{
			return (cgui_window*)cref_ptr(windows, i);
		}
	}

	return CGUI_WINDOW_PLACEHOLDER;
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
_prop_add(xcb_window_t win, xcb_atom_t prop, xcb_atom_t type, uint32_t data_n, const void *data)
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
		main_set_error(CERR_XCB);
		free(x_err);
		return false;
	}

	return true;
}

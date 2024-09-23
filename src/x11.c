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
#include "util.h"
#include "x11.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/* custom atoms names */

#define ATOM_VERSION           "_CGUI_VERSION"
#define ATOM_SIGNALS           "_CGUI_SIGNALS"
#define ATOM_RECONFIG          "_CGUI_RECONFIG"
#define ATOM_ACCEL             "_CGUI_ACCEL"
#define ATOM_WINDOW_STATES     "_CGUI_WINDOW_STATE"
#define ATOM_WINDOW_ACTIVE     "_CGUI_STATE_WIN_ACTIVE"
#define ATOM_WINDOW_DISABLED   "_CGUI_STATE_WIN_DISABLED"
#define ATOM_WINDOW_GRID_LOCK  "_CGUI_STATE_GRID_LOCK"
#define ATOM_WINDOW_FOCUS_LOCK "_CGUI_STATE_FOCUS_LOCK"
#define ATOM_WINDOW_FOCUS      "_CGUI_WINDOW_FOCUS"
#define ATOM_PASTE_TMP_1       "_CGUI_PASTE_TMP_1"
#define ATOM_PASTE_TMP_2       "_CGUI_PASTE_TMP_2"
#define ATOM_PASTE_TMP_3       "_CGUI_PASTE_TMP_3"

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

/* conversion */

#define TO_INT(D)  util_limit(D, INT16_MIN,  INT16_MAX)
#define TO_UINT(D) util_limit(D,       0.0, UINT16_MAX)

/* helpers */

static cgui_window     *find_window          (xcb_window_t);
static xcb_atom_t       get_atom             (const char *) CGUI_NONNULL(1);
static uint8_t          get_extension_opcode (const char *) CGUI_NONNULL(1);
static bool             prop_add             (xcb_window_t, xcb_atom_t, xcb_atom_t, uint32_t, const void *);
static bool             prop_set             (xcb_window_t, xcb_atom_t, xcb_atom_t, uint32_t, const void *);
static bool             test_cookie          (xcb_void_cookie_t);
static struct cgui_mods translate_mods       (uint16_t state);

/* event handlers */

static void event_button            (xcb_button_press_event_t *, bool) CGUI_NONNULL(1);
static void event_client_message    (xcb_client_message_event_t *)     CGUI_NONNULL(1);
static void event_configure         (xcb_configure_notify_event_t *)   CGUI_NONNULL(1);
static void event_enter             (xcb_enter_notify_event_t *)       CGUI_NONNULL(1);
static void event_expose            (xcb_expose_event_t *)             CGUI_NONNULL(1);
static void event_focus_in          (xcb_focus_in_event_t *)           CGUI_NONNULL(1);
static void event_focus_out         (xcb_focus_out_event_t *)          CGUI_NONNULL(1);
static void event_key               (xcb_key_press_event_t *, bool)    CGUI_NONNULL(1);
static void event_keymap            (xcb_mapping_notify_event_t *)     CGUI_NONNULL(1);
static void event_leave             (xcb_leave_notify_event_t *)       CGUI_NONNULL(1);
static void event_map               (xcb_map_notify_event_t *)         CGUI_NONNULL(1);
static void event_motion            (xcb_motion_notify_event_t *)      CGUI_NONNULL(1);
static void event_present           (xcb_present_generic_event_t *)    CGUI_NONNULL(1);
static void event_selection_clear   (xcb_selection_clear_event_t *)    CGUI_NONNULL(1);
static void event_selection_request (xcb_selection_request_event_t *)  CGUI_NONNULL(1);
static void event_unknown           (xcb_generic_event_t *)            CGUI_NONNULL(1);
static void event_unmap             (xcb_unmap_notify_event_t *)       CGUI_NONNULL(1);
static void event_xinput_touch      (xcb_input_touch_begin_event_t *)  CGUI_NONNULL(1);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/* ICCCM properties */

static char  const *class_name  = NULL;
static char  const *class_class = NULL;
static char *const *argv        = NULL;
static int          argc        = 0;

/* xcb globals */

static xcb_connection_t  *connection = NULL;
static xcb_screen_t      *screen     = NULL;
static xcb_depth_t       *depth      = NULL;
static xcb_visualtype_t  *visual     = NULL;
static xcb_key_symbols_t *keysyms    = NULL;
static xcb_colormap_t     colormap   = 0;
static xcb_window_t       win_leader = 0;

/* common atoms */

static xcb_atom_t atom_clip = 0; /* "CLIPBOARD"                   */
static xcb_atom_t atom_time = 0; /* "TIMESTAMP"                   */
static xcb_atom_t atom_mult = 0; /* "MULTIPLE"                    */
static xcb_atom_t atom_trgt = 0; /* "TARGETS"                     */
static xcb_atom_t atom_utf8 = 0; /* "UTF8_STRING"                 */
static xcb_atom_t atom_prot = 0; /* "WM_PROTOCOLS"                */
static xcb_atom_t atom_del  = 0; /* "WM_DELETE_WINDOW"            */
static xcb_atom_t atom_foc  = 0; /* "WM_TAKE_FOCUS"               */
static xcb_atom_t atom_nam  = 0; /* "WM_NAME"                     */
static xcb_atom_t atom_ico  = 0; /* "WM_ICON_MANE"                */
static xcb_atom_t atom_cls  = 0; /* "WM_CLASS"                    */
static xcb_atom_t atom_cmd  = 0; /* "WM_COMMAND"                  */
static xcb_atom_t atom_host = 0; /* "WM_CLIENT_MACHINE"           */
static xcb_atom_t atom_lead = 0; /* "WM_CLIENT_LEADER"            */
static xcb_atom_t atom_ping = 0; /* "_NET_WM_PING"                */
static xcb_atom_t atom_pid  = 0; /* "_NET_WM_PID"                 */
static xcb_atom_t atom_nnam = 0; /* "_NET_WM_NAME"                */
static xcb_atom_t atom_nico = 0; /* "_NET_WM_ICON_NAME"           */
static xcb_atom_t atom_wtyp = 0; /* "_NET_WM_WINDOW_TYPE"         */
static xcb_atom_t atom_wnom = 0; /* "_NET_WM_WINDOW_TYPE_NORMAL"  */
static xcb_atom_t atom_wdsk = 0; /* "_NET_WM_WINDOW_TYPE_DESKTOP" */
static xcb_atom_t atom_wovr = 0; /* "_NET_WM_WINDOW_TYPE_OVERLAY" */
static xcb_atom_t atom_wdlg = 0; /* "_NET_WM_WINDOW_TYPE_DIALOG"  */
static xcb_atom_t atom_nstt = 0; /* "_NET_WM_STATE"               */
static xcb_atom_t atom_full = 0; /* "_NET_WM_STATE_FULLSREEN"     */

/* CGUI custom atoms */

static xcb_atom_t atom_sig  = 0; /* _ATOM_SIGNALS           */
static xcb_atom_t atom_vers = 0; /* _ATOM_VERSION           */
static xcb_atom_t atom_stt  = 0; /* _ATOM_WINDOW_STATES     */
static xcb_atom_t atom_wfoc = 0; /* _ATOM_WINDOW_FOCUS      */
static xcb_atom_t atom_tmp1 = 0; /* _ATOM_PASTE_TMP_1       */
static xcb_atom_t atom_tmp2 = 0; /* _ATOM_PASTE_TMP_2       */
static xcb_atom_t atom_tmp3 = 0; /* _ATOM_PASTE_TMP_3       */
static xcb_atom_t atom_won  = 0; /* _ATOM_WINDOW_ACTIVE     */
static xcb_atom_t atom_wena = 0; /* _ATOM_WINDOW_DISABLED   */
static xcb_atom_t atom_plck = 0; /* _ATOM_WINDOW_GRID_LOCK  */
static xcb_atom_t atom_flck = 0; /* _ATOM_WINDOW_FOCUS_LOCK */
static xcb_atom_t atom_conf = 0; /* _ATOM_RECONFIG          */
static xcb_atom_t atom_acl  = 0; /* _ATOM_ACCEL             */

static xcb_atom_t atom_isig = 0;                       /* "_INTERNAL_LOOP_SIGNAL"          */
static xcb_atom_t atom_aclx[CGUI_CONFIG_ACCELS] = {0}; /* "_CGUI_WINDOW_ACCEL_x" x = 1..12 */

/* extensions op codes */

static uint8_t opcode_present = 0;
static uint8_t opcode_xinput  = 0;

/* events buffer */

static cref *events = CREF_PLACEHOLDER;

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

xcb_connection_t *
x11_connection(void)
{
	return connection;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_init(int argc_, char **argv_, const char *class_name_, const char *class_class_, xcb_connection_t *conn)
{
	xcb_visualtype_iterator_t visual_it;
	xcb_depth_iterator_t depth_it;
	xcb_void_cookie_t xc;
	uint8_t depth_target;
	size_t host_n;
	size_t name_n;
	size_t vers_n;
	size_t pid;
	char s[20];
	char host[256] = "";
	bool match = false;

	const uint32_t leader_mask_vals[] =
	{
		XCB_EVENT_MASK_SUBSTRUCTURE_NOTIFY   |
		XCB_EVENT_MASK_SUBSTRUCTURE_REDIRECT |
		XCB_EVENT_MASK_PROPERTY_CHANGE,
	};

	/* setup class and cmd args */

	argc        = argc_;
	argv        = argv_;
	class_class = class_class_;
	class_name  = class_name_;

	/* init event buffer */

	if ((events = cref_create()) == CREF_PLACEHOLDER)
	{	
		main_set_error(CERR_MEMORY);
		return;
	}

	/* open connection */

	if (!(connection = conn ? conn : xcb_connect(NULL, NULL)))
	{
		goto fail_server;
	}

	/* find screen */

	if (!(screen = xcb_setup_roots_iterator(xcb_get_setup(connection)).data))
	{
		goto fail_setup;
	}

	/* get depth */

	depth_target = CONFIG->alt_present ? screen->root_depth : 32;
	depth_it     = xcb_screen_allowed_depths_iterator(screen);
	for (; depth_it.rem; xcb_depth_next(&depth_it))
	{
		if (depth_it.data->depth == depth_target)
		{
			depth = depth_it.data;
			break;
		}
	}
	if (!depth)
	{
		goto fail_setup;
	}

	/* get visual */

	visual_it = xcb_depth_visuals_iterator(depth);
	for (; visual_it.rem; xcb_visualtype_next(&visual_it))
	{
		if (CONFIG->alt_present)
		{
			match = visual_it.data->visual_id == screen->root_visual;
		}
		else
		{
			match = visual_it.data->_class == XCB_VISUAL_CLASS_TRUE_COLOR;
		}
		if (match)
		{
			visual = visual_it.data;
			break;
		}
	}
	if (!visual)
	{
		goto fail_setup;
	}

	/* create colormap */

	colormap = xcb_generate_id(connection);
	xc = xcb_create_colormap_checked(
		connection,
		XCB_COLORMAP_ALLOC_NONE,
		colormap,
		screen->root,
		visual->visual_id);

	if (!test_cookie(xc))
	{
		goto fail_setup;
	}

	/* create leader window */

	win_leader = xcb_generate_id(connection);
	xc = xcb_create_window_checked(
		connection,
		XCB_COPY_FROM_PARENT,
		win_leader,
		screen->root,
		0, 0,
		1, 1,
		0,
		XCB_WINDOW_CLASS_INPUT_OUTPUT,
		screen->root_visual,
		XCB_CW_EVENT_MASK,
		leader_mask_vals);

	if (!test_cookie(xc))
	{
		goto fail_leader;
	}

	/* create keysym map */

	if (!(keysyms = xcb_key_symbols_alloc(connection)))
	{
		goto fail_keysyms;
	}

	/* get atoms */

	atom_clip = get_atom("CLIPBOARD");
	atom_time = get_atom("TIMESTAMP");
	atom_mult = get_atom("MULTIPLE");
	atom_trgt = get_atom("TARGETS");
	atom_utf8 = get_atom("UTF8_STRING");
	atom_prot = get_atom("WM_PROTOCOLS");
	atom_del  = get_atom("WM_DELETE_WINDOW");
	atom_foc  = get_atom("WM_TAKE_FOCUS");
	atom_nam  = get_atom("WM_NAME");
	atom_ico  = get_atom("WM_ICON_NAME");
	atom_cls  = get_atom("WM_CLASS");
	atom_cmd  = get_atom("WM_COMMAND");
	atom_host = get_atom("WM_CLIENT_MACHINE");
	atom_lead = get_atom("WM_CLIENT_LEADER");
	atom_ping = get_atom("_NET_WM_PING");
	atom_pid  = get_atom("_NET_WM_PID");
	atom_nnam = get_atom("_NET_WM_NAME");
	atom_nico = get_atom("_NET_WM_ICON_NAME");
	atom_isig = get_atom("_INTERNAL_LOOP_SIGNAL");
	atom_wtyp = get_atom("_NET_WM_WINDOW_TYPE");
	atom_wnom = get_atom("_NET_WM_WINDOW_TYPE_NORMAL");
	atom_wdsk = get_atom("_NET_WM_WINDOW_TYPE_DESKTOP");
	atom_wovr = get_atom("_NET_WM_WINDOW_TYPE_DOCK");
	atom_wdlg = get_atom("_NET_WM_WINDOW_TYPE_DIALOG");
	atom_nstt = get_atom("_NET_WM_STATE");
	atom_full = get_atom("_NET_WM_STATE_FULLSCREEN");

	atom_sig  = get_atom(ATOM_SIGNALS);
	atom_vers = get_atom(ATOM_VERSION);
	atom_stt  = get_atom(ATOM_WINDOW_STATES);
	atom_wfoc = get_atom(ATOM_WINDOW_FOCUS);
	atom_tmp1 = get_atom(ATOM_PASTE_TMP_1);
	atom_tmp2 = get_atom(ATOM_PASTE_TMP_2);
	atom_tmp3 = get_atom(ATOM_PASTE_TMP_3);
	atom_won  = get_atom(ATOM_WINDOW_ACTIVE);
	atom_wena = get_atom(ATOM_WINDOW_DISABLED);
	atom_plck = get_atom(ATOM_WINDOW_GRID_LOCK);
	atom_flck = get_atom(ATOM_WINDOW_FOCUS_LOCK);
	atom_conf = get_atom(ATOM_RECONFIG);
	atom_acl  = get_atom(ATOM_ACCEL);

	for (int i = 0; i < CGUI_CONFIG_ACCELS; i++) {
		sprintf(s, ATOM_ACCEL "_%i", i + 1);
		atom_aclx[i] = get_atom(s);
	}

	/* set leader window ICCCM properties */

	gethostname(host, 256);

	host_n = strlen(host);
	vers_n = strlen(CGUI_VERSION);
	name_n = strlen(class_class);
	pid    = getpid();

	prop_set(win_leader, atom_lead, XCB_ATOM_WINDOW,   1,      &win_leader);
	prop_set(win_leader, atom_pid,  XCB_ATOM_CARDINAL, 1,      &pid);
	prop_set(win_leader, atom_nam,  XCB_ATOM_STRING,   host_n, host);
	prop_set(win_leader, atom_vers, XCB_ATOM_STRING,   vers_n, CGUI_VERSION);
	prop_set(win_leader, atom_nam,  XCB_ATOM_STRING,   name_n, class_class);
	prop_set(win_leader, atom_nnam, atom_utf8,         name_n, class_class);
	prop_set(win_leader, atom_prot, XCB_ATOM_ATOM,     1,      &atom_sig);

	for (int i = 0; i < argc; i++)
	{
		prop_add(win_leader, atom_cmd, XCB_ATOM_STRING, strlen(argv[i]) + 1, argv[i]);
	}

	if (cgui_error())
	{
		goto fail_props;
	}

	/* get extensions opcodes */

	opcode_present = get_extension_opcode("Present");
	opcode_xinput  = get_extension_opcode("XInputExtension");

	/* end */
	
	xcb_flush(connection);

	return;

	/* errors */

fail_props:
	xcb_key_symbols_free(keysyms);
fail_keysyms:
	xcb_destroy_window(connection, win_leader);
fail_leader:
	xcb_free_colormap(connection, colormap);
fail_setup:
	if (!conn)
	{
		xcb_disconnect(connection);
	}
	connection = NULL;
fail_server:
	main_set_error(CERR_XCB);
	cref_destroy(events);
	events = CREF_PLACEHOLDER;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_key(uint8_t keycode, struct cgui_mods mods, uint32_t *keysym, uint32_t *utf32, char utf8[static 8])
{
	int shift = 0;

	if (mods.mod_5)
	{
		shift += 2;
	}

	if (mods.shift != mods.capslock)
	{
		shift += 1;
	}

	*keysym = xcb_key_symbols_get_keysym(keysyms, keycode, shift);
	*utf32  = xkb_keysym_to_utf32(*keysym);
	xkb_keysym_to_utf8(*keysym, utf8, 8);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

xcb_window_t
x11_leader_window(void)
{
	if (cgui_error())
	{
		return 0;
	}

	return win_leader;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_reset(bool kill_connection)
{
	CREF_FOR_EACH(events, i)
	{
		free((void*)cref_ptr(events, i));
	}
	cref_destroy(events);
	events = CREF_PLACEHOLDER;

	if (connection)
	{
		xcb_key_symbols_free(keysyms);
		xcb_destroy_window(connection, win_leader);
		xcb_free_colormap(connection, colormap);
		if (kill_connection)
		{
			xcb_disconnect(connection);
		}
		connection = NULL;
	}	
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct cgui_screen
x11_screen(size_t i, size_t *n, size_t *primary)
{
	xcb_randr_get_monitors_cookie_t xc;
	xcb_randr_get_monitors_reply_t *xr;
	xcb_randr_monitor_info_iterator_t xi;
	struct cgui_screen s = {0};

	*primary = 0;
	*n = 0;
	xc = xcb_randr_get_monitors(connection, screen->root, 1);
	xr = xcb_randr_get_monitors_reply(connection, xc, NULL);
	if (!xr)
	{
		main_set_error(CERR_XCB);
		return s;
	}

	*n = xcb_randr_get_monitors_monitors_length(xr);
	xi = xcb_randr_get_monitors_monitors_iterator(xr);
	for (size_t j = 0; xi.rem; j++)
	{
		s.x       = xi.data->x;
		s.y       = xi.data->y;
		s.width   = xi.data->width;
		s.height  = xi.data->height;
		s.primary = xi.data->primary;
		if (s.primary)
		{
			*primary = i;
		}

		xcb_randr_monitor_info_next(&xi);
		if (j == i)
		{
			break;
		}
	}

	free(xr);

	return s;
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

	if (cref_length(events) > 0)
	{
		event = (xcb_generic_event_t*)cref_ptr(events, 0);
		cref_pull(events, 0);
	}
	else
	{
		main_unlock();
		event = xcb_wait_for_event(connection);
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
			event_button((xcb_button_press_event_t*)event, true);
			break;

		case XCB_BUTTON_RELEASE:
			event_button((xcb_button_press_event_t*)event, false);
			break;

		case XCB_KEY_PRESS:
			event_key((xcb_key_press_event_t*)event, true);
			break;

		case XCB_KEY_RELEASE:
			event_key((xcb_key_press_event_t*)event, false);
			break;

		case XCB_ENTER_NOTIFY:
			event_enter((xcb_leave_notify_event_t*)event);
			break;

		case XCB_LEAVE_NOTIFY:
			event_leave((xcb_leave_notify_event_t*)event);
			break;

		case XCB_UNMAP_NOTIFY:
			event_unmap((xcb_unmap_notify_event_t*)event);
			break;

		case XCB_MAP_NOTIFY:
			event_map((xcb_map_notify_event_t*)event);
			break;

		case XCB_EXPOSE:
			event_expose((xcb_expose_event_t*)event);
			break;

		case XCB_MOTION_NOTIFY:
			event_motion((xcb_motion_notify_event_t*)event);
			break;

		case XCB_FOCUS_IN:
			event_focus_in((xcb_focus_in_event_t*)event);
			break;

		case XCB_FOCUS_OUT:
			event_focus_out((xcb_focus_out_event_t*)event);
			break;

		case XCB_CLIENT_MESSAGE:
			event_client_message((xcb_client_message_event_t*)event);
			break;

		case XCB_CONFIGURE_NOTIFY:
			event_configure((xcb_configure_notify_event_t*)event);
			break;

		case XCB_MAPPING_NOTIFY:
			event_keymap((xcb_mapping_notify_event_t*)event);
			break;

		case XCB_SELECTION_CLEAR:
			event_selection_clear((xcb_selection_clear_event_t*)event);
			break;

		case XCB_SELECTION_REQUEST:
			event_selection_request((xcb_selection_request_event_t*)event);
			break;

		case XCB_GE_GENERIC:
			if (((xcb_ge_generic_event_t*)event)->extension == opcode_present)
			{
				event_present((xcb_present_generic_event_t*)event);
			}
			else if (((xcb_ge_generic_event_t*)event)->extension == opcode_xinput)
			{
				event_xinput_touch((xcb_input_touch_begin_event_t*)event);
			}
			break;

		default:
			event_unknown(event);
			break;
	}

	/* end */

	free(event);
	xcb_flush(connection);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

xcb_visualtype_t *
x11_visual(void)
{
	return visual;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_activate(xcb_window_t id)
{
	test_cookie(xcb_map_window_checked(connection, id));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
x11_window_create(xcb_window_t *id, xcb_pixmap_t *buffer, double x, double y, double width, double height)
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
		XCB_GRAVITY_NORTH_WEST,
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
		colormap,
	};

	/* create X window */

	*id = xcb_generate_id(connection);
	xc = xcb_create_window_checked(
		connection,
		depth->depth,
		*id,
		screen->root,
		TO_INT(x),
		TO_INT(y),
		TO_UINT(width),
		TO_UINT(height),
		0,
		XCB_WINDOW_CLASS_INPUT_OUTPUT,
		visual->visual_id,
		XCB_CW_BACK_PIXEL
		| XCB_CW_BORDER_PIXEL
		| XCB_CW_BIT_GRAVITY
		| XCB_CW_EVENT_MASK
		| XCB_CW_COLORMAP,
		ev_mask);

	if (!test_cookie(xc))
	{
		goto fail_id;
	}

	/* create buffer for present operations, it won't be used if alt_present = false */

	*buffer = xcb_generate_id(connection);
	xc = xcb_create_pixmap_checked(connection, depth->depth, *buffer, *id, width, height);

	if (!test_cookie(xc))
	{
		goto fail_buffer;
	}

	/* indicate that the window should receive Present extension events */

	xc = xcb_present_select_input_checked(
		connection,
		xcb_generate_id(connection),
		*id,
		XCB_PRESENT_EVENT_MASK_COMPLETE_NOTIFY);

	if (!test_cookie(xc))
	{
		goto fail_present;
	}

	/* indicate that the window should receive XI touch extension events */

	xc = xcb_input_xi_select_events(
		connection,
		*id,
		1,
		(xcb_input_event_mask_t*)(&xi_mask));
	
	if (!test_cookie(xc))
	{
		goto fail_xi;
	}

	/* set window's X properties */

	gethostname(host, 256);

	host_n = strlen(host);
	vers_n = strlen(CGUI_VERSION);
	cls0_n = strlen(class_name) + 1;
	cls1_n = strlen(class_class);
	pid    = getpid();

	prop_set(*id, atom_host, XCB_ATOM_STRING,   host_n, host);
	prop_set(*id, atom_vers, XCB_ATOM_STRING,   vers_n, CGUI_VERSION);
	prop_set(*id, atom_lead, XCB_ATOM_WINDOW,   1,      &win_leader);
	prop_set(*id, atom_pid,  XCB_ATOM_CARDINAL, 1,      &pid);

	prop_set(*id, atom_cls,  XCB_ATOM_STRING,   cls0_n, class_name);
	prop_add(*id, atom_cls,  XCB_ATOM_STRING,   cls1_n, class_class);

	prop_set(*id, atom_prot, XCB_ATOM_ATOM,     1,      &atom_del);
	prop_add(*id, atom_prot, XCB_ATOM_ATOM,     1,      &atom_foc);
	prop_add(*id, atom_prot, XCB_ATOM_ATOM,     1,      &atom_ping);
	prop_add(*id, atom_prot, XCB_ATOM_ATOM,     1,      &atom_sig);

	x11_window_rename(*id, NULL);
	x11_window_update_state_hints(*id, (struct cgui_window_state_flags){false});

	if (cgui_error())
	{
		goto fail_prop;
	}

	/* end */

	xcb_flush(connection);

	return true;

	/* errors */

fail_prop:
fail_xi:
fail_present:
fail_buffer:
	xcb_destroy_window(connection, *id);
fail_id:
	main_set_error(CERR_XCB);
	return false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_deactivate(xcb_window_t id)
{
	test_cookie(xcb_unmap_window_checked(connection, id));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_destroy(xcb_window_t id, xcb_pixmap_t buffer)
{
	test_cookie(xcb_unmap_window_checked(connection, id));
	test_cookie(xcb_destroy_window_checked(connection, id));
	test_cookie(xcb_free_pixmap_checked(connection, buffer));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_move(xcb_window_t id, double x, double y)
{
	test_cookie(
		xcb_configure_window_checked(
			connection,
			id,
			XCB_CONFIG_WINDOW_X | XCB_CONFIG_WINDOW_Y,
			(uint32_t[2]){TO_INT(x), TO_INT(y)}));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_present(xcb_window_t id, xcb_pixmap_t buffer, uint32_t serial, bool async)
{
	if (CONFIG->alt_present)
	{
		xcb_present_pixmap(
			connection,
			id,
			buffer,
			serial, 
			XCB_XFIXES_REGION_NONE,
			XCB_XFIXES_REGION_NONE,
			0, 0,
			0,
			0,
			0,
			async ? XCB_PRESENT_OPTION_ASYNC : XCB_PRESENT_OPTION_NONE,
			0, async ? 0 : CONFIG->anim_divider, 0,
			0,
			NULL);
	}
	else
	{
		xcb_present_notify_msc(connection, id, serial, 0, async ? 0 : CONFIG->anim_divider, 0);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_rename(xcb_window_t id, const char *name)
{
	size_t n;

	if (!name)
	{
		name = class_name;
	}
	n = strlen(name);

	prop_set(id, atom_nam,  XCB_ATOM_STRING, n, name);
	prop_set(id, atom_ico,  XCB_ATOM_STRING, n, name);
	prop_set(id, atom_nnam, atom_utf8,       n, name);
	prop_set(id, atom_nnam, atom_utf8,       n, name);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_resize(xcb_window_t id, double width, double height)
{
	test_cookie(
		xcb_configure_window_checked(
			connection,
			id,
			XCB_CONFIG_WINDOW_WIDTH | XCB_CONFIG_WINDOW_HEIGHT,
			(uint32_t[2]){TO_UINT(width), TO_UINT(height)}));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_set_accel(xcb_window_t id, int accel, const char *name)
{
	test_cookie(xcb_delete_property_checked(connection, id, atom_aclx[accel]));
	if (name)
	{
		prop_set(id, atom_aclx[accel], atom_utf8, strlen(name), name);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_set_transient(xcb_window_t id, xcb_window_t id_under)
{
	prop_set(id, XCB_ATOM_WM_TRANSIENT_FOR, XCB_ATOM_WINDOW, 1, &id_under);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_set_type(xcb_window_t id, enum cgui_window_type type)
{
	xcb_atom_t atom;

	switch (type)
	{
		case CGUI_WINDOW_NORMAL:
			atom = atom_wnom;
			break;

		case CGUI_WINDOW_DIALOG:
			atom = atom_wdlg;
			break;

		case CGUI_WINDOW_DESKTOP:
			atom = atom_wdsk;
			break;

		case CGUI_WINDOW_OVERLAY:
			atom = atom_wovr;
			break;

		default:
			return;
	}

	prop_set(id, atom_wtyp, XCB_ATOM_ATOM, 1, &atom);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_set_urgency(xcb_window_t id, bool set_on)
{
	xcb_icccm_wm_hints_t *xhints;
	xcb_get_property_reply_t *xr;
	xcb_get_property_cookie_t xc;

	xc = xcb_get_property(connection, 0, id, XCB_ATOM_WM_HINTS, XCB_ATOM_WM_HINTS, 0, UINT32_MAX);
	xr = xcb_get_property_reply(connection, xc, NULL);
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

	prop_set(id, XCB_ATOM_WM_HINTS, XCB_ATOM_WM_HINTS, sizeof(xcb_icccm_wm_hints_t), xhints);

	free(xr);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_toggle_fullscreen(xcb_window_t id)
{
	xcb_client_message_event_t event =
	{
		.response_type = XCB_CLIENT_MESSAGE,
		.type          = atom_nstt,
		.format        = 32,
		.window        = id,
		.data          =
		{
			.data32[0] = 2,
			.data32[1] = atom_full,
			.data32[2] = XCB_ATOM_NONE,
			.data32[3] = 1,
		},
	};

	xcb_send_event(connection, 1, screen->root, XCB_EVENT_MASK_SUBSTRUCTURE_NOTIFY, (char*)&event);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_update_buffer(xcb_window_t id, xcb_pixmap_t *buffer, double width, double height)
{
	test_cookie(xcb_free_pixmap_checked(connection, *buffer));

	*buffer = xcb_generate_id(connection);

	test_cookie(xcb_create_pixmap_checked(connection, depth->depth, *buffer, id, width, height));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_update_focus_hints(xcb_window_t id, double x, double y, double width, double height)
{
	uint32_t geometry[4] =
	{
		TO_UINT(x),
		TO_UINT(y),
		TO_UINT(width),
		TO_UINT(height),
	};
	
	if (geometry[2] == 0 && geometry[3] == 0)
	{
		test_cookie(xcb_delete_property_checked(connection, id, atom_wfoc));
	}
	else
	{
		prop_set(id, atom_wfoc, XCB_ATOM_CARDINAL, 4, geometry);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_update_size_hints(xcb_window_t id, double min_width, double min_height, double max_width, double max_height)
{
	const xcb_size_hints_t xhints =
	{
		.flags      = XCB_ICCCM_SIZE_HINT_P_MIN_SIZE | XCB_ICCCM_SIZE_HINT_P_MAX_SIZE,
		.min_width  = TO_UINT(min_width),
		.min_height = TO_UINT(min_height),
		.max_width  = TO_UINT(max_width),
		.max_height = TO_UINT(max_height),
	};

	prop_set(id, XCB_ATOM_WM_NORMAL_HINTS, XCB_ATOM_WM_SIZE_HINTS, sizeof(xcb_size_hints_t), &xhints);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_update_state_hints(xcb_window_t id, struct cgui_window_state_flags state)
{
	test_cookie(xcb_delete_property_checked(connection, id, atom_stt));

	if (state.active)
	{
		prop_add(id, atom_stt, XCB_ATOM_ATOM, 1, &atom_won);
	}

	if (state.disabled)
	{
		prop_add(id, atom_stt, XCB_ATOM_ATOM, 1, &atom_wena);
	}

	if (state.locked_grid)
	{
		prop_add(id, atom_stt, XCB_ATOM_ATOM, 1, &atom_plck);
	}

	if (state.locked_focus)
	{
		prop_add(id, atom_stt, XCB_ATOM_ATOM, 1, &atom_flck);
	}
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
event_button(xcb_button_press_event_t *xcb_event, bool press)
{
	struct cgui_event event =
	{
		.type        = press ? CGUI_EVENT_BUTTON_PRESS : CGUI_EVENT_BUTTON_RELEASE,
		.window      = find_window(xcb_event->event),
		.button_id   = xcb_event->detail,
		.button_x    = xcb_event->event_x,
		.button_y    = xcb_event->event_y,
		.button_mods = translate_mods(xcb_event->state),
	};

	main_update(&event);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
event_client_message(xcb_client_message_event_t *xcb_event)
{
	struct cgui_event event = {0};

	const xcb_atom_t a1 = xcb_event->data.data32[0];
	const xcb_atom_t a2 = xcb_event->data.data32[1];
	const xcb_atom_t a3 = xcb_event->data.data32[2];

	if (xcb_event->type != atom_prot)
	{
		return;
	}

	/* window independent message */

	if (a1 == atom_sig && a2 == atom_conf)
	{
		event.type   = CGUI_EVENT_RECONFIG;
		event.window = CGUI_WINDOW_PLACEHOLDER;
		goto update;
	}

	/* window dependent messages */

	if (a1 == atom_sig && a2 == atom_acl)
	{
		event.type        = CGUI_EVENT_ACCELERATOR;
		event.window      = find_window(xcb_event->window);
		event.accelerator = a3;
		goto update;
	}

	if (a1 == atom_del)
	{
		event.type   = CGUI_EVENT_CLOSE;
		event.window = find_window(xcb_event->window);
		goto update;
	}

	if (a1 == atom_foc)
	{
		xcb_set_input_focus(connection, XCB_INPUT_FOCUS_PARENT, xcb_event->window, XCB_CURRENT_TIME);
		return;
	}

	if (a1 == atom_ping)
	{
		xcb_event->window = screen->root;
		xcb_send_event(connection, 0, screen->root, XCB_EVENT_MASK_NO_EVENT, (char*)xcb_event);
		xcb_flush(connection);
		return;
	}

	/* send CGUI event */

update:

	main_update(&event);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
event_configure(xcb_configure_notify_event_t *xcb_event)
{
	struct cgui_event event =
	{
		.type             = CGUI_EVENT_TRANSFORM,
		.window           = find_window(xcb_event->window),
		.transform_x      = xcb_event->x,
		.transform_y      = xcb_event->y,
		.transform_width  = xcb_event->width,
		.transform_height = xcb_event->height,
	};

	main_update(&event);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
event_enter(xcb_enter_notify_event_t *xcb_event)
{
	struct cgui_event event = 
	{
		.type   = CGUI_EVENT_ENTER,
		.window = find_window(xcb_event->event),
	};

	main_update(&event);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
event_expose(xcb_expose_event_t *xcb_event)
{
	struct cgui_event event =
	{
		.type   = CGUI_EVENT_REDRAW,
		.window = find_window(xcb_event->window),
	};

	if (xcb_event->count == 0)
	{
		main_update(&event);
	};
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
event_focus_in(xcb_focus_in_event_t *xcb_event)
{
	struct cgui_event event =
	{
		.type   = CGUI_EVENT_FOCUS,
		.window = find_window(xcb_event->event),
	};

	main_update(&event);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
event_focus_out(xcb_focus_out_event_t *xcb_event)
{
	struct cgui_event event =
	{
		.type   = CGUI_EVENT_UNFOCUS,
		.window = find_window(xcb_event->event),
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
event_key(xcb_key_press_event_t *xcb_event, bool press)
{
	struct cgui_event event =
	{
		.type      = press ? CGUI_EVENT_KEY_PRESS : CGUI_EVENT_KEY_RELEASE,
		.window    = find_window(xcb_event->event),
		.key_code  = xcb_event->detail,
		.key_mods  = translate_mods(xcb_event->state),
	};

	main_update(&event);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
event_keymap(xcb_mapping_notify_event_t *xcb_event)
{
	xcb_refresh_keyboard_mapping(keysyms, xcb_event);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
event_leave(xcb_leave_notify_event_t *xcb_event)
{
	struct cgui_event event = 
	{
		.type   = CGUI_EVENT_LEAVE,
		.window = find_window(xcb_event->event),
	};

	main_update(&event);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
event_map(xcb_map_notify_event_t *xcb_event)
{
	struct cgui_event event =
	{
		.type   = CGUI_EVENT_MAP,
		.window = find_window(xcb_event->window),
	};

	main_update(&event);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
event_motion(xcb_motion_notify_event_t *xcb_event)
{
	struct cgui_event event =
	{
		.type      = CGUI_EVENT_POINTER_MOTION,
		.window    = find_window(xcb_event->event),
		.pointer_x = xcb_event->event_x,
		.pointer_y = xcb_event->event_y,
	};

	main_update(&event);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
event_present(xcb_present_generic_event_t *xcb_event)
{
	xcb_present_complete_notify_event_t *present = (xcb_present_complete_notify_event_t*)xcb_event;
	xcb_present_complete_kind_t type = CONFIG->alt_present ?
		XCB_PRESENT_COMPLETE_KIND_PIXMAP : XCB_PRESENT_COMPLETE_KIND_NOTIFY_MSC;

	struct cgui_event event =
	{
		.type   = CGUI_EVENT_PRESENT,
		.window = find_window(present->window),
	};

	if (xcb_event->evtype == XCB_PRESENT_EVENT_COMPLETE_NOTIFY
	 && present->serial   == event.window->x_serial
	 && present->kind     == type)
	{
		main_update(&event);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
event_selection_clear(xcb_selection_clear_event_t *xcb_event)
{
	struct cgui_event event =
	{
		.type   = CGUI_EVENT_NONE,
		.window = CGUI_WINDOW_PLACEHOLDER,
	};

	(void)xcb_event;

	// TODO

	main_update(&event);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
event_selection_request(xcb_selection_request_event_t *xcb_event)
{
	struct cgui_event event =
	{
		.type   = CGUI_EVENT_NONE,
		.window = CGUI_WINDOW_PLACEHOLDER,
	};

	(void)xcb_event;

	// TODO

	main_update(&event);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
event_unknown(xcb_generic_event_t *xcb_event)
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
event_unmap(xcb_unmap_notify_event_t *xcb_event)
{
	struct cgui_event event =
	{
		.type   = CGUI_EVENT_UNMAP,
		.window = find_window(xcb_event->window),
	};

	main_update(&event);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
event_xinput_touch(xcb_input_touch_begin_event_t *xcb_event)
{
	struct cgui_event event =
	{
		.window   = find_window(xcb_event->event),
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
find_window(xcb_window_t id)
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
get_atom(const char *name)
{
	xcb_intern_atom_cookie_t xc;
	xcb_intern_atom_reply_t *xr;
	xcb_atom_t xa;

	xc = xcb_intern_atom(connection, 0, strlen(name), name);
	xr = xcb_intern_atom_reply(connection, xc, NULL);
	if (!xr)
	{
		main_set_error(CERR_XCB);
		return 0;
	}

	xa = xr->atom;
	free(xr);

	return xa;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static uint8_t
get_extension_opcode(const char *name)
{
	xcb_query_extension_cookie_t xc;
	xcb_query_extension_reply_t *xr;

	uint8_t opcode;

	xc = xcb_query_extension(connection, strlen(name), name);
	xr = xcb_query_extension_reply(connection, xc, NULL);
	if (!xr)
	{
		main_set_error(CERR_XCB);
		return 0;
	}

	opcode = xr->major_opcode;
	free(xr);

	return opcode;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
prop_add(xcb_window_t win, xcb_atom_t prop, xcb_atom_t type, uint32_t data_n, const void *data)
{
	xcb_void_cookie_t xc;

	xc = xcb_change_property_checked(
		connection,
		XCB_PROP_MODE_APPEND,
		win,
		prop,
		type,
		type == atom_utf8 || type == atom_time || type == XCB_ATOM_STRING ? 8 : 32,
		data_n,
		data);

	return test_cookie(xc);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
prop_set(xcb_window_t win, xcb_atom_t prop, xcb_atom_t type, uint32_t data_n, const void *data)
{
	xcb_void_cookie_t xc;

	xc = xcb_change_property_checked(
		connection,
		XCB_PROP_MODE_REPLACE,
		win,
		prop,
		type,
		type == atom_utf8 || type == atom_time || type == XCB_ATOM_STRING ? 8 : 32,
		data_n,
		data);

	return test_cookie(xc);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
test_cookie(xcb_void_cookie_t xc)
{
	xcb_generic_error_t *x_err;

	if ((x_err = xcb_request_check(connection, xc)))
	{
		main_set_error(CERR_XCB);
		free(x_err);
		return false;
	}

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static struct cgui_mods
translate_mods(uint16_t state)
{
	struct cgui_mods mods =
	{
		.capslock = state & XCB_MOD_MASK_LOCK,
		.shift    = state & XCB_MOD_MASK_SHIFT,
		.ctrl     = state & XCB_MOD_MASK_CONTROL,
		.mod_1    = state & XCB_MOD_MASK_1,
		.mod_2    = state & XCB_MOD_MASK_2,
		.mod_3    = state & XCB_MOD_MASK_3,
		.mod_4    = state & XCB_MOD_MASK_4,
		.mod_5    = state & XCB_MOD_MASK_5,
	};

	return mods;
}

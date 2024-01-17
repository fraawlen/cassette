/**
 * Copyright © 2024 Frawwlen <fraawlen@posteo.net>
 *
 * This file is part of the Derelict Graphics (DG) GUI library.
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
#include <math.h>
#include <stdbool.h>
#include <pthread.h>
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include <cairo/cairo.h>
#include <cairo/cairo-xcb.h>
#include <fontconfig/fontconfig.h>
#include <xcb/xcb.h>
#include <xcb/xcb_icccm.h>
#include <xcb/xcb_keysyms.h>
#include <xcb/present.h>
#include <xcb/randr.h>
#include <xcb/xinput.h>
#include <xkbcommon/xkbcommon.h>

#include "atom.h"
#include "config.h"
#include "config-private.h"
#include "core.h"
#include "errno.h"
#include "input_buffer.h"
#include "stack.h"
#include "util.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/* macros for common parameter checking */

#define _IS_INIT      assert(_init);
#define _IS_WINDOW(X) assert(dg_core_stack_find(&_windows, X, &X->id));
#define _IS_GRID(X)   assert(dg_core_stack_find(&_grids,   X, &X->id));
#define _IS_CELL(X)   assert(dg_core_stack_find(&_cells,   X, &X->id));

/* macros for running callbacks */

#define _RUN_FN(X, ...)        if (X)  {X(__VA_ARGS__);}
#define _RUN_INT_FN(X, Y, ...) if (X && X(__VA_ARGS__)) {Y;}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

typedef enum {
	_FOCUS_SEEK_VERT,
	_FOCUS_SEEK_HORZ,
	_FOCUS_SEEK_CLOSE =  1,
	_FOCUS_SEEK_FAR   = -1,
	_FOCUS_SEEK_DIR_A =  1,
	_FOCUS_SEEK_DIR_B = -1,
} _focus_seek_param_t;

typedef enum {
	_WINDOW_RENDER_NONE,
	_WINDOW_RENDER_AREAS,
	_WINDOW_RENDER_BORDER,
	_WINDOW_RENDER_BOTH, /* border + areas              */
	_WINDOW_RENDER_FULL, /* border + areas + background */
} _window_render_level_t;

typedef enum {
	_WINDOW_PRESENT_NONE,
	_WINDOW_PRESENT_DEFAULT,
	_WINDOW_PRESENT_IMMEDIATE,
} _window_present_schedule_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/* internal structs */

typedef struct _popup_t _popup_t;
struct _popup_t {
	int16_t px, py;
	int16_t pw, ph;
	_popup_t *p_parent;
	_popup_t *p_child;
	dg_core_window_t *w;
};

typedef struct {
	int16_t x, y, w, h;
} _rect_t;

typedef struct {
	const char *name;
	void (*fn)(dg_core_window_t *w, int accel_id);
} _accel_t;

typedef struct {
	size_t id;
	bool redraw;
	dg_core_cell_t *c;
	dg_core_grid_t *g_parent;
	dg_core_input_buffer_t touches;
	int16_t cx, cy, cw, ch;
	int16_t px, py, pw, ph;
} _area_t;

typedef struct {
	_area_t **areas;
	size_t n;
} _area_list_t;

typedef struct {
	xcb_timestamp_t time;
	bool owned;
	void *data;
	size_t data_n;
	void (*fn_copy)(int clipboard, dg_core_cell_t *c);
	void (*fn_lost)(int clipboard, dg_core_cell_t *c);
	dg_core_cell_t *c_signal;
} _selection_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/* api structs */

struct _cell_t {
	unsigned int serial;
	size_t id;
	bool to_destroy;
	bool ena;
	void *props;
	void (*fn_draw)(dg_core_cell_t *c, dg_core_cell_drawing_context_t *dc);
	void (*fn_event)(dg_core_cell_t *c, dg_core_cell_event_t *ev);
	void (*fn_destroy)(dg_core_cell_t *c);
};

struct _grid_t {
	size_t id;
	bool to_destroy;
	bool used;
	dg_core_stack_t areas;
	int16_t cw, ch;
	int16_t pw, ph;
	int16_t *chu;
	int16_t *cwu;
	double  *fhu;
	double  *fwu;
	double  n_fhu;
	double  n_fwu;
	int16_t n_chu, n_chu_inv;
	int16_t n_cwu, n_cwu_inv;
	dg_core_grid_t *g_ref;
};

struct _window_t {
	size_t id;
	bool to_destroy;
	/* visual update data */
	_window_render_level_t render_level;
	_window_present_schedule_t present_schedule;
	uint32_t present_serial;
	unsigned long last_render_time;
	/* input trackers */
	dg_core_input_buffer_t buttons;
	dg_core_input_buffer_t touches;
	/* visual elements */
	xcb_window_t x_win;
	cairo_t *c_ctx;
	cairo_surface_t *c_srf;
	/* base properties */
	const char *name;
	const char *name_icon;
	dg_core_window_state_t state;
	bool fixed;
	/* content */
	dg_core_stack_t grids;
	dg_core_grid_t *g_current;
	_popup_t *p_container;
	_area_t *a_focus;
	_accel_t accels[DG_CORE_CONFIG_MAX_ACCELS];
	/* geometry */
	int16_t px, py;
	int16_t pw, ph;
	int16_t cw_extra, ch_extra;
	/* callback functions */
	void (*callback_close)(dg_core_window_t *w);
	void (*callback_focus)(dg_core_window_t *w, dg_core_cell_t *c);
	void (*callback_grid)(dg_core_window_t *w, dg_core_grid_t *p);
	void (*callback_state)(dg_core_window_t *w, dg_core_window_state_t state, dg_core_window_state_setting_mode_t mode);
	void (*callback_redraw)(dg_core_window_t *w, unsigned long delay);
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/* X helpers */

static xcb_atom_t      _x_get_atom                (const char *name);
static xcb_atom_t      _x_get_atom_sel            (int selection);
static xcb_atom_t      _x_get_atom_sel_target     (int selection);
static uint8_t         _x_get_extension_opcode    (const char *name);
static _rect_t         _x_get_monitor_geometry_at (int16_t px, int16_t py);
static int             _x_get_selection_id        (xcb_atom_t xa);
static xcb_timestamp_t _x_get_timestamp           (void);
static bool            _x_send_sel_data           (int selection, xcb_window_t requestor, xcb_atom_t prop, xcb_atom_t target);
static void            _x_set_prop                (bool append, xcb_window_t win, xcb_atom_t prop, xcb_atom_t type, uint32_t data_n, const void *data);
static bool            _x_test_cookie             (xcb_void_cookie_t xc, bool log);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/* event handlers */

static void _event_client_message    (xcb_client_message_event_t *x_ev);
static void _event_configure         (xcb_configure_notify_event_t *x_ev);
static void _event_core_input        (xcb_key_press_event_t *x_ev);
static void _event_expose            (xcb_expose_event_t *x_ev);
static void _event_focus_in          (xcb_focus_in_event_t *x_ev);
static void _event_focus_out         (xcb_focus_in_event_t *x_ev);
static void _event_keymap            (xcb_mapping_notify_event_t *x_ev);
static void _event_leave             (xcb_leave_notify_event_t *x_ev);
static void _event_map               (xcb_map_notify_event_t *x_ev);
static void _event_motion            (xcb_motion_notify_event_t *x_ev);
static void _event_present           (xcb_present_generic_event_t *x_ev);
static void _event_selection_clear   (xcb_selection_clear_event_t *x_ev);
static void _event_selection_request (xcb_selection_request_event_t *x_ev);
static void _event_unmap             (xcb_unmap_notify_event_t *x_ev);
static void _event_visibility        (xcb_visibility_notify_event_t *x_ev);
static void _event_xinput_touch      (xcb_input_touch_begin_event_t *x_ev);

static void _sub_event_action_cell   (dg_core_window_t *w, dg_core_config_action_t action);
static void _sub_event_action_focus  (dg_core_window_t *w, dg_core_config_action_t action);
static void _sub_event_action_misc   (dg_core_config_action_t action);
static void _sub_event_action_window (dg_core_window_t *w, dg_core_config_action_t action);
static void _sub_event_clipboard     (dg_core_window_t *w, uint8_t type, uint8_t clipboard);
static void _sub_event_input_button  (dg_core_window_t *w, uint8_t button,  xcb_key_press_event_t *x_ev);
static void _sub_event_input_key     (dg_core_window_t *w, uint8_t keycode, xcb_key_press_event_t *x_ev);
static void _sub_event_touch_begin   (dg_core_window_t *w, uint32_t id, int16_t px, int16_t py);
static void _sub_event_touch_end     (dg_core_window_t *w, uint32_t id, int16_t px, int16_t py);
static void _sub_event_touch_update  (dg_core_window_t *w, uint32_t id, int16_t px, int16_t py);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/* procedures with side effects */

static void _area_redraw             (_area_t *a, dg_core_window_t *w, unsigned long delay); 
static void _area_update_geometry    (_area_t *a, dg_core_grid_t  *g, bool is_popup);
static void _cell_destroy            (dg_core_cell_t *c);
static bool _cell_process_bare_event (dg_core_cell_t *c, dg_core_cell_event_t *cev);
static void _clipboard_clear         (int clipboard);
static void _grid_destroy            (dg_core_grid_t *g);
static void _grid_update_geometry    (dg_core_grid_t *g, bool is_popup);
static void _misc_reconfig           (void);
static bool _popup_grab_inputs       (void);
static void _popup_ungrab_inputs     (void);
static void _popup_kill              (_popup_t *p);

static void _window_destroy               (dg_core_window_t *w);
static void _window_focus_by_pointer      (dg_core_window_t *w, int16_t px, int16_t py);
static void _window_present               (dg_core_window_t *w);
static bool _window_process_cell_event    (dg_core_window_t *w, _area_t *a, dg_core_cell_event_t *cev);
static void _window_redraw                (dg_core_window_t *w);
static void _window_refocus               (dg_core_window_t *w);
static void _window_resize                (dg_core_window_t *w, int16_t pw, int16_t ph);
static void _window_send_event_to_all     (dg_core_window_t *w, dg_core_cell_event_t *cev);
static void _window_set_focus             (dg_core_window_t *w, _area_t *a);
static void _window_set_focus_lock        (dg_core_window_t *w, bool lock);
static void _window_set_present_schedule  (dg_core_window_t *w, _window_present_schedule_t present_schedule);
static void _window_set_render_level      (dg_core_window_t *w, _window_render_level_t render_level);
static void _window_set_state             (dg_core_window_t *w, dg_core_window_state_t state, dg_core_window_state_setting_mode_t mode);
static void _window_toggle_state          (dg_core_window_t *w, dg_core_window_state_t state_bits);
static void _window_update_current_grid   (dg_core_window_t *w);
static void _window_update_geometries     (dg_core_window_t *w, bool is_popup);
static void _window_update_wm_focus_hints (dg_core_window_t *w);
static void _window_update_wm_size_hints  (dg_core_window_t *w);

static dg_core_window_t *_popup_prep_core_input   (xcb_key_press_event_t *x_ev);
static dg_core_window_t *_popup_prep_motion_input (xcb_motion_notify_event_t *x_ev);
static dg_core_window_t *_window_create           (bool fixed);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/* pure functions */

static _rect_t               _area_get_current_geometry (_area_t *a, dg_core_window_t *w);
static dg_core_cell_focus_t  _area_get_focus_type       (_area_t *a, dg_core_window_t *w);
static _area_t              *_grid_find_first_area      (dg_core_grid_t *g, dg_core_cell_t *c);
static _area_list_t          _grid_get_neighbour_areas  (dg_core_grid_t *g, _area_t *a);
static dg_core_window_t     *_loop_find_window          (xcb_window_t x_win);
static bool                  _loop_no_active_windows    (void);
static _rect_t               _popup_get_geometry        (dg_core_window_t *w_ref, int16_t px, int16_t px_alt, int16_t py_alt, int16_t py, int16_t pw, int16_t ph);
static _popup_t             *_popup_find_under_coords   (int16_t px, int16_t py);

static _area_t         *_window_find_area_under_coords (dg_core_window_t *w, int16_t px, int16_t py);
static dg_core_grid_t  *_window_find_smallest_grid     (dg_core_window_t *w);
static dg_core_color_t  _window_get_border_color       (dg_core_window_t *w);
static _area_t         *_window_seek_focus_ortho       (dg_core_window_t *w, _area_t *a_start, _focus_seek_param_t axis, _focus_seek_param_t side, _focus_seek_param_t dir);
static _area_t         *_window_seek_focus_logic       (dg_core_window_t *w, _area_t *a_start, _focus_seek_param_t dir);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/* xcb globals */

static xcb_connection_t  *_x_con   = NULL;
static xcb_screen_t      *_x_scr   = NULL;
static xcb_depth_t       *_x_dph   = NULL;
static xcb_visualtype_t  *_x_vis   = NULL;
static xcb_key_symbols_t *_x_ksm   = NULL;
static xcb_colormap_t     _x_clm   = 0;
static xcb_window_t       _x_win_l = 0; /* leader window id */

/* program startup args for ICCCM properties */

static char  const *_class[2] = {NULL, NULL};
static char *const *_argv     = NULL;
static int          _argc     = 0;

/* public elements instances tracking and event buffer */

static unsigned int _serial = 0;

static dg_core_stack_t _windows = {.ptr = NULL, .n = 0, .n_alloc = 0};
static dg_core_stack_t _grids   = {.ptr = NULL, .n = 0, .n_alloc = 0};
static dg_core_stack_t _cells   = {.ptr = NULL, .n = 0, .n_alloc = 0};
static dg_core_stack_t _events  = {.ptr = NULL, .n = 0, .n_alloc = 0};

/* popup tracking */

static _popup_t *_p_last  = NULL;
static _popup_t *_p_hover = NULL;

/* session states */

static bool _ext_x = false;
static bool _init  = false;
static bool _loop  = false;

static bool _allow_user_exit = true;

/* user provided event handlers */

static void (*_fn_event_postprocessor) (xcb_generic_event_t *x_ev) = NULL;
static bool (*_fn_event_preprocessor)  (xcb_generic_event_t *x_ev) = NULL;

/* loop signal callback */

static void (*_fn_callback_loop_signal) (uint32_t serial) = NULL;

/* common X atoms */

static xcb_atom_t _xa_clip = 0; /* "CLIPBOARD"         */
static xcb_atom_t _xa_time = 0; /* "TIMESTAMP"         */
static xcb_atom_t _xa_mult = 0; /* "MULTIPLE"          */
static xcb_atom_t _xa_trgt = 0; /* "TARGETS"           */
static xcb_atom_t _xa_utf8 = 0; /* "UTF8_STRING"       */
static xcb_atom_t _xa_prot = 0; /* "WM_PROTOCOLS"      */
static xcb_atom_t _xa_del  = 0; /* "WM_DELETE_WINDOW"  */ 
static xcb_atom_t _xa_foc  = 0; /* "WM_TAKE_FOCUS"     */
static xcb_atom_t _xa_nam  = 0; /* "WM_NAME"           */
static xcb_atom_t _xa_ico  = 0; /* "WM_ICON_MANE"      */
static xcb_atom_t _xa_cls  = 0; /* "WM_CLASS"          */
static xcb_atom_t _xa_cmd  = 0; /* "WM_COMMAND"        */
static xcb_atom_t _xa_host = 0; /* "WM_CLIENT_MACHINE" */
static xcb_atom_t _xa_lead = 0; /* "WM_CLIENT_LEADER"  */
static xcb_atom_t _xa_ping = 0; /* "_NET_WM_PING"      */
static xcb_atom_t _xa_pid  = 0; /* "_NET_WM_PID"       */
static xcb_atom_t _xa_nnam = 0; /* "_NET_WM_NAME"      */
static xcb_atom_t _xa_nico = 0; /* "_NET_WM_ICON_NAME" */

/* DG custom atoms */

static xcb_atom_t _xa_sig  = 0; /* DG_CORE_ATOM_SIGNALS           */
static xcb_atom_t _xa_vers = 0; /* DG_CORE_ATOM_VERSION           */
static xcb_atom_t _xa_stt  = 0; /* DG_CORE_ATOM_WINDOW_STATES     */
static xcb_atom_t _xa_dfoc = 0; /* DG_CORE_ATOM_WINDOW_FOCUS      */
static xcb_atom_t _xa_tmp1 = 0; /* DG_CORE_ATOM_PASTE_TMP_1       */
static xcb_atom_t _xa_tmp2 = 0; /* DG_CORE_ATOM_PASTE_TMP_2       */
static xcb_atom_t _xa_tmp3 = 0; /* DG_CORE_ATOM_PASTE_TMP_3       */
static xcb_atom_t _xa_won  = 0; /* DG_CORE_ATOM_WINDOW_ACTIVE     */
static xcb_atom_t _xa_wena = 0; /* DG_CORE_ATOM_WINDOW_DISABLED   */
static xcb_atom_t _xa_plck = 0; /* DG_CORE_ATOM_WINDOW_GRID_LOCK  */
static xcb_atom_t _xa_flck = 0; /* DG_CORE_ATOM_WINDOW_FOCUS_LOCK */
static xcb_atom_t _xa_conf = 0; /* DG_CORE_ATOM_RECONFIG          */
static xcb_atom_t _xa_acl  = 0; /* DG_CORE_ATOM_ACCEL             */

static xcb_atom_t _xa_isig = 0;                              /* "_INTERNAL_LOOP_SIGNAL"        */
static xcb_atom_t _xa_aclx[DG_CORE_CONFIG_MAX_ACCELS] = {0}; /* "_DG_WINDOW_ACCEL_x" x = 1..12 */

/* extensions op codes */

static uint8_t _x_opc_present = 0;
static uint8_t _x_opc_xinput  = 0;

/* selection data, respectively : clipboard, primary and secondary */

static _selection_t _sel[3]         = {0};
static xcb_atom_t   _sel_targets[4] = {0};

/************************************************************************************************************/
/* PUBLIC - MAIN ********************************************************************************************/
/************************************************************************************************************/

unsigned int
dg_core_get_serial(void)
{
	_IS_INIT;
	
	return ++_serial;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

xcb_connection_t *
dg_core_get_xcb_connection(void)
{
	return _x_con;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

xcb_window_t
dg_core_get_xcb_leader_window(void)
{
	return _x_win_l;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_init(int argc, char *const *argv, const char *class_name, const char *class_class,
             xcb_connection_t *connection)
{
	assert(!_init);

	xcb_void_cookie_t xc;

	/* init structs */

	dg_core_stack_init(&_windows, 0);
	dg_core_stack_init(&_grids,   0);
	dg_core_stack_init(&_cells,   0);
	dg_core_stack_init(&_events,  0);

	/* load config */

	if (!dg_core_config_init()) {
		return;
	}

	/* setup class and cmd args */

	int offset = 0;

	if (argv && strlen(argv[0]) > 2 && argv[0][0] == '.' && argv[0][1] == '/') {
		offset += 2;
	}
	_class[1] = class_class ? class_class : (argv ? argv[0] + offset : "dg");
	_class[0] = class_name  ? class_name  : _class[1];

	_argc = argc;
	_argv = argv;

	/* setup x connection */

	if (connection) {
		_x_con = connection;
		_ext_x = true;
	} else {
		_x_con = xcb_connect(NULL, NULL);
		if (!_x_con) {
			goto err_crit;
		}
	}

	/* find x screen */

	_x_scr = xcb_setup_roots_iterator(xcb_get_setup(_x_con)).data;
	if (!_x_scr) {
		goto err_crit;
	}

	/* get x depth */

	xcb_depth_iterator_t x_dph_it = xcb_screen_allowed_depths_iterator(_x_scr);

	for (; x_dph_it.rem; xcb_depth_next(&x_dph_it)) {
		if (x_dph_it.data->depth == 32) {
			_x_dph = x_dph_it.data;
			break;
		}
	}
	if (!_x_dph) {
		goto err_crit;
	}
	
	/* get x visual type */

	xcb_visualtype_iterator_t x_vis_it = xcb_depth_visuals_iterator(_x_dph);

	for (; x_vis_it.rem; xcb_visualtype_next(&x_vis_it)) {
		if (x_vis_it.data->_class == XCB_VISUAL_CLASS_TRUE_COLOR) {
			_x_vis = x_vis_it.data;
			break;
		}
	}
	if (!_x_vis) {
		goto err_crit;
	}

	/* get keysym table */

	_x_ksm = xcb_key_symbols_alloc(_x_con);
	if (!_x_ksm) {
		goto err_crit;
	}

	/* get atoms */

	_xa_clip = _x_get_atom("CLIPBOARD");
	_xa_time = _x_get_atom("TIMESTAMP");
	_xa_mult = _x_get_atom("MULTIPLE");
	_xa_trgt = _x_get_atom("TARGETS");
	_xa_utf8 = _x_get_atom("UTF8_STRING");
	_xa_prot = _x_get_atom("WM_PROTOCOLS");
	_xa_del  = _x_get_atom("WM_DELETE_WINDOW");
	_xa_foc  = _x_get_atom("WM_TAKE_FOCUS");
	_xa_nam  = _x_get_atom("WM_NAME");
	_xa_ico  = _x_get_atom("WM_ICON_NAME");
	_xa_cls  = _x_get_atom("WM_CLASS");
	_xa_cmd  = _x_get_atom("WM_COMMAND");
	_xa_host = _x_get_atom("WM_CLIENT_MACHINE");
	_xa_lead = _x_get_atom("WM_CLIENT_LEADER");
	_xa_ping = _x_get_atom("_NET_WM_PING");
	_xa_pid  = _x_get_atom("_NET_WM_PID");
	_xa_nnam = _x_get_atom("_NET_WM_NAME");
	_xa_nico = _x_get_atom("_NET_WM_ICON_NAME");
	_xa_isig = _x_get_atom("_INTERNAL_LOOP_SIGNAL");
	_xa_sig  = _x_get_atom(DG_CORE_ATOM_SIGNALS);
	_xa_vers = _x_get_atom(DG_CORE_ATOM_VERSION);
	_xa_stt  = _x_get_atom(DG_CORE_ATOM_WINDOW_STATES);
	_xa_dfoc = _x_get_atom(DG_CORE_ATOM_WINDOW_FOCUS);
	_xa_tmp1 = _x_get_atom(DG_CORE_ATOM_PASTE_TMP_1);
	_xa_tmp2 = _x_get_atom(DG_CORE_ATOM_PASTE_TMP_2);
	_xa_tmp3 = _x_get_atom(DG_CORE_ATOM_PASTE_TMP_3);
	_xa_won  = _x_get_atom(DG_CORE_ATOM_WINDOW_ACTIVE);
	_xa_wena = _x_get_atom(DG_CORE_ATOM_WINDOW_DISABLED);
	_xa_plck = _x_get_atom(DG_CORE_ATOM_WINDOW_GRID_LOCK);
	_xa_flck = _x_get_atom(DG_CORE_ATOM_WINDOW_FOCUS_LOCK);
	_xa_conf = _x_get_atom(DG_CORE_ATOM_RECONFIG);
	_xa_acl  = _x_get_atom(DG_CORE_ATOM_ACCEL);

	char s[20];
	for (int i = 0; i < sizeof(_xa_aclx) / sizeof(xcb_atom_t); i++) {
		sprintf(s, DG_CORE_ATOM_ACCEL "_%i", i + 1);
		_xa_aclx[i] = _x_get_atom(s);
	}

	_sel_targets[0] = _xa_trgt;
	_sel_targets[1] = _xa_time;
	_sel_targets[2] = _xa_mult;
	_sel_targets[3] = _xa_utf8;

	/* get extensions opcodes */

	_x_opc_present = _x_get_extension_opcode("Present");
	_x_opc_xinput  = _x_get_extension_opcode("XInputExtension");

	/* get colormap to create transparent windows */

	_x_clm = xcb_generate_id(_x_con);
	xc = xcb_create_colormap_checked(
		_x_con,
		XCB_COLORMAP_ALLOC_NONE,
		_x_clm,
		_x_scr->root,
		_x_vis->visual_id);

	_x_test_cookie(xc, true);

	/* create leader window */

	const uint32_t mask_vals[] = {
		XCB_EVENT_MASK_SUBSTRUCTURE_NOTIFY   |
		XCB_EVENT_MASK_SUBSTRUCTURE_REDIRECT |
		XCB_EVENT_MASK_PROPERTY_CHANGE,
	};

	_x_win_l = xcb_generate_id(_x_con);
	xc = xcb_create_window_checked(
		_x_con,
		XCB_COPY_FROM_PARENT,
		_x_win_l,
		_x_scr->root,
		0, 0,
		1, 1,
		0,
		XCB_WINDOW_CLASS_INPUT_OUTPUT,
		_x_scr->root_visual,
		XCB_CW_EVENT_MASK,
		mask_vals);

	_x_test_cookie(xc, true);

	/* set leader window properties */

	const size_t vers_n = strlen(DG_CORE_VERSION);
	const size_t name_n = strlen(_class[0]);
	const size_t pid    = getpid();

	char   host[256] = "";
	size_t host_n;

	gethostname(host, 256);
	host_n = strlen(host);

	_x_set_prop(false, _x_win_l, _xa_lead, XCB_ATOM_WINDOW,   1,      &_x_win_l);
	_x_set_prop(false, _x_win_l, _xa_pid,  XCB_ATOM_CARDINAL, 1,      &pid);
	_x_set_prop(false, _x_win_l, _xa_nam,  XCB_ATOM_STRING,   host_n, host);
	_x_set_prop(false, _x_win_l, _xa_vers, XCB_ATOM_STRING,   vers_n, DG_CORE_VERSION);
	_x_set_prop(false, _x_win_l, _xa_nam,  XCB_ATOM_STRING,   name_n, _class[0]);
	_x_set_prop(false, _x_win_l, _xa_nnam, _xa_utf8,          name_n, _class[0]);
	_x_set_prop(false, _x_win_l, _xa_prot, XCB_ATOM_ATOM,     1,      &_xa_sig);
	for (size_t i = 0; i < argc; i++) {
		_x_set_prop(true, _x_win_l, _xa_cmd, XCB_ATOM_STRING, strlen(argv[i]) + 1, argv[i]);
	}

	/* end of initalisation */

	_init = true;	

	return;	

	/* errors */

err_crit:

	dg_core_errno_set(DG_CORE_ERRNO_XCB_CRIT);
	dg_core_reset();
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
dg_core_is_init(void)
{
	return _init;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_reconfig(void)
{
	_IS_INIT;

	_misc_reconfig();
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void 
dg_core_reset(void)
{
	/* disconnect from x server */
	
	if (_x_ksm) {
		xcb_key_symbols_free(_x_ksm);
	}

	if (_x_con) {
		xcb_destroy_window(_x_con, _x_win_l);
		xcb_free_colormap(_x_con, _x_clm);
		if (!_ext_x) {
			xcb_disconnect(_x_con);
		}
	}

	/* get rid of all tracked stuff */

	_serial  = 0;
	_p_last  = NULL;
	_p_hover = NULL;

	dg_core_stack_reset(&_windows);
	dg_core_stack_reset(&_grids);
	dg_core_stack_reset(&_cells);
	dg_core_stack_reset(&_events);

	/* reset the config */

	dg_core_config_reset();

	/* reset all global variables */

	_x_con   = NULL;
	_x_scr   = NULL;
	_x_dph   = NULL;
	_x_vis   = NULL;
	_x_ksm   = NULL;
	_x_clm   = 0;
	_x_win_l = 0;

	_class[0] = NULL;
	_class[1] = NULL;
	_argv     = NULL;
	_argc     = 0;

	_x_opc_present = 0;
	_x_opc_xinput  = 0;

	_ext_x = false;
	_loop  = false;

	_fn_event_postprocessor  = NULL;
	_fn_event_preprocessor   = NULL;
	_fn_callback_loop_signal = NULL;

	_xa_clip = 0;
	_xa_time = 0;
	_xa_mult = 0;
	_xa_trgt = 0;
	_xa_utf8 = 0;
	_xa_prot = 0;
	_xa_del  = 0;
	_xa_foc  = 0;
	_xa_nam  = 0;
	_xa_ico  = 0;
	_xa_cls  = 0;
	_xa_cmd  = 0;
	_xa_host = 0;
	_xa_lead = 0;
	_xa_ping = 0;
	_xa_pid  = 0;
	_xa_nnam = 0;
	_xa_nico = 0;
	_xa_isig = 0;
	_xa_sig  = 0;
	_xa_vers = 0;
	_xa_stt  = 0;
	_xa_dfoc = 0;
	_xa_tmp1 = 0;
	_xa_tmp2 = 0;
	_xa_tmp3 = 0;
	_xa_won  = 0;
	_xa_wena = 0;
	_xa_plck = 0;
	_xa_flck = 0;
	_xa_conf = 0;
	_xa_acl  = 0;

	for (int i = 0; i < sizeof(_xa_aclx) / sizeof(xcb_atom_t); i++) {
		_xa_aclx[i] = 0;
	}

	for (int i = 0; i < sizeof(_sel_targets) / sizeof(xcb_atom_t); i++) {
		_sel_targets[i] = 0;
	}

	/* reset selections */

	for (size_t i = 0; i < 3; i++) {
		_clipboard_clear(i);
	}

	/* extra cleanup for debugging */

	if (dg_core_util_test_env("DG_CORE_DEBUG")) {
		cairo_debug_reset_static_data();
		FcFini();
	}

	/* end */
	
	_init = false;
}

/************************************************************************************************************/
/* PUBLIC - LOOP ********************************************************************************************/
/************************************************************************************************************/

void
dg_core_loop_abort(void)
{
	_IS_INIT;

	_loop = false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_loop_allow_user_exit(void)
{
	_allow_user_exit = true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_loop_block_user_exit(void)
{
	_allow_user_exit = false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_loop_run(void)
{
	_IS_INIT;
	assert(!_loop);

	xcb_generic_event_t *x_ev;

	_loop = true;

	while (_init && _loop && !_loop_no_active_windows()) {

		/* grab next event from stack buffer if any, otherwhise retrieve new event from server */

		if (_events.n > 0) {
			x_ev = (xcb_generic_event_t*)_events.ptr[0];
			dg_core_stack_pull(&_events, x_ev);
		} else {
			x_ev = xcb_wait_for_event(_x_con);
			if (!x_ev) {
				dg_core_errno_set(DG_CORE_ERRNO_XCB);
				break;
			}
		}

		/* extra user's pre-event processor */

		_RUN_INT_FN(_fn_event_preprocessor, goto skip_events, x_ev);

		/* error events */

		if (x_ev->response_type == 0) {
			dg_core_errno_set(DG_CORE_ERRNO_XCB);
			goto skip_events;
		}

		/* built-in event processors */

		switch (x_ev->response_type & ~0x80) {

			/* can mix key and button_press events because their structs have the same fields */

			case XCB_BUTTON_PRESS:
			case XCB_BUTTON_RELEASE:
			case XCB_KEY_PRESS:
			case XCB_KEY_RELEASE:
				_event_core_input((xcb_key_press_event_t*)x_ev);
				break;

			case XCB_LEAVE_NOTIFY:
				_event_leave((xcb_leave_notify_event_t*)x_ev);
				break;

			case XCB_UNMAP_NOTIFY:
				_event_unmap((xcb_unmap_notify_event_t*)x_ev);
				break;

			case XCB_MAP_NOTIFY:
				_event_map((xcb_map_notify_event_t*)x_ev);
				break;

			case XCB_EXPOSE:
				_event_expose((xcb_expose_event_t*)x_ev);
				break;

			case XCB_MOTION_NOTIFY:
				_event_motion((xcb_motion_notify_event_t*)x_ev);
				break;

			case XCB_FOCUS_IN:
				_event_focus_in((xcb_focus_in_event_t*)x_ev);
				break;

			case XCB_FOCUS_OUT:
				_event_focus_out((xcb_focus_in_event_t*)x_ev);
				break;

			case XCB_CLIENT_MESSAGE:
				_event_client_message((xcb_client_message_event_t*)x_ev);
				break;

			case XCB_CONFIGURE_NOTIFY:
				_event_configure((xcb_configure_notify_event_t*)x_ev);
				break;

			case XCB_VISIBILITY_NOTIFY:
				_event_visibility((xcb_visibility_notify_event_t*)x_ev);
				break;

			case XCB_MAPPING_NOTIFY:
				_event_keymap((xcb_mapping_notify_event_t*)x_ev);
				break;

			case XCB_SELECTION_CLEAR:
				_event_selection_clear((xcb_selection_clear_event_t*)x_ev);
				break;

			case XCB_SELECTION_REQUEST:
				_event_selection_request((xcb_selection_request_event_t*)x_ev);
				break;

			case XCB_GE_GENERIC:
				if (((xcb_ge_generic_event_t*)x_ev)->extension == _x_opc_present) {
					_event_present((xcb_present_generic_event_t*)x_ev);
				} else if (((xcb_ge_generic_event_t*)x_ev)->extension == _x_opc_xinput) {
					_event_xinput_touch((xcb_input_touch_begin_event_t*)x_ev);
				}
				break;
		}

skip_events:

		/* extra user's post-event processor */

		_RUN_FN(_fn_event_postprocessor, x_ev);

		/* prepare window's visual update and destroy things that needs it */

		for (size_t i = _windows.n; i > 0; i--) {
			_window_present((dg_core_window_t*)_windows.ptr[i - 1]);
			_window_destroy((dg_core_window_t*)_windows.ptr[i - 1]);
		}

		for (size_t i = _grids.n; i > 0; i--) {
			_grid_destroy((dg_core_grid_t*)_grids.ptr[i - 1]);
		}

		for (size_t i = _cells.n; i > 0; i--) {
			_cell_destroy((dg_core_cell_t*)_cells.ptr[i - 1]);
		}

		/* cleaning for next event */
		
		free(x_ev);
		xcb_flush(_x_con);
	}

	_loop = false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_loop_set_callback_signal(void (*fn)(uint32_t serial))
{
	_IS_INIT;

	_fn_callback_loop_signal = fn;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_loop_set_event_postprocessor(void (*fn)(xcb_generic_event_t *x_ev))
{
	_IS_INIT;

	_fn_event_postprocessor = fn;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_loop_set_event_preprocessor(bool (*fn)(xcb_generic_event_t *x_ev))
{
	_IS_INIT;

	_fn_event_preprocessor = fn;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
dg_core_loop_send_signal(uint32_t serial)
{
	_IS_INIT;

	xcb_client_message_data_t x_data = {.data32 = {serial}};
	xcb_client_message_event_t x_ev = {
		.response_type = XCB_CLIENT_MESSAGE,
		.format   = 32,
		.sequence = 0,
		.window   = _x_win_l,
		.type     = _xa_isig,
		.data     = x_data};

	_x_test_cookie(
		xcb_send_event_checked(_x_con, 0, _x_win_l, XCB_EVENT_MASK_NO_EVENT, (char*)&x_ev),
		true);
	
	xcb_flush(_x_con);

	return true;
}

/************************************************************************************************************/
/* PUBLIC - WINDOW ******************************************************************************************/
/************************************************************************************************************/

void
dg_core_window_activate(dg_core_window_t *w)
{
	_IS_INIT;
	_IS_WINDOW(w);

	assert(!(w->state & DG_CORE_WINDOW_STATE_ACTIVE));
	assert(w->grids.n > 0);

	/* if there is no current grid, select the first one, and update the window's geometry */

	if (!w->g_current) {
		w->g_current = (dg_core_grid_t*)w->grids.ptr[0];
		_window_update_geometries(w, false);
		_RUN_FN(w->callback_grid, w, w->g_current);
	} else {
		goto skip_resize;
	}

	if (w->fixed) {
		goto skip_resize;	
	}

	/* resize the window if needed */

	int16_t pw = w->g_current->pw;
	int16_t ph = w->g_current->ph;

	pw += w->g_current->n_fwu == 0.0 ? 0 : dg_core_config_convert_str_width(w->cw_extra);
	ph += w->g_current->n_fhu == 0.0 ? 0 : dg_core_config_convert_str_height(w->ch_extra);

	_x_test_cookie(
		xcb_configure_window_checked(
			_x_con,
			w->x_win,
			XCB_CONFIG_WINDOW_WIDTH | XCB_CONFIG_WINDOW_HEIGHT,
			(uint32_t[2]){pw, ph}),
		true);

	/* update internal stuff */

skip_resize:

	w->last_render_time = dg_core_util_get_time();

	_window_set_focus(w, NULL);
	_window_set_state(w, DG_CORE_WINDOW_STATE_ACTIVE, DG_CORE_WINDOW_SET_STATE);
	_window_update_wm_size_hints(w);

	if (DG_CORE_CONFIG->win_focused_on_activate) {
		_window_set_state(w, DG_CORE_WINDOW_STATE_FOCUSED, DG_CORE_WINDOW_SET_STATE);
	}

	/* map the window */

	_x_test_cookie(xcb_map_window_checked(_x_con, w->x_win), true);
	xcb_flush(_x_con);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

dg_core_window_t *
dg_core_window_create(dg_core_window_kind_t kind)
{
	_IS_INIT;

	return _window_create((kind == DG_CORE_WINDOW_FIXED));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_window_deactivate(dg_core_window_t *w)
{
	_IS_INIT;
	_IS_WINDOW(w);

	assert((w->state & DG_CORE_WINDOW_STATE_ACTIVE));

	dg_core_cell_event_t cev = {.kind = DG_CORE_CELL_EVENT_CANCEL};

	_x_test_cookie(xcb_unmap_window_checked(_x_con, w->x_win), true);
	_window_send_event_to_all(w, &cev);
	_window_set_state(
		w,
		DG_CORE_WINDOW_STATE_ACTIVE | DG_CORE_WINDOW_STATE_LOCKED_GRID | DG_CORE_WINDOW_STATE_LOCKED_FOCUS,
		DG_CORE_WINDOW_UNSET_STATE);

	dg_core_input_buffer_clear(&w->buttons);
	dg_core_input_buffer_clear(&w->touches);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_window_destroy(dg_core_window_t *w)
{
	_IS_INIT;
	_IS_WINDOW(w);

	w->to_destroy = true;

	if (!_loop) {
		_window_destroy(w);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_window_disable(dg_core_window_t *w)
{
	_IS_INIT;
	_IS_WINDOW(w);

	dg_core_cell_event_t cev = {.kind = DG_CORE_CELL_EVENT_WINDOW_DISABLE};

	_window_set_state(w, DG_CORE_WINDOW_STATE_DISABLED, DG_CORE_WINDOW_SET_STATE);
	_window_set_state(w, DG_CORE_WINDOW_STATE_LOCKED_FOCUS, DG_CORE_WINDOW_UNSET_STATE);
	_window_set_render_level(w, _WINDOW_RENDER_BORDER);
	_window_set_present_schedule(w, _WINDOW_PRESENT_DEFAULT);
	_window_set_focus(w, NULL);
	_window_send_event_to_all(w, &cev);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_window_enable(dg_core_window_t *w)
{
	_IS_INIT;
	_IS_WINDOW(w);

	dg_core_cell_event_t cev = {.kind = DG_CORE_CELL_EVENT_WINDOW_ENABLE};

	_window_set_state(w, DG_CORE_WINDOW_STATE_DISABLED, DG_CORE_WINDOW_UNSET_STATE);
	_window_set_render_level(w, _WINDOW_RENDER_BORDER);
	_window_set_present_schedule(w, _WINDOW_PRESENT_DEFAULT);
	_window_send_event_to_all(w, &cev);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cairo_t *
dg_core_window_get_cairo_context(dg_core_window_t *w)
{
	_IS_INIT;
	_IS_WINDOW(w);

	return w->c_ctx;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cairo_surface_t *
dg_core_window_get_cairo_surface(dg_core_window_t *w)
{
	_IS_INIT;
	_IS_WINDOW(w);

	return w->c_srf;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

dg_core_grid_t *
dg_core_window_get_current_grid(dg_core_window_t *w)
{
	_IS_INIT;
	_IS_WINDOW(w);

	return w->g_current;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

int16_t
dg_core_window_get_pixel_height(dg_core_window_t *w)
{
	_IS_INIT;
	_IS_WINDOW(w);

	return w->ph;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

int16_t
dg_core_window_get_pixel_width(dg_core_window_t *w)
{
	_IS_INIT;
	_IS_WINDOW(w);

	return w->pw;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

dg_core_window_state_t
dg_core_window_get_state(dg_core_window_t *w)
{
	_IS_INIT;
	_IS_WINDOW(w);

	return w->state;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

xcb_window_t
dg_core_window_get_xcb_window(dg_core_window_t *w)
{
	_IS_INIT;
	_IS_WINDOW(w);

	return w->x_win;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_window_non_urgent(dg_core_window_t *w)
{
	_IS_INIT;
	_IS_WINDOW(w);

	xcb_icccm_wm_hints_t *x_hints;

	xcb_get_property_cookie_t xc = xcb_get_property(_x_con, 0, w->x_win, XCB_ATOM_WM_HINTS, XCB_ATOM_WM_HINTS, 0, UINT32_MAX);
	xcb_get_property_reply_t *xr = xcb_get_property_reply(_x_con, xc, NULL);
	if (!xr) {
		dg_core_errno_set(DG_CORE_ERRNO_XCB);
		return;
	}

	x_hints = xcb_get_property_value(xr);
	x_hints->flags &= ~XCB_ICCCM_WM_HINT_X_URGENCY;

	_x_set_prop(false, w->x_win, XCB_ATOM_WM_HINTS, XCB_ATOM_WM_HINTS, sizeof(xcb_icccm_wm_hints_t), x_hints);
	xcb_flush(_x_con);

	free(xr);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_window_pull_grid(dg_core_window_t *w, dg_core_grid_t *g)
{
	_IS_INIT;
	_IS_WINDOW(w);
	_IS_GRID(g);
	
	assert(!(w->state & DG_CORE_WINDOW_STATE_ACTIVE));
	assert(dg_core_stack_find(&w->grids, g, NULL));

	if (g == w->g_current) {
		dg_core_window_reset_current_grid(w);
	}

	dg_core_stack_pull(&w->grids, g);
	_window_update_wm_size_hints(w);
	g->used = false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_window_push_grid(dg_core_window_t *w, dg_core_grid_t *g)
{
	_IS_INIT;
	_IS_WINDOW(w);
	_IS_GRID(g);

	assert(!g->used);
	assert(!(w->state & DG_CORE_WINDOW_STATE_ACTIVE));
	assert(!dg_core_stack_find(&w->grids, g, NULL));
	assert(dg_core_window_test_grid_push(w, g));

	dg_core_stack_push(&w->grids, g, NULL);
	_window_update_wm_size_hints(w);
	g->used = true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_window_redraw(dg_core_window_t *w)
{
	_IS_INIT;
	_IS_WINDOW(w);

	assert(w->state & DG_CORE_WINDOW_STATE_ACTIVE);

	_window_set_render_level(w, _WINDOW_RENDER_FULL);
	_window_set_present_schedule(w, _WINDOW_PRESENT_DEFAULT);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_window_rename(dg_core_window_t *w, const char *name, const char *name_icon)
{
	_IS_INIT;
	_IS_WINDOW(w);

	size_t n1;
	size_t n2;

	w->name      = name      ? name      : _class[0];
	w->name_icon = name_icon ? name_icon : name;

	n1 = strlen(w->name);
	n2 = strlen(w->name_icon);

	_x_set_prop(false, w->x_win, _xa_nam,  XCB_ATOM_STRING, n1, w->name);
	_x_set_prop(false, w->x_win, _xa_ico,  XCB_ATOM_STRING, n2, w->name_icon);
	_x_set_prop(false, w->x_win, _xa_nnam, _xa_utf8,        n1, w->name);
	_x_set_prop(false, w->x_win, _xa_nico, _xa_utf8,        n2, w->name_icon);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_window_reset_current_grid(dg_core_window_t *w)
{
	_IS_INIT;
	_IS_WINDOW(w);

	assert(!(w->state & DG_CORE_WINDOW_STATE_ACTIVE));

	if (w->g_current != NULL) {
		_window_set_focus(w, NULL);
		 w->g_current = NULL;
		_RUN_FN(w->callback_grid, w, NULL);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_window_set_accelerator(
	dg_core_window_t *w,
	int accel_id,
	const char *name,
	void (*fn)(dg_core_window_t *w, int accel_id))
{
	_IS_INIT;
	_IS_WINDOW(w);

	assert(accel_id <= DG_CORE_CONFIG_MAX_ACCELS && accel_id > 0);
	assert(name || !fn);

	accel_id--;
	name = fn ? name : "";
	w->accels[accel_id].fn = fn;
	w->accels[accel_id].name = name;

	_x_set_prop(false, w->x_win, _xa_aclx[accel_id], _xa_utf8, strlen(name), name);
	xcb_flush(_x_con);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_window_set_callback_close(dg_core_window_t *w, void (*fn)(dg_core_window_t *w))
{
	_IS_INIT;
	_IS_WINDOW(w);

	w->callback_close = fn;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_window_set_callback_focus(dg_core_window_t *w, void (*fn)(dg_core_window_t *w, dg_core_cell_t *c))
{
	_IS_INIT;
	_IS_WINDOW(w);

	w->callback_focus = fn;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_window_set_callback_grid(dg_core_window_t *w, void (*fn)(dg_core_window_t *w, dg_core_grid_t *p))
{
	_IS_INIT;
	_IS_WINDOW(w);

	w->callback_grid = fn;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_window_set_callback_state(
	dg_core_window_t *w,
	void (*fn)(dg_core_window_t *w, dg_core_window_state_t state, dg_core_window_state_setting_mode_t mode))
{
	_IS_INIT;
	_IS_WINDOW(w);

	w->callback_state = fn;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_window_set_callback_redraw(dg_core_window_t *w, void (*fn)(dg_core_window_t *w, unsigned long delay))
{
	_IS_INIT;
	_IS_WINDOW(w);

	w->callback_redraw = fn;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_window_set_default_grid(dg_core_window_t *w,dg_core_grid_t *g)
{
	_IS_INIT;
	_IS_WINDOW(w);
	_IS_GRID(g);

	size_t i = 0;

	assert(!(w->state & DG_CORE_WINDOW_STATE_ACTIVE));
	assert(dg_core_stack_find(&w->grids, g, &i));

	w->grids.ptr[i] = w->grids.ptr[0];
	w->grids.ptr[0] = g;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
 
void
dg_core_window_set_extra_size(dg_core_window_t *w, int16_t cw_extra, int16_t ch_extra)
{
	_IS_INIT;
	_IS_WINDOW(w);

	assert(!w->fixed);

	w->cw_extra = cw_extra;
	w->ch_extra = ch_extra;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_window_set_fixed_size(dg_core_window_t *w, int16_t pw, int16_t ph)
{
	_IS_INIT;
	_IS_WINDOW(w);

	assert(w->fixed);
	assert(pw > 0 && ph > 0);

	_x_test_cookie(
		xcb_configure_window_checked(
			_x_con,
			w->x_win,
			XCB_CONFIG_WINDOW_WIDTH | XCB_CONFIG_WINDOW_HEIGHT,
			(uint32_t[2]){pw, ph}),
		true);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_window_set_fixed_position(dg_core_window_t *w, int16_t px, int16_t py)
{
	_IS_INIT;
	_IS_WINDOW(w);

	assert(w->fixed);

	_x_test_cookie(
		xcb_configure_window_checked(
			_x_con,
			w->x_win,
			XCB_CONFIG_WINDOW_X | XCB_CONFIG_WINDOW_Y,
			(uint32_t[2]){px, py}),
		true);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_window_swap_grid(dg_core_window_t *w, dg_core_grid_t *g1, dg_core_grid_t *g2)
{
	_IS_INIT;
	_IS_WINDOW(w);
	_IS_GRID(g1);
	_IS_GRID(g2);

	size_t i;

	assert(!g2->used);
	assert(dg_core_window_test_grid_swap(w, g1, g2));
	assert(dg_core_stack_find(&w->grids, g1, &i));
	assert(!dg_core_stack_find(&w->grids, g2, NULL));

	w->grids.ptr[i] = g2;

	g1->used = false;
	g2->used = true;

	/* extra operations for when the swap happens when the swapped grid is visible */

	if (w->g_current != g1) {
		return;
	}
	
	w->g_current = g2;
	_window_update_geometries(w, false);
	_window_refocus(w);
	_RUN_FN(w->callback_grid, w, w->g_current);

	if (w->state & DG_CORE_WINDOW_STATE_ACTIVE) {
		_window_set_render_level(w, _WINDOW_RENDER_FULL);
		_window_set_present_schedule(w, _WINDOW_PRESENT_DEFAULT);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
dg_core_window_test_grid_push(dg_core_window_t *w, dg_core_grid_t *g)
{
	_IS_INIT;
	_IS_WINDOW(w);
	_IS_GRID(g);

	assert(!dg_core_stack_find(&w->grids, g, NULL));

	if (w->grids.n == 0) {
		return true;
	}

	if (dg_core_grid_test_flexibility((dg_core_grid_t*)w->grids.ptr[0], g) != DG_CORE_GRID_FLEX_SAME) {
		return false;
	}

	int cmp;
	for (size_t i = 0; i < w->grids.n; i++) {
		cmp = dg_core_grid_test_size((dg_core_grid_t*)w->grids.ptr[i], g);
		if (cmp == DG_CORE_GRID_SIZE_EQUAL || cmp == DG_CORE_GRID_SIZE_UNDEFINED) {
			return false;
		}
	}

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
dg_core_window_test_grid_swap(dg_core_window_t *w, dg_core_grid_t *g1, dg_core_grid_t *g2)
{
	_IS_INIT;
	_IS_WINDOW(w);
	_IS_GRID(g1);
	_IS_GRID(g2);

	assert( dg_core_stack_find(&w->grids, g1, NULL));
	assert(!dg_core_stack_find(&w->grids, g2, NULL));

	if (dg_core_grid_test_flexibility(g1, g2) == DG_CORE_GRID_FLEX_SAME &&
	    dg_core_grid_test_size(g1, g2) == DG_CORE_GRID_SIZE_EQUAL) {
		return true;
	}

	return false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_window_transient_for(dg_core_window_t *w, dg_core_window_t *w_for)
{
	_IS_INIT;
	_IS_WINDOW(w);

	if (w_for) {
		_IS_WINDOW(w_for);
	}

	xcb_window_t x_win_for = w_for ? w_for->x_win : XCB_WINDOW_NONE;

	_x_set_prop(false, w->x_win, XCB_ATOM_WM_TRANSIENT_FOR, XCB_ATOM_WINDOW, 1, &x_win_for);
	xcb_flush(_x_con);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_window_urgent(dg_core_window_t *w)
{
	_IS_INIT;
	_IS_WINDOW(w);

	assert(w->state & DG_CORE_WINDOW_STATE_ACTIVE);

	xcb_icccm_wm_hints_t *x_hints;

	xcb_get_property_cookie_t xc = xcb_get_property(_x_con, 0, w->x_win, XCB_ATOM_WM_HINTS, XCB_ATOM_WM_HINTS, 0, UINT32_MAX);
	xcb_get_property_reply_t *xr = xcb_get_property_reply(_x_con, xc, NULL);
	if (!xr) {
		dg_core_errno_set(DG_CORE_ERRNO_XCB);
		return;
	}

	x_hints = xcb_get_property_value(xr);
	x_hints->flags |= XCB_ICCCM_WM_HINT_X_URGENCY;

	_x_set_prop(false, w->x_win, XCB_ATOM_WM_HINTS, XCB_ATOM_WM_HINTS, sizeof(xcb_icccm_wm_hints_t), x_hints);
	xcb_flush(_x_con);

	free(xr);
}

/************************************************************************************************************/
/* PUBLIC - GRID ********************************************************************************************/
/************************************************************************************************************/

void
dg_core_grid_assign_cell(dg_core_grid_t *g, dg_core_cell_t *c, int16_t cx, int16_t cy, int16_t cw, int16_t ch)
{
	_IS_INIT;
	_IS_GRID(g);
	_IS_CELL(c);

	assert(!g->used);
	assert(cx >= 0 && cy >= 0 && cw > 0 && ch > 0);
	assert(cx + cw <= g->cw && cy + ch <= g->ch);

	_area_t *a = malloc(sizeof(_area_t));
	if (!a) {
		dg_core_errno_set(DG_CORE_ERRNO_MEMORY);
		goto fail_alloc;
	}

	a->redraw = false;
	a->g_parent = g;
	a->c  = c;
	a->cx = cx;
	a->cy = cy;
	a->cw = cw;
	a->ch = ch;
	a->id = 0;

	if (!dg_core_input_buffer_init(&a->touches, DG_CORE_INPUT_BUFFER_MAX_TOUCHES, DG_CORE_INPUT_BUFFER_COORD)) {
		goto fail_buf_init;
	}

	if (!dg_core_stack_push(&g->areas, a, &a->id)) {
		goto fail_push;
	}
	
	/* send assign event to newly created area */

	dg_core_cell_event_t cev = {.kind = DG_CORE_CELL_EVENT_ASSIGN};

	_cell_process_bare_event(c, &cev);

	/* end */

	return;

	/* errors */

fail_push:
	dg_core_input_buffer_reset(&a->touches);
fail_buf_init:
	free(a);
fail_alloc:
	return;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

dg_core_grid_t *
dg_core_grid_clone(dg_core_grid_t *g)
{
	_IS_INIT;
	_IS_GRID(g);

	dg_core_grid_t *g2 = dg_core_grid_create(g->cw, g->ch);
	if (!g2) {
		return NULL;
	}

	for (size_t i = 0; i < g->cw; i++) {
		dg_core_grid_set_column_width(g2,  i, g->cwu[i]);
		dg_core_grid_set_column_growth(g2, i, g->fwu[i]);
	}

	for (size_t i = 0; i < g->ch; i++) {
		dg_core_grid_set_row_height(g2, i, g->chu[i]);
		dg_core_grid_set_row_growth(g2, i, g->fhu[i]);
	}

	_area_t *a;
	for (size_t i = 0; i < g->areas.n; i++) {
		a = (_area_t*)g->areas.ptr[i];
		dg_core_grid_assign_cell(g2, a->c, a->cx, a->cy, a->cw, a->ch);
	}

	dg_core_grid_set_reference(g2, g->g_ref);

	return g2;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

dg_core_grid_t *
dg_core_grid_create(int16_t cw, int16_t ch)
{
	_IS_INIT;

	assert(cw > 0 && ch > 0);

	dg_core_grid_t *g = malloc(sizeof(dg_core_grid_t));
	if (!g) {
		dg_core_errno_set(DG_CORE_ERRNO_MEMORY);
		goto fail_alloc;
	}

	g->areas = DG_CORE_STACK_EMPTY;
	g->g_ref = NULL;
	g->used = false;
	g->cw = cw;
	g->ch = ch;
	g->n_fwu = 0.0;
	g->n_fhu = 0.0;
	g->n_cwu = cw;
	g->n_chu = ch;
	g->n_cwu_inv = 0;
	g->n_chu_inv = 0;
	g->to_destroy = false;
	g->id = 0;

	g->cwu = NULL;
	g->chu = NULL;
	g->fwu = NULL;
	g->fhu = NULL;

	g->cwu = calloc(cw, sizeof(int16_t));
	g->chu = calloc(ch, sizeof(int16_t));
	g->fwu = calloc(cw, sizeof(double));
	g->fhu = calloc(ch, sizeof(double));

	if (!g->chu || !g->cwu || !g->fhu || !g->fwu) {
		dg_core_errno_set(DG_CORE_ERRNO_MEMORY);
		goto fail_sub_alloc;
	}

	if (!dg_core_stack_push(&_grids, g, &g->id)) {
		goto fail_push;
	}

	for (size_t i = 0; i < cw; i++) {
		g->cwu[i] = 1;
	}

	for (size_t i = 0; i < ch; i++) {
		g->chu[i] = 1;
	}

	return g;

	/* errors */

fail_push:
fail_sub_alloc:
	free(g->cwu);
	free(g->chu);
	free(g->fwu);
	free(g->fhu);
	free(g);
fail_alloc:
	return NULL;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_grid_destroy(dg_core_grid_t *g)
{
	_IS_INIT;
	_IS_GRID(g);

	g->to_destroy = true;

	if (!_loop) {
		_grid_destroy(g);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

int16_t
dg_core_grid_get_min_pixel_width(dg_core_grid_t *g)
{
	_IS_INIT;
	_IS_GRID(g);

	_grid_update_geometry(g, false);

	return g->pw;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

int16_t
dg_core_grid_get_min_pixel_height(dg_core_grid_t *g)
{
	_IS_INIT;
	_IS_GRID(g);

	_grid_update_geometry(g, false);

	return g->ph;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_grid_set_column_growth(dg_core_grid_t *g, int16_t cx, double growth)
{
	_IS_INIT;
	_IS_GRID(g);

	assert(!g->used);
	assert(cx >= 0 && cx < g->cw);
	assert(growth >= 0.0);

	g->n_fwu  += growth - g->fwu[cx];
	g->fwu[cx] = growth;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_grid_set_column_width(dg_core_grid_t *g, int16_t cx, int16_t cwu)
{
	_IS_INIT;
	_IS_GRID(g);

	assert(!g->used);
	assert(cx >= 0 && cx < g->cw);

	const int16_t cwu_old = g->cwu[cx];
	if (cwu_old == cwu) {
		return;
	}

	if (cwu_old > 0) {
		g->n_cwu -= cwu_old;
	} else {
		g->n_cwu_inv += cwu_old;
	}

	if (cwu > 0) {
		g->n_cwu += cwu;
	} else {
		g->n_cwu_inv -= cwu;
	}

	g->cwu[cx] = cwu;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_grid_set_reference(dg_core_grid_t *g, dg_core_grid_t *g_ref)
{
	_IS_INIT;
	_IS_GRID(g);
	
	assert(!g->used);

	if (g_ref) {
		_IS_GRID(g_ref);
		assert(dg_core_grid_test_flexibility(g, g_ref) == DG_CORE_GRID_FLEX_SAME);
		assert(dg_core_grid_test_size(g, g_ref)        == DG_CORE_GRID_SIZE_EQUAL);
	}

	g->g_ref = g_ref;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_grid_set_row_growth(dg_core_grid_t *g, int16_t cy, double growth)
{
	_IS_INIT;
	_IS_GRID(g);

	assert(!g->used);
	assert(cy >= 0 && cy < g->ch);
	assert(growth >= 0.0);

	g->n_fhu  += growth - g->fhu[cy];
	g->fhu[cy] = growth;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_grid_set_row_height(dg_core_grid_t *g, int16_t cy, int16_t chu)
{
	_IS_INIT;
	_IS_GRID(g);

	assert(!g->used);
	assert(cy >= 0 && cy < g->ch);

	const int16_t chu_old = g->chu[cy];
	if (chu_old == chu) {
		return;
	}

	if (chu_old > 0) {
		g->n_chu -= chu_old;
	} else {
		g->n_chu_inv += chu_old;
	}

	if (chu > 0) {
		g->n_chu += chu;
	} else {
		g->n_chu_inv -= chu;
	}

	g->chu[cy] = chu;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

dg_core_grid_test_flex_result_t
dg_core_grid_test_flexibility(dg_core_grid_t *g1, dg_core_grid_t *g2)
{
	_IS_INIT;
	_IS_GRID(g1);
	_IS_GRID(g2);

	bool flex_x1 = g1->n_fwu > 0;
	bool flex_x2 = g2->n_fwu > 0;
	bool flex_y1 = g1->n_fhu > 0;
	bool flex_y2 = g2->n_fhu > 0;

	if ((flex_x1 ^ flex_x2) >= 0 && (flex_y1 ^ flex_y2) >= 0) {
		return DG_CORE_GRID_FLEX_SAME;
	} else {
		return DG_CORE_GRID_FLEX_DIFFERENT;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

dg_core_grid_test_size_result_t
dg_core_grid_test_size(dg_core_grid_t *g1, dg_core_grid_t *g2)
{
	_IS_INIT;
	_IS_GRID(g1);
	_IS_GRID(g2);

	/* compare x axis */
	
	int x;

	if (g1->cw == g2->cw && g1->n_cwu == g2->n_cwu && g1->n_cwu_inv == g2->n_cwu_inv) {
		x = DG_CORE_GRID_SIZE_EQUAL;
	} else if (g1->cw >= g2->cw && g1->n_cwu >= g2->n_cwu && g1->n_cwu_inv >= g2->n_cwu_inv) {
		x = DG_CORE_GRID_SIZE_BIGGER;
	} else if (g1->cw <= g2->cw && g1->n_cwu <= g2->n_cwu && g1->n_cwu_inv <= g2->n_cwu_inv) {
		x = DG_CORE_GRID_SIZE_SMALLER;
	} else {
		x = DG_CORE_GRID_SIZE_UNDEFINED;
	}

	/* compare y axis */

	int y;

	if (g1->ch == g2->ch && g1->n_chu == g2->n_chu && g1->n_chu_inv == g2->n_chu_inv) {
		y = DG_CORE_GRID_SIZE_EQUAL;
	} else if (g1->ch >= g2->ch && g1->n_chu >= g2->n_chu && g1->n_chu_inv >= g2->n_chu_inv) {
		y = DG_CORE_GRID_SIZE_BIGGER;
	} else if (g1->ch <= g2->ch && g1->n_chu <= g2->n_chu && g1->n_chu_inv <= g2->n_chu_inv) {
		y = DG_CORE_GRID_SIZE_SMALLER;
	} else {
		y = DG_CORE_GRID_SIZE_UNDEFINED;
	}

	/* returns */

	if (x == DG_CORE_GRID_SIZE_EQUAL && y == DG_CORE_GRID_SIZE_EQUAL) {
		return DG_CORE_GRID_SIZE_EQUAL;
	}

	if ((x == DG_CORE_GRID_SIZE_BIGGER && y == DG_CORE_GRID_SIZE_BIGGER) ||
	    (x == DG_CORE_GRID_SIZE_BIGGER && y == DG_CORE_GRID_SIZE_EQUAL)  ||
		(x == DG_CORE_GRID_SIZE_EQUAL  && y == DG_CORE_GRID_SIZE_BIGGER)) {
		return DG_CORE_GRID_SIZE_BIGGER;
	}

	if ((x == DG_CORE_GRID_SIZE_SMALLER && y == DG_CORE_GRID_SIZE_SMALLER) ||
	    (x == DG_CORE_GRID_SIZE_SMALLER && y == DG_CORE_GRID_SIZE_EQUAL)   ||
		(x == DG_CORE_GRID_SIZE_EQUAL   && y == DG_CORE_GRID_SIZE_SMALLER)) {
		return DG_CORE_GRID_SIZE_SMALLER;
	}

	return DG_CORE_GRID_SIZE_UNDEFINED;
}

/************************************************************************************************************/
/* PUBLIC - CELL ********************************************************************************************/
/************************************************************************************************************/

dg_core_cell_t *
dg_core_cell_create(
	unsigned int serial,
	void (*fn_draw)(dg_core_cell_t *c, dg_core_cell_drawing_context_t *dc),
	void (*fn_event)(dg_core_cell_t *c, dg_core_cell_event_t *ev),
	void (*fn_destroy)(dg_core_cell_t *c),
	void *props)
{
	_IS_INIT;

	assert(fn_draw);

	dg_core_cell_t *c = malloc(sizeof(dg_core_cell_t));
	if (!c) {
		dg_core_errno_set(DG_CORE_ERRNO_MEMORY);
		goto fail_alloc;
	}

	c->serial     = serial;
	c->props      = props;
	c->ena        = true;
	c->to_destroy = false;
	c->id         = 0;

	c->fn_draw    = fn_draw;
	c->fn_event   = fn_event;
	c->fn_destroy = fn_destroy;

	if (!dg_core_stack_push(&_cells, c, &c->id)) {
		goto fail_push;
	}

	return c;

	/* errors */

fail_push:
	free(c);
fail_alloc:
	return NULL;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_cell_destroy(dg_core_cell_t *c)
{
	_IS_INIT;
	_IS_CELL(c);

	c->to_destroy = true;

	if (!_loop) {
		_cell_destroy(c);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_cell_disable(dg_core_cell_t *c)
{
	_IS_INIT;
	_IS_CELL(c);

	c->ena = false;

	dg_core_cell_event_t cev = {.kind = DG_CORE_CELL_EVENT_STATE_DISABLE};	
	_cell_process_bare_event(c, &cev);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_cell_enable(dg_core_cell_t *c)
{
	_IS_INIT;
	_IS_CELL(c);

	c->ena = true;

	dg_core_cell_event_t cev = {.kind = DG_CORE_CELL_EVENT_STATE_ENABLE};
	_cell_process_bare_event(c, &cev);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
(*dg_core_cell_get_fn_destroy(dg_core_cell_t *c))(dg_core_cell_t *c)
{
	_IS_INIT;
	_IS_CELL(c);

	return c->fn_destroy;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
(*dg_core_cell_get_fn_event(dg_core_cell_t *c))(dg_core_cell_t *c, dg_core_cell_event_t *ev)
{
	_IS_INIT;
	_IS_CELL(c);

	return c->fn_event;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
(*dg_core_cell_get_fn_draw(dg_core_cell_t *c))(dg_core_cell_t *c, dg_core_cell_drawing_context_t *dc)
{
	_IS_INIT;
	_IS_CELL(c);

	return c->fn_draw;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const void *
dg_core_cell_get_props(dg_core_cell_t *c)
{
	_IS_INIT;
	_IS_CELL(c);

	return c->props;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

unsigned int
dg_core_cell_get_serial(dg_core_cell_t *c)
{
	_IS_INIT;
	_IS_CELL(c);

	return c->serial;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
dg_core_cell_is_enabled(dg_core_cell_t *c)
{
	_IS_INIT;
	_IS_CELL(c);

	return c->ena;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_cell_redraw(dg_core_cell_t *c)
{
	_IS_INIT;
	_IS_CELL(c);

	dg_core_window_t *w;
	_area_t *a;

	for (size_t i = 0; i < _windows.n; i++) {
		
		w = (dg_core_window_t*)_windows.ptr[i];
		if (!(w->state & DG_CORE_WINDOW_STATE_ACTIVE)) {
			continue;
		}

		_window_set_render_level(w, _WINDOW_RENDER_AREAS);
		_window_set_present_schedule(w, _WINDOW_PRESENT_DEFAULT);

		for (size_t j = 0; j < w->g_current->areas.n; j++) {
			a = (_area_t*)w->g_current->areas.ptr[j];
			if (a->c == c) {
				a->redraw = true;
			}
		}
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
dg_core_cell_send_custom_event(dg_core_cell_t *c, unsigned int id, void *data, size_t data_n)
{
	_IS_INIT;
	_IS_CELL(c);

	dg_core_cell_event_t cev = {
		.kind = DG_CORE_CELL_EVENT_CUSTOM,
		.custom_id = id,
		.custom_data = data,
		.custom_data_n = data_n,
	};

	return _cell_process_bare_event(c, &cev);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_cell_toggle(dg_core_cell_t *c)
{
	_IS_INIT;
	_IS_CELL(c);

	if (c->ena) {
		dg_core_cell_disable(c);
	} else {
		dg_core_cell_enable(c);
	}
}

/************************************************************************************************************/
/* PUBLIC - CLIPBOARD ***************************************************************************************/
/************************************************************************************************************/

void
dg_core_clipboard_copy(int clipboard, const char *str, dg_core_cell_t *c, 
	void (*fn_copy)(int clipboard, dg_core_cell_t *c), void (*fn_lost)(int clipboard, dg_core_cell_t *c))
{
	_IS_INIT;

	assert(clipboard >= 0 && clipboard <= 2);
	assert(str);

	const xcb_timestamp_t xt = _x_get_timestamp();

	/* save data internally */

	char *tmp = strdup(str);
	if (!tmp) {
		dg_core_errno_set(DG_CORE_ERRNO_MEMORY);
		return;
	}
	
	free(_sel[clipboard].data);
	_sel[clipboard].time = xt;
	_sel[clipboard].owned = true;
	_sel[clipboard].data = tmp;
	_sel[clipboard].data_n = strlen(tmp) + 1;
	_sel[clipboard].fn_copy = fn_copy;
	_sel[clipboard].fn_lost = fn_lost;
	_sel[clipboard].c_signal = c;

	/* get selection ownership */
	
	_x_test_cookie(xcb_set_selection_owner_checked(_x_con, _x_win_l, _x_get_atom_sel(clipboard), xt), true);
	xcb_flush(_x_con);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_clipboard_delete(int clipboard)
{
	_IS_INIT;

	assert(clipboard >= 0 && clipboard <= 2);

	if (!_sel[clipboard].owned) {
		return;
	}

	_x_test_cookie(
		xcb_set_selection_owner_checked(_x_con, XCB_WINDOW_NONE, _x_get_atom_sel(clipboard), _x_get_timestamp()),
		true);
	xcb_flush(_x_con);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

char *
dg_core_clipboard_paste(int clipboard)
{
	_IS_INIT;

	assert(clipboard >= 0 && clipboard <= 2);
	
	const xcb_timestamp_t xt = _x_get_timestamp();
	const xcb_atom_t  xa_sel = _x_get_atom_sel(clipboard);
	const xcb_atom_t  xa_tmp = _x_get_atom_sel_target(clipboard);
	
	char *tmp = NULL;

	/* send selection request */

	if (_sel[clipboard].owned) {
		tmp = strdup(_sel[clipboard].data);
		goto done;
	}

	_x_test_cookie(xcb_convert_selection_checked(_x_con, _x_win_l, xa_sel, _xa_utf8, xa_tmp, xt), true);
	xcb_flush(_x_con);

	/* wait for selection notification event */

	xcb_selection_notify_event_t *x_ev;

	while (true) {

		x_ev = (xcb_selection_notify_event_t*)xcb_wait_for_event(_x_con);
		if (!x_ev) {
			return NULL;
		}

		if ((x_ev->response_type & ~0x80) == XCB_SELECTION_NOTIFY &&
			x_ev->selection == xa_sel   &&
		    x_ev->requestor == _x_win_l &&
			x_ev->target    == _xa_utf8 &&
			x_ev->time      == xt) {
			if (x_ev->property == XCB_ATOM_NONE) {
				free(x_ev);
				return NULL;
			}
			free(x_ev);
			break;
		} else {
			dg_core_stack_push(&_events, x_ev, NULL);
		}
	}

	/* grab the contents of the selection and delete the property holding it*/

	xcb_get_property_cookie_t xc = xcb_get_property(_x_con, XCB_PROPERTY_DELETE, _x_win_l, xa_tmp, XCB_ATOM_ANY, 0, UINT32_MAX);
	xcb_get_property_reply_t *xr = xcb_get_property_reply(_x_con, xc, NULL);
	if (!xr) {
		dg_core_errno_set(DG_CORE_ERRNO_XCB);
		return NULL;
	}

	tmp = strdup(xcb_get_property_value(xr));
	free(xr);

	/* end */

done:

	if (!tmp) {
		dg_core_errno_set(DG_CORE_ERRNO_MEMORY);
	}

	return tmp;
}

/************************************************************************************************************/
/* POPUP ****************************************************************************************************/
/************************************************************************************************************/

void
dg_core_popup_spawn(dg_core_cell_t *c, dg_core_window_t *w_ref, int16_t px, int16_t px_alt, int16_t py, 
                    int16_t py_alt, int16_t pw, int16_t ph)
{
	_IS_INIT;
	_IS_CELL(c);

	if (w_ref) {
		_IS_WINDOW(w_ref);
	}

	assert(pw > 0 && ph > 0);

	/* instantiate */

	_rect_t rect = _popup_get_geometry(w_ref, px, px_alt, py, py_alt, pw, ph);

	_popup_t *p = malloc(sizeof(_popup_t));
	if (!p) {
		dg_core_errno_set(DG_CORE_ERRNO_MEMORY);
		goto fail_alloc;
	}

	p->px = rect.x;
	p->py = rect.y;
	p->pw = rect.w;
	p->ph = rect.h;

	p->p_parent = _p_last;
	p->p_child  = NULL;

	/* grab inputs if its the first active popup */

	if (!_p_last && !_popup_grab_inputs()) {
		dg_core_errno_set(DG_CORE_ERRNO_XCB);
		goto fail_grab;
	}

	/* create window with override redirect */

	p->w = _window_create(true);
	if (!p->w) {
		goto fail_win;
	}

	p->w->p_container = p;

	/* create grid to host the cell */

	dg_core_grid_t *g = dg_core_grid_create(1, 1);
	if (!g) {
		goto fail_grid;
	}

	dg_core_grid_set_column_growth(g, 0, 1.0);
	dg_core_grid_set_row_growth(g, 0, 1.0);
	dg_core_grid_assign_cell(g, c, 0, 0, 1, 1);
	if (g->areas.n == 0) {
		goto fail_grid_assign;
	}

	dg_core_window_push_grid(p->w, g);
	if (p->w->grids.n == 0) {
		goto fail_grid_push;
	}

	/* setup and activate popup */

	_x_test_cookie(
		xcb_configure_window_checked(
			_x_con,
			p->w->x_win,
			XCB_CONFIG_WINDOW_X | XCB_CONFIG_WINDOW_Y | XCB_CONFIG_WINDOW_WIDTH | XCB_CONFIG_WINDOW_HEIGHT,
			(uint32_t[4]){p->px, p->py, p->pw, p->ph}),
		true);
	
	p->w->g_current = g;
	_window_update_geometries(p->w, true);
	dg_core_window_activate(p->w);

	/* end */

	if (_p_last) {
		_p_last->p_child = p;
	} else {
		_p_hover = NULL;
	}
	_p_last = p;

	return;

	/* errors */

fail_grid_push:
fail_grid_assign:
	dg_core_grid_destroy(g);
fail_grid:
	dg_core_window_destroy(p->w);
fail_win:
fail_grab:
	free(p);
fail_alloc:
	return;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_popup_kill_children(dg_core_window_t *w)
{
	_IS_INIT;
	_IS_WINDOW(w);

	if (!w->p_container) {
		return;
	}

	_popup_kill(w->p_container->p_child);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_popup_kill_all(void)
{
	_IS_INIT;

	_popup_t *p = _p_last;
	if (!p) {
		return;
	}

	while (p->p_parent) {
		p = p->p_parent;
	}

	_popup_kill(p);
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static _rect_t
_area_get_current_geometry(_area_t *a, dg_core_window_t *w)
{
	int16_t l;
	int16_t n;
	double  f;

	dg_core_grid_t *g = w->g_current;
	_rect_t rect = {.x = a->px, .y = a->py, .w = a->pw, .h = a->ph};

	/* extra horizontal offset and width */

	f = g->n_fwu;
	n = w->pw - g->pw;

	for (size_t i = 0; i < a->cx + a->cw; i++) {
		if (f <= 0.0) {
			break;
		}
		l  = n * g->fwu[i] / f;
		f -= g->fwu[i];
		n -= l;
		if (i < a->cx) {
			rect.x += l;
		} else {
			rect.w += l;
		}
	}

	/* extra vertical offset and height */

	f = g->n_fhu;
	n = w->ph - g->ph;

	for (size_t i = 0; i < a->cy + a->ch; i++) {
		if (f <= 0.0) {
			break;
		}
		l  = n * g->fhu[i] / f;
		f -= g->fhu[i];
		n -= l;
		if (i < a->cy) {
			rect.y += l;
		} else {
			rect.h += l;
		}
	}

	return rect;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static dg_core_cell_focus_t
_area_get_focus_type(_area_t *a, dg_core_window_t *w)
{
	if (a == w->a_focus && (w->state & DG_CORE_WINDOW_STATE_LOCKED_FOCUS)) {
		return DG_CORE_CELL_FOCUS_PRIMARY_LOCKED;
	}

	if (a == w->a_focus) {
		return DG_CORE_CELL_FOCUS_PRIMARY;
	}

	if (a->touches.n > 0) {
		return DG_CORE_CELL_FOCUS_SECONDARY;
	}

	return DG_CORE_CELL_FOCUS_NONE;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_area_redraw(_area_t *a, dg_core_window_t *w, unsigned long delay)
{
	if (!a->redraw && w->render_level != _WINDOW_RENDER_FULL) {
		return;
	}

	const dg_core_cell_focus_t focus = _area_get_focus_type(a, w);

	/* setup drawing context then draw */

	_rect_t rect = _area_get_current_geometry(a, w);
	if (rect.w <= 0 || rect.h <= 0) {
		return;
	}

	dg_core_cell_drawing_context_t dc = {
		.msg = DG_CORE_CELL_DRAW_MSG_NONE,
		.focus = focus,
		.delay = delay,
		.cell_px = rect.x,
		.cell_py = rect.y,
		.cell_pw = rect.w,
		.cell_ph = rect.h,
		.is_enabled = a->c->ena,
		.win_is_enabled = !(w->state & DG_CORE_WINDOW_STATE_DISABLED),
		.c_ctx = w->c_ctx,
	};

	cairo_set_operator(w->c_ctx, CAIRO_OPERATOR_SOURCE);
	a->c->fn_draw(a->c, &dc);
	a->redraw = (dc.msg & DG_CORE_CELL_DRAW_MSG_REQUEST_UPDATE);

	/* trigger a redraw of neighbouring areas if their focus level is higher */

	if (!(dc.msg & DG_CORE_CELL_DRAW_MSG_OUT_OF_BOUNDS) || a ==  w->a_focus) {
		return;
	}

	_area_list_t neighbours = _grid_get_neighbour_areas(w->g_current, a);
	dg_core_cell_focus_t focus_nb;

	for (size_t i = 0; i < neighbours.n; i++) {
		focus_nb = _area_get_focus_type(neighbours.areas[i], w);
		if ((focus == DG_CORE_CELL_FOCUS_NONE      && focus_nb != DG_CORE_CELL_FOCUS_NONE) ||
		    (focus == DG_CORE_CELL_FOCUS_SECONDARY && w->a_focus == neighbours.areas[i])) {
			neighbours.areas[i]->redraw = true;
		}
	}

	free(neighbours.areas);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_area_update_geometry(_area_t *a, dg_core_grid_t *g, bool is_popup)
{
	const int16_t l1 = is_popup ? 0 : DG_CORE_CONFIG->win_pad_outer + DG_CORE_CONFIG->win_thick_bd;
	const int16_t l2 = is_popup ? 0 : DG_CORE_CONFIG->win_pad_inner;

	a->px = l1;
	for (size_t i = 0; i < a->cx; i++) {
		if (g->cwu[i] != 0) {
			a->px += l2 + dg_core_config_get_cell_width(g->cwu[i]);
		}
	}

	a->py = l1;
	for (size_t i = 0; i < a->cy; i++) {
		if (g->chu[i] != 0) {
			a->py += l2 + dg_core_config_get_cell_height(g->chu[i]);
		}
	}

	a->pw = dg_core_config_get_cell_width(g->cwu[a->cx]);
	for (size_t i = a->cx + 1; i < a->cx + a->cw; i++) {
		if (g->cwu[i] != 0) {
			a->pw += l2 + dg_core_config_get_cell_width(g->cwu[i]);
		}
	}

	a->ph = dg_core_config_get_cell_height(g->chu[a->cy]);
	for (size_t i = a->cy + 1; i < a->cy + a->ch; i++) {
		if (g->chu[i] != 0) {
			a->ph += l2 + dg_core_config_get_cell_height(g->chu[i]);
		}
	}

	a->pw -= g->cwu[a->cx] != 0 ? 0 : l2;
	a->ph -= g->chu[a->cy] != 0 ? 0 : l2;
	a->pw = a->pw > 0 ? a->pw : -l2;
	a->ph = a->ph > 0 ? a->ph : -l2;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_cell_destroy(dg_core_cell_t *c)
{
	if (!c->to_destroy) {
		return;
	}

	_RUN_FN(c->fn_destroy, c);

	dg_core_stack_pull(&_cells, c);
	free(c);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
_cell_process_bare_event(dg_core_cell_t *c, dg_core_cell_event_t *cev)
{
	if (!c || !c->fn_event) {
		return false;
	}

	/* fill remaining event fields */

	cev->msg = DG_CORE_CELL_EVENT_MSG_NONE;
	cev->w_host  = NULL;
	cev->cell_px = -1;
	cev->cell_py = -1;
	cev->cell_pw = -1;
	cev->cell_ph = -1;

	/* send event */

	c->fn_event(c, cev);

	/* process msg */

	if (cev->msg & DG_CORE_CELL_EVENT_MSG_REJECT) {
		return false;
	}

	if (cev->msg & DG_CORE_CELL_EVENT_MSG_REQUEST_UPDATE) {
		dg_core_cell_redraw(c);
	}

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_clipboard_clear(int clipboard)
{
	free(_sel[clipboard].data);

	_sel[clipboard].time     = 0;
	_sel[clipboard].owned    = false;
	_sel[clipboard].data     = NULL;
	_sel[clipboard].data_n   = 0;
	_sel[clipboard].fn_copy  = NULL;
	_sel[clipboard].fn_lost  = NULL;
	_sel[clipboard].c_signal = NULL;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_event_client_message(xcb_client_message_event_t *x_ev)
{
	if (x_ev->type == _xa_isig) {
		_RUN_FN(_fn_callback_loop_signal, x_ev->data.data32[0]);
		return;
	}

	if (x_ev->type != _xa_prot) {
		return;
	}

	const xcb_atom_t xa  = x_ev->data.data32[0];
	const xcb_atom_t xa2 = x_ev->data.data32[1];
	const xcb_atom_t xa3 = x_ev->data.data32[2];

	/* window independent messages */

	if (xa == _xa_sig && xa2 == _xa_conf) {
		_misc_reconfig();
		return;
	}

	/* window dependent messages */

	dg_core_window_t *w = _loop_find_window(x_ev->window);
	if (!w) {
		return;
	}

	if (xa == _xa_sig && xa2 == _xa_acl) {
		if (xa3 > 0 && xa3 <= DG_CORE_CONFIG_MAX_ACCELS) {
			_RUN_FN(w->accels[xa3 - 1].fn, w, xa3);
		}
		return;
	}

	if (xa == _xa_del) {
		if (w->callback_close) {
			w->callback_close(w);
		} else {
			dg_core_window_deactivate(w);
		}
		return;
	}

	if (xa == _xa_foc) {
		xcb_set_input_focus(_x_con, XCB_INPUT_FOCUS_PARENT, w->x_win, XCB_CURRENT_TIME);
		return;
	}

	if (xa == _xa_ping) {
		x_ev->window = _x_scr->root;
		xcb_send_event(_x_con, 0, _x_scr->root, XCB_EVENT_MASK_NO_EVENT, (char*)x_ev);
		xcb_flush(_x_con);
		return;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_event_configure(xcb_configure_notify_event_t *x_ev)
{
	dg_core_window_t *w = _loop_find_window(x_ev->window);
	if (!w) {
		return;
	}

	w->px = x_ev->x;
	w->py = x_ev->y;

	if (x_ev->width == w->pw && x_ev->height == w->ph) {
		return;
	}

	if (x_ev->width > INT16_MAX) {
		x_ev->width = INT16_MAX;
	}

	if (x_ev->height > INT16_MAX) {
		x_ev->height = INT16_MAX;
	}

	_window_resize(w, (int16_t)x_ev->width, (int16_t)x_ev->height);
	_window_update_current_grid(w);
	_window_update_wm_focus_hints(w);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_event_core_input(xcb_key_press_event_t *x_ev)
{
	dg_core_window_t *w = _loop_find_window(x_ev->event);
	if (!w) {
		w = _popup_prep_core_input(x_ev);
		if (!w) {
			return;
		}
	}
	
	uint8_t val = x_ev->detail;

	/* identify event type */

	uint8_t type = x_ev->response_type & ~0x80;

	bool is_key   = type == XCB_KEY_PRESS || type == XCB_KEY_RELEASE;
	bool is_press = type == XCB_KEY_PRESS || type == XCB_BUTTON_PRESS;

	/* get swap shift level */

	int shift;

	if (!(x_ev->state & DG_CORE_CONFIG->mod_meta)) {
		shift = 0;
	} else if (x_ev->state & XCB_MOD_MASK_SHIFT) {
		shift = 2;
	} else {
		shift = 1;
	}

	/* get swap */

	if ((is_key && val > DG_CORE_CONFIG_MAX_KEYS) || (!is_key && val > DG_CORE_CONFIG_MAX_BUTTONS)) {
		return;
	}
	
	dg_core_config_swap_t swap = is_key ? DG_CORE_CONFIG->swap_key[val][shift] : 
	                                      DG_CORE_CONFIG->swap_but[val][shift];

	/* do stuff depending on swap type whether the button/key are pressed or released */

	switch (swap.kind) {
	
		case DG_CORE_CONFIG_SWAP_TO_DEFAULT:
			swap.value = val;
			/* fallthrough */

		case DG_CORE_CONFIG_SWAP_TO_VALUE:
			if (is_key) {
				_sub_event_input_key(w, swap.value, x_ev);
			} else {
				_sub_event_input_button(w, swap.value, x_ev);
			}
			return;

		case DG_CORE_CONFIG_SWAP_TO_NONE:
			return;

		default:
			break;
	}

	/* subevents that only happens on button/key press and not during release */

	if (!is_press) {
		return;
	}
	
	switch (swap.kind) {
		
		case DG_CORE_CONFIG_SWAP_TO_ACCELERATOR:
			_RUN_FN(w->accels[swap.value - 1].fn, w, swap.value);
			break;

		case DG_CORE_CONFIG_SWAP_TO_ACTION_CELL:
			_sub_event_action_cell(w, swap.value);
			break;

		case DG_CORE_CONFIG_SWAP_TO_ACTION_FOCUS:
			_sub_event_action_focus(w, swap.value);
			break;

		case DG_CORE_CONFIG_SWAP_TO_ACTION_WINDOW:
			_sub_event_action_window(w, swap.value);
			break;

		case DG_CORE_CONFIG_SWAP_TO_ACTION_MISC:
			_sub_event_action_misc(swap.value);
			break;

		case DG_CORE_CONFIG_SWAP_TO_CLIPBOARD_CUT:
		case DG_CORE_CONFIG_SWAP_TO_CLIPBOARD_COPY:
		case DG_CORE_CONFIG_SWAP_TO_CLIPBOARD_PASTE:
			_sub_event_clipboard(w, swap.kind, swap.value);
			break;

		default:
			break;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_event_expose(xcb_expose_event_t *x_ev)
{
	dg_core_window_t *w = _loop_find_window(x_ev->window);
	if (!w) {
		return;
	}

	_window_set_render_level(w, _WINDOW_RENDER_FULL);
	_window_set_present_schedule(w, _WINDOW_PRESENT_IMMEDIATE);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_event_focus_in(xcb_focus_in_event_t *x_ev)
{
	dg_core_window_t *w = _loop_find_window(x_ev->event);
	if (!w) {
		return;
	}

	_window_set_state(w, DG_CORE_WINDOW_STATE_FOCUSED, DG_CORE_WINDOW_SET_STATE);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_event_focus_out(xcb_focus_in_event_t *x_ev)
{
	dg_core_window_t *w = _loop_find_window(x_ev->event);
	if (!w) {
		return;
	}

	/* clear all tracked inputs */

	dg_core_grid_t *g;
	
	dg_core_input_buffer_clear(&w->buttons);
	dg_core_input_buffer_clear(&w->touches);

	for (size_t i = 0; i < w->grids.n; i++) {
		g = (dg_core_grid_t*)w->grids.ptr[i];
		for (size_t j = 0; j < g->areas.n; j++) {
			dg_core_input_buffer_clear(&((_area_t*)g->areas.ptr[j])->touches);
		}
	}

	/* mode values significations (maybe) (common for both focus in and out) :                     */
	/* 0 = explictit focus change by the end-user                                                  */
	/* 1 = focus temporary lost when window is resized or moved      (never happens for focus in ) */
	/* 2 = focus gain after expose event after move or resize action (never happens for focus out) */
	/* 3 = focus change due to a workspace change when the window is focused                       */

	if (x_ev->mode != 1) {
		_window_set_state(w, DG_CORE_WINDOW_STATE_FOCUSED, DG_CORE_WINDOW_UNSET_STATE);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_event_keymap(xcb_mapping_notify_event_t *x_ev)
{
	xcb_refresh_keyboard_mapping(_x_ksm, x_ev);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_event_leave(xcb_leave_notify_event_t *x_ev)
{
	dg_core_window_t *w = _loop_find_window(x_ev->event);
	if (!w) {
		return;
	}

	/* fixes focus glitch of motion not detected when leaving cells if the pointer moves too fast */

	if (w->g_current) {
		_window_focus_by_pointer(w, x_ev->event_x, x_ev->event_y);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_event_map(xcb_map_notify_event_t *x_ev)
{
	dg_core_window_t *w = _loop_find_window(x_ev->window);
	if (!w) {
		return;
	}

	_window_set_state(w, DG_CORE_WINDOW_STATE_MAPPED, DG_CORE_WINDOW_SET_STATE);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_event_motion(xcb_motion_notify_event_t *x_ev)
{
	dg_core_window_t *w = _loop_find_window(x_ev->event);
	if (!w) {
		w = _popup_prep_motion_input(x_ev);
		if (!w) {
			return;
		}
	}

	/* update focus */

	_window_focus_by_pointer(w, x_ev->event_x, x_ev->event_y);
	if (!w->a_focus) {
		return;
	}

	/* send motion event to focused cell if any */
	
	dg_core_cell_event_t cev = {
		.kind = w->buttons.n > 0 ? DG_CORE_CELL_EVENT_POINTER_DRAG : DG_CORE_CELL_EVENT_POINTER_HOVER,
		.pointer_px = x_ev->event_x,
		.pointer_py = x_ev->event_y,
	};

	_window_process_cell_event(w, w->a_focus, &cev);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_event_present(xcb_present_generic_event_t *x_ev)
{
	xcb_present_complete_notify_event_t *x_pev;
	dg_core_window_t *w;

	/* present event filtering */

	if (x_ev->evtype != XCB_PRESENT_EVENT_COMPLETE_NOTIFY) {
		return;
	}

	x_pev = (xcb_present_complete_notify_event_t *)x_ev;
	if (x_pev->kind != XCB_PRESENT_COMPLETE_KIND_NOTIFY_MSC) {
		return;
	}

	w = _loop_find_window(x_pev->window);
	if (!w || w->present_serial != x_pev->serial) {
		return;
	}

	/* update window's content */

	if (w->g_current) {
		_window_redraw(w);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_event_selection_clear(xcb_selection_clear_event_t *x_ev)
{
	const int selection = _x_get_selection_id(x_ev->selection);
	if (selection == -1) {
		return;
	}	
	
	_RUN_FN(_sel[selection].fn_lost, selection, _sel[selection].c_signal);
	_clipboard_clear(selection);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_event_selection_request(xcb_selection_request_event_t *x_ev)
{
	const int selection = _x_get_selection_id(x_ev->selection);

	bool data_sent = false; /* should be set to true only if actual data was sent, not time or targets */

	xcb_selection_notify_event_t x_ev_notif = {
		.response_type = XCB_SELECTION_NOTIFY,
		.sequence      = x_ev->sequence,
		.time          = x_ev->time,
		.requestor     = x_ev->requestor,
		.selection     = x_ev->selection,
		.target        = x_ev->target,
		.property      = XCB_ATOM_NONE
	};

	/* filtering */

	if (selection == -1 ||
	    (x_ev->time != XCB_TIME_CURRENT_TIME && x_ev->time < _sel[selection].time) ||
	    (x_ev->target == _xa_mult && x_ev->property == XCB_ATOM_NONE)) {
		goto refuse;
	}

	for (int i = 0; i < sizeof(_sel_targets) / sizeof(xcb_atom_t); i++) {
		if (x_ev->target == _sel_targets[i]) {
			goto found;
		}
	}
	goto refuse;

found:;

	/* send data depending on target type */

	xcb_atom_t *xas;
	xcb_get_property_cookie_t xc;
	xcb_get_property_reply_t *xr;

	if (x_ev->target != _xa_mult) {	
		x_ev->property = x_ev->property == XCB_ATOM_NONE ? x_ev->target : x_ev->property;
		data_sent = _x_send_sel_data(selection, x_ev->requestor, x_ev->property, x_ev->target);
		goto success;
	}

	xc = xcb_get_property(_x_con, XCB_PROPERTY_DELETE, x_ev->requestor, x_ev->property, XCB_ATOM_ATOM, 0, UINT32_MAX);
	xr = xcb_get_property_reply(_x_con, xc, NULL);
	if (!xr) {
		dg_core_errno_set(DG_CORE_ERRNO_XCB);
		goto refuse;
	}

	xas = (xcb_atom_t*)xcb_get_property_value(xr);
	for (size_t i = 0; i + 1 < xr->value_len; i += 2) {
		data_sent |= _x_send_sel_data(selection, x_ev->requestor, xas[i], xas[i + 1]);
	}

	free(xr);

	/* send notification event as reply to request */

success:

	x_ev_notif.property = x_ev->property;

	if (data_sent) {
		_RUN_FN(_sel[selection].fn_copy, selection, _sel[selection].c_signal);
	}

refuse:

	_x_test_cookie(
		xcb_send_event_checked(_x_con, 0, x_ev->requestor, XCB_EVENT_MASK_NO_EVENT, (char*)&x_ev_notif),
		true);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_event_unmap(xcb_unmap_notify_event_t *x_ev)
{
	dg_core_window_t *w = _loop_find_window(x_ev->window);
	if (!w) {
		return;
	}

	dg_core_cell_event_t cev = {.kind = DG_CORE_CELL_EVENT_CANCEL};

	_window_send_event_to_all(w, &cev);
	_window_set_state(w, DG_CORE_WINDOW_STATE_MAPPED, DG_CORE_WINDOW_UNSET_STATE);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_event_visibility(xcb_visibility_notify_event_t *x_ev)
{
	dg_core_window_t *w = _loop_find_window(x_ev->window);
	if (!w) {
		return;
	}

	_window_set_state(
		w,
		DG_CORE_WINDOW_STATE_OBSCURED,
		x_ev->state > 1 ? DG_CORE_WINDOW_SET_STATE : DG_CORE_WINDOW_UNSET_STATE);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_event_xinput_touch(xcb_input_touch_begin_event_t *x_ev)
{
	dg_core_window_t *w = _loop_find_window(x_ev->event);
	if (!w) {
		return;
	}

	/*****/

	int16_t px = dg_core_util_convert_fp1616_to_int16(x_ev->event_x);
	int16_t py = dg_core_util_convert_fp1616_to_int16(x_ev->event_y);

	switch (x_ev->event_type) {

		case XCB_INPUT_TOUCH_BEGIN:
			_sub_event_touch_begin(w, x_ev->detail, px, py);
			break;

		case XCB_INPUT_TOUCH_END:
			_sub_event_touch_end(w, x_ev->detail, px, py);
			break;

		case XCB_INPUT_TOUCH_UPDATE:
			_sub_event_touch_update(w, x_ev->detail, px, py);
			break;

		default:
			break;
	}	
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_grid_destroy(dg_core_grid_t *g)
{
	if (!g->to_destroy) {
		return;
	}

	_area_t *a;

	for (size_t i = 0; i < g->areas.n; i++) {
		a = (_area_t*)g->areas.ptr[i];
		dg_core_input_buffer_reset(&a->touches);
		free(a);
	}

	dg_core_stack_reset(&g->areas);
	free(g->chu);
	free(g->cwu);
	free(g->fwu);
	free(g->fhu);
	
	dg_core_stack_pull(&_grids, g);
	free(g);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static _area_t *
_grid_find_first_area(dg_core_grid_t *g, dg_core_cell_t *c)
{
	_area_t *a;

	dg_core_cell_event_t cev = {
		.kind = DG_CORE_CELL_EVENT_SEEK_CELL,
		.seek_cell = c,
	};

	/* first check if an area holds the target cell, if not, send a cell seek event to that area's cell */
	/* if the event has not been rejected, then it should mean that the target cell is somewhere        */
	/* within that area's cell (and that cell is therefore a meta-cell).                                */

	for (size_t i = 0; i < g->areas.n; i++) {
		a = (_area_t*)g->areas.ptr[i];
		if (c == a->c || _cell_process_bare_event(a->c, &cev)) {
			return a;
		}
	}

	return NULL;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static _area_list_t
_grid_get_neighbour_areas(dg_core_grid_t *g, _area_t *a)
{
	_area_list_t list = {0};

	if (g->areas.n == 0) {
		return list;
	}

	list.areas = calloc(g->areas.n, sizeof(_area_t*));
	if (!list.areas) {
		dg_core_errno_set(DG_CORE_ERRNO_MEMORY);
		return list;
	}

	/* search bounds */

	const int16_t cx = a->cx - 1;
	const int16_t cy = a->cy - 1;
	const int16_t cw = a->cw + 2;
	const int16_t ch = a->ch + 2;

	/* check if any areas cross the search bounds */

	_area_t *a_tmp;

	for (size_t i = 0; i < g->areas.n; i++) {
		a_tmp = (_area_t*)g->areas.ptr[i];

		if (dg_core_util_test_bounds(a_tmp->cx, a_tmp->cy, cx, cy, cw, ch) && 
		    dg_core_util_test_bounds(a_tmp->cx + a_tmp->cw, a_tmp->cy + a_tmp->ch, cx, cy, cw, ch)) {
			list.areas[list.n] = a_tmp;
			list.n++;
		}
	}

	return list;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_grid_update_geometry(dg_core_grid_t *g, bool is_popup)
{
	const int16_t l1 = is_popup ? 0 : 2 * (DG_CORE_CONFIG->win_thick_bd + DG_CORE_CONFIG->win_pad_outer);
	const int16_t l2 = is_popup ? 0 : DG_CORE_CONFIG->win_pad_inner;

	g->pw = l1 + dg_core_config_get_cell_width(g->cwu[0]);
	for (size_t i = 1; i < g->cw; i++) {
		if (g->cwu[i] != 0) {
			g->pw += l2 + dg_core_config_get_cell_width(g->cwu[i]);
		}
	}

	g->ph = l1 + dg_core_config_get_cell_height(g->chu[0]);
	for (size_t i = 1; i < g->ch; i++) {
		if (g->chu[i] != 0) {
			g->ph += l2 + dg_core_config_get_cell_height(g->chu[i]);
		}
	}

	g->pw -= g->cwu[0] != 0 ? 0 : l2;
	g->ph -= g->chu[0] != 0 ? 0 : l2;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static dg_core_window_t *
_loop_find_window(xcb_window_t x_win)
{
	dg_core_window_t *win;

	for (size_t i = 0; i < _windows.n; i++) {
		win = (dg_core_window_t*)_windows.ptr[i];
		if (x_win == win->x_win) {
			return win;
		}
	}

	return NULL;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
_loop_no_active_windows(void)
{
	for (size_t i = 0; i < _windows.n; i++) {
		if (((dg_core_window_t *)_windows.ptr[i])->state & DG_CORE_WINDOW_STATE_ACTIVE) {
			return false;
		}
	}

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_misc_reconfig(void)
{
	dg_core_resource_load_all();

	dg_core_window_t *w;
	dg_core_grid_t *g;
	dg_core_grid_t *g_min;

	/* update all windows                                                */
	/* resize them if they are smaller than their new minimun dimensions */

	for (size_t i = 0; i < _windows.n; i++) {

		w = (dg_core_window_t*)_windows.ptr[i];
		g = w->g_current;
		g_min = _window_find_smallest_grid(w);

		if (!(w->state & DG_CORE_WINDOW_STATE_ACTIVE)) {
			dg_core_window_reset_current_grid(w);
			continue;
		}

		_window_update_geometries(w, w->p_container);
		_window_update_current_grid(w);
		_window_update_wm_size_hints(w);
		_window_set_render_level(w, _WINDOW_RENDER_FULL);
		_window_set_present_schedule(w, _WINDOW_PRESENT_DEFAULT);

		if (g->n_fwu == 0.0 || (g == g_min && g->pw > w->pw)) {
			xcb_configure_window(_x_con, w->x_win, XCB_CONFIG_WINDOW_WIDTH, &g->pw);
		}

		if (g->n_fhu == 0.0 || (g == g_min && g->ph > w->ph)) {
			xcb_configure_window(_x_con, w->x_win, XCB_CONFIG_WINDOW_HEIGHT, &g->ph);
		}
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static _popup_t *
_popup_find_under_coords(int16_t px, int16_t py)
{
	_popup_t *p = _p_last;

	do {
		if (dg_core_util_test_bounds(px, py, p->px, p->py, p->pw, p->ph)) {
			return p;
		}
		p = p->p_parent;
	} while (p);

	return NULL;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static _rect_t
_popup_get_geometry(dg_core_window_t *w_ref, int16_t px, int16_t px_alt, int16_t py_alt, int16_t py, 
	int16_t pw, int16_t ph)
{ 
	if (w_ref) {
		px += w_ref->px;
		py += w_ref->py;
		px_alt += w_ref->px;
		py_alt += w_ref->py;
	}

	/* apply overrides and max geometry */

	if (DG_CORE_CONFIG->popup_override_position) {
		px = DG_CORE_CONFIG->popup_override_px;
		py = DG_CORE_CONFIG->popup_override_py;
		px_alt = DG_CORE_CONFIG->popup_override_px;
		py_alt = DG_CORE_CONFIG->popup_override_py;
	}

	if (DG_CORE_CONFIG->popup_override_width && DG_CORE_CONFIG->popup_override_pw > 0) {
		pw = DG_CORE_CONFIG->popup_override_pw;
	} else if (DG_CORE_CONFIG->popup_max_pw > 0) {
		pw = DG_CORE_CONFIG->popup_max_pw;
	}

	if (DG_CORE_CONFIG->popup_override_height && DG_CORE_CONFIG->popup_override_ph > 0) {
		pw = DG_CORE_CONFIG->popup_override_ph;
	} else if (DG_CORE_CONFIG->popup_max_ph > 0) {
		pw = DG_CORE_CONFIG->popup_max_ph;
	}

	/* find which display will the popup appear on and retrieve its geometry */
	/* thrn adjust the popup's geometry based on the monitor's geometry      */

	_rect_t rect = _x_get_monitor_geometry_at(px, py);
	if (rect.w <= 0 || rect.h <= 0) {
		goto skip_adjust;
	}

	if (px + pw > rect.x + rect.w) {
		px = px_alt - pw;
	}

	if (py + ph > rect.y + rect.h) {
		py = py_alt - ph;
	}

	if (px < rect.x) {
		px = rect.x;
	}

	if (py < rect.y) {
		py = rect.y;
	}

	if (pw > rect.w) {
		pw = rect.w;
	}

	if (ph > rect.h) {
		ph = rect.h;
	}

skip_adjust:

	/*****/

	return (_rect_t){.x = px, .y = py, .w = pw, .h = ph};
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
_popup_grab_inputs(void)
{
	bool fail = false;

	/* no need to explicitely grab touch events as they get interpreted as pointer events after the grab */
	/* this also means that multitouch won't work on popups                                              */

	xcb_grab_keyboard_cookie_t xc1 =
		xcb_grab_keyboard(
			_x_con,
			0,
			_x_scr->root,
			XCB_CURRENT_TIME,
			XCB_GRAB_MODE_ASYNC,
			XCB_GRAB_MODE_ASYNC);

	xcb_grab_pointer_cookie_t xc2 = 
		xcb_grab_pointer(
			_x_con,
			0,
			_x_scr->root,
			XCB_EVENT_MASK_BUTTON_PRESS | XCB_EVENT_MASK_BUTTON_RELEASE | XCB_EVENT_MASK_POINTER_MOTION,
			XCB_GRAB_MODE_ASYNC,
			XCB_GRAB_MODE_ASYNC,
			XCB_NONE,
			XCB_NONE,
			XCB_CURRENT_TIME);

	xcb_grab_keyboard_reply_t *xr1 = xcb_grab_keyboard_reply(_x_con, xc1, NULL);
	xcb_grab_pointer_reply_t  *xr2 = xcb_grab_pointer_reply(_x_con,  xc2, NULL);

	fail = !xr1 || !xr2;
	fail = !xr2;
	if (fail) {
		dg_core_errno_set(DG_CORE_ERRNO_XCB);
		_popup_ungrab_inputs();
	}

	free(xr1);
	free(xr2);
	
	return !fail;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
_popup_kill(_popup_t *p)
{
	if (!p) {
		return;
	}

	_popup_t *p_next;

	_p_last = p->p_parent;
	if (!_p_last) {
		_popup_ungrab_inputs();
	} else {
		_p_last->p_child = NULL;
	}

	do {
		p_next = p->p_child;
		_p_hover = p_next == _p_hover ? NULL : _p_hover;
		dg_core_grid_destroy((dg_core_grid_t*)p->w->grids.ptr[0]);
		dg_core_window_destroy(p->w);
		free(p);
		p = p_next;
	} while (p);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static dg_core_window_t *
_popup_prep_core_input(xcb_key_press_event_t *x_ev)
{
	if (!_p_last || x_ev->event != _x_scr->root) {
		return NULL;
	}

	uint8_t type = x_ev->response_type & ~0x80;

	/* keyboard inputs, without coordinates, always goes to the last spawned popup */

	if (type == XCB_KEY_PRESS || type == XCB_KEY_RELEASE) {
		return _p_last->w;
	}

	/* check if an input was already ongoing                               */
	/* if the button action was out of bounds of all popups, kill them all */
	/* if the found popup is not the last one, kill all its childrens      */

	_popup_t *p;

	if (_p_last->w->buttons.n > 0) {
		goto skip_search;
	}

	p = _popup_find_under_coords(x_ev->event_x, x_ev->event_y);
	if (!p) {
		dg_core_popup_kill_all();
		return NULL;
	} else if (p != _p_last) {
		_popup_kill(p->p_child);
		return NULL;
	}

skip_search:

	/* transform input coordinates to popup's */

	x_ev->event_x -= _p_last->px;
	x_ev->event_y -= _p_last->py;

	return _p_last->w;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static dg_core_window_t *
_popup_prep_motion_input(xcb_motion_notify_event_t *x_ev)
{
	if (!_p_last || x_ev->event != _x_scr->root) {
		return NULL;
	}

	/* check if an input was already ongoing                                             */
	/* if it is use the popup that had the input, otherwhise find the popup under coords */

	dg_core_cell_event_t cev = {.kind = DG_CORE_CELL_EVENT_FOCUS_LOSE};
	_popup_t *p_prev = _p_hover;

	if (!_p_hover || _p_hover->w->buttons.n == 0) {
		_p_hover = _popup_find_under_coords(x_ev->event_x, x_ev->event_y);
	}

	if (p_prev && p_prev != _p_hover) {
		_window_send_event_to_all(p_prev->w, &cev);
	}

	if (!_p_hover) {
		return NULL;
	}

	/* transform input coordinates to popup's */

	x_ev->event_x -= _p_hover->px;
	x_ev->event_y -= _p_hover->py;

	return _p_hover->w;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void 
_popup_ungrab_inputs(void)
{
	_x_test_cookie(xcb_ungrab_keyboard_checked(_x_con, XCB_CURRENT_TIME), true);
	_x_test_cookie(xcb_ungrab_pointer_checked(_x_con,  XCB_CURRENT_TIME), true);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_sub_event_action_cell(dg_core_window_t *w, dg_core_config_action_t action)
{
	if (!w->a_focus) {
		return;
	}

	dg_core_cell_event_t cev = {0};

	switch (action) {
	
		case DG_CORE_CONFIG_ACTION_CELL_SELECT_LESS:
			cev.kind = DG_CORE_CELL_EVENT_SELECT_LESS;
			break;

		case DG_CORE_CONFIG_ACTION_CELL_SELECT_MORE:
			cev.kind = DG_CORE_CELL_EVENT_SELECT_MORE;
			break;

		case DG_CORE_CONFIG_ACTION_CELL_SELECT_NONE:
			cev.kind = DG_CORE_CELL_EVENT_SELECT_NONE;
			break;

		case DG_CORE_CONFIG_ACTION_CELL_SELECT_ALL:
			cev.kind = DG_CORE_CELL_EVENT_SELECT_ALL;	
			break;

		case DG_CORE_CONFIG_ACTION_CELL_TRIGGER_1:
			cev.kind = DG_CORE_CELL_EVENT_TRIGGER;	
			cev.trigger = 1;
			break;

		case DG_CORE_CONFIG_ACTION_CELL_TRIGGER_2:
			cev.kind = DG_CORE_CELL_EVENT_TRIGGER;	
			cev.trigger = 2;
			break;

		case DG_CORE_CONFIG_ACTION_CELL_TRIGGER_3:
			cev.kind = DG_CORE_CELL_EVENT_TRIGGER;	
			cev.trigger = 3;
			break;

		case DG_CORE_CONFIG_ACTION_CELL_TRIGGER_4:
			cev.kind = DG_CORE_CELL_EVENT_TRIGGER;	
			cev.trigger = 4;
			break;

		case DG_CORE_CONFIG_ACTION_CELL_TRIGGER_5:
			cev.kind = DG_CORE_CELL_EVENT_TRIGGER;	
			cev.trigger = 5;
			break;

		case DG_CORE_CONFIG_ACTION_CELL_REDRAW:
			if (w->a_focus) {
				w->a_focus->redraw = true;
				_window_set_present_schedule(w, _WINDOW_PRESENT_DEFAULT);
				_window_set_render_level(w, _WINDOW_RENDER_AREAS);
			}
			return;
		
		default:
			return;
	}
	
	_window_process_cell_event(w, w->a_focus, &cev);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_sub_event_action_focus(dg_core_window_t *w, dg_core_config_action_t action)
{
	_rect_t rect = {0};

	_window_set_focus_lock(w, false);

	/*****/

	if (action == DG_CORE_CONFIG_ACTION_FOCUS_NONE) {
		if (w->p_container) {
			_popup_kill(w->p_container);
		} else if (w->a_focus) {
			_window_set_focus(w, NULL);
		}
		return;
	}

	/* match conf action to subfocus type */

	dg_core_cell_subfocus_t subfocus;

	switch (action) {

		case DG_CORE_CONFIG_ACTION_FOCUS_LEFT:
			subfocus = DG_CORE_CELL_SUBFOCUS_LEFT;
			break;
		
		case DG_CORE_CONFIG_ACTION_FOCUS_RIGHT:
			subfocus = DG_CORE_CELL_SUBFOCUS_RIGHT;
			break;
		
		case DG_CORE_CONFIG_ACTION_FOCUS_UP:
			subfocus = DG_CORE_CELL_SUBFOCUS_UP;
			break;
		
		case DG_CORE_CONFIG_ACTION_FOCUS_DOWN:
			subfocus = DG_CORE_CELL_SUBFOCUS_DOWN;
			break;
		
		case DG_CORE_CONFIG_ACTION_FOCUS_LEFTMOST:
			subfocus = DG_CORE_CELL_SUBFOCUS_LEFTMOST;
			break;
		
		case DG_CORE_CONFIG_ACTION_FOCUS_RIGHTMOST:
			subfocus = DG_CORE_CELL_SUBFOCUS_RIGHTMOST;
			break;
		
		case DG_CORE_CONFIG_ACTION_FOCUS_TOP:
			subfocus = DG_CORE_CELL_SUBFOCUS_TOP;
			break;
		
		case DG_CORE_CONFIG_ACTION_FOCUS_BOTTOM:
			subfocus = DG_CORE_CELL_SUBFOCUS_BOTTOM;
			break;
	
		case DG_CORE_CONFIG_ACTION_FOCUS_NEXT:
			subfocus = DG_CORE_CELL_SUBFOCUS_NEXT;
			break;
		
		case DG_CORE_CONFIG_ACTION_FOCUS_PREV:
			subfocus = DG_CORE_CELL_SUBFOCUS_PREV;
			break;
		
		case DG_CORE_CONFIG_ACTION_FOCUS_FIRST:
			subfocus = DG_CORE_CELL_SUBFOCUS_FIRST;
			break;
		
		case DG_CORE_CONFIG_ACTION_FOCUS_LAST:
			subfocus = DG_CORE_CELL_SUBFOCUS_LAST;
			break;
		
		default:
			return;
	}

	/* first, if there is a focused cell, send the focus action to that cell in case its a meta-cell that */
	/* needs to update its internal focus                                                                 */

	dg_core_cell_event_t cev = {
		.kind = DG_CORE_CELL_EVENT_SUBFOCUS,
		.subfocus = subfocus,
	};

	if (_window_process_cell_event(w, w->a_focus, &cev)) {
		return;
	}

	/* if the subfocus event had been rejected or couldn't be done at all, then it means that either the */
	/* cell within the focused area is not a meta-cell, and if it is, the meta-cell can't update its     */
	/* internal focus (like if it is already on the "edge" of the cell                                   */

	/* in case the focused cell is a meta-cell, retrieve the pixel position of the subfocused cell */
	/* otherwhise, use the grid-level area coordinates                                             */

	dg_core_cell_event_t cev2 = {
		.kind = DG_CORE_CELL_EVENT_INFO_FOCUSED_CELL,
		.info_focus_cell = NULL,
	};

	if (_window_process_cell_event(w, w->a_focus, &cev2)) {
		rect.x = cev.info_focus_px;
		rect.y = cev.info_focus_py;
	} else if (w->a_focus) {
		rect = _area_get_current_geometry(w->a_focus, w);
	}

	/* find new grid-level area to focus                                                     */
	/* reiterate the search until the next area is found and the focus event is not rejected */

	_area_t *a = NULL;

	dg_core_cell_event_t cev3 = {
		.kind = DG_CORE_CELL_EVENT_FOCUS_GAIN_BY_ACTION,
		.focus_subfocus = subfocus,
		.focus_prev_px = rect.x,
		.focus_prev_py = rect.y,
	};

	if (!w->a_focus) {
		action = DG_CORE_CONFIG_ACTION_FOCUS_FIRST;
	}

	if (action == DG_CORE_CONFIG_ACTION_FOCUS_FIRST || action == DG_CORE_CONFIG_ACTION_FOCUS_LAST) {
		a = NULL;
	} else {
		a = w->a_focus;
	}

	do {
		switch (action) {

			case DG_CORE_CONFIG_ACTION_FOCUS_LEFT:
				a = _window_seek_focus_ortho(w, a, _FOCUS_SEEK_HORZ, _FOCUS_SEEK_CLOSE, _FOCUS_SEEK_DIR_A);
				break;
		
			case DG_CORE_CONFIG_ACTION_FOCUS_RIGHT:
				a = _window_seek_focus_ortho(w, a, _FOCUS_SEEK_HORZ, _FOCUS_SEEK_CLOSE, _FOCUS_SEEK_DIR_B);
				break;
		
			case DG_CORE_CONFIG_ACTION_FOCUS_UP:
				a = _window_seek_focus_ortho(w, a, _FOCUS_SEEK_VERT, _FOCUS_SEEK_CLOSE, _FOCUS_SEEK_DIR_A);
				break;
		
			case DG_CORE_CONFIG_ACTION_FOCUS_DOWN:
				a = _window_seek_focus_ortho(w, a, _FOCUS_SEEK_VERT, _FOCUS_SEEK_CLOSE, _FOCUS_SEEK_DIR_B);
				break;
		
			case DG_CORE_CONFIG_ACTION_FOCUS_LEFTMOST:
				a = _window_seek_focus_ortho(w, a, _FOCUS_SEEK_HORZ, _FOCUS_SEEK_FAR, _FOCUS_SEEK_DIR_A);
				break;
		
			case DG_CORE_CONFIG_ACTION_FOCUS_RIGHTMOST:
				a = _window_seek_focus_ortho(w, a, _FOCUS_SEEK_HORZ, _FOCUS_SEEK_FAR, _FOCUS_SEEK_DIR_B);
				break;
		
			case DG_CORE_CONFIG_ACTION_FOCUS_TOP:
				a = _window_seek_focus_ortho(w, a, _FOCUS_SEEK_VERT, _FOCUS_SEEK_FAR, _FOCUS_SEEK_DIR_A);
				break;
		
			case DG_CORE_CONFIG_ACTION_FOCUS_BOTTOM:
				a = _window_seek_focus_ortho(w, a, _FOCUS_SEEK_VERT, _FOCUS_SEEK_FAR, _FOCUS_SEEK_DIR_B);
				break;
	
			case DG_CORE_CONFIG_ACTION_FOCUS_NEXT:
				a = _window_seek_focus_logic(w, a, _FOCUS_SEEK_DIR_A);
				break;
		
			case DG_CORE_CONFIG_ACTION_FOCUS_PREV:
				a = _window_seek_focus_logic(w, a, _FOCUS_SEEK_DIR_B);
				break;
		
			case DG_CORE_CONFIG_ACTION_FOCUS_FIRST:
				a = _window_seek_focus_logic(w, a, _FOCUS_SEEK_DIR_A);
				break;
		
			case DG_CORE_CONFIG_ACTION_FOCUS_LAST:
				a = _window_seek_focus_logic(w, a, _FOCUS_SEEK_DIR_B);
				break;
		
			default:
				return;
		}

	} while (a && !_window_process_cell_event(w, a, &cev3));

	/* if new area is found set the focus */

	if (a) {
		_window_set_focus(w, a);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_sub_event_action_misc(dg_core_config_action_t action)
{
	switch (action) {
		
		case DG_CORE_CONFIG_ACTION_MISC_RECONFIG:
			_misc_reconfig();
			break;

		case DG_CORE_CONFIG_ACTION_MISC_EXIT:
			if (_allow_user_exit) {
				dg_core_loop_abort();
			}
			break;

		default:
			return;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_sub_event_action_window(dg_core_window_t *w, dg_core_config_action_t action)
{
	switch (action) {

		case DG_CORE_CONFIG_ACTION_WINDOW_LOCK_GRID:
			_window_toggle_state(w, DG_CORE_WINDOW_STATE_LOCKED_GRID);
			_window_set_present_schedule(w, _WINDOW_PRESENT_DEFAULT);
			_window_set_render_level(w, _WINDOW_RENDER_BORDER);
			_window_update_current_grid(w);
			_window_update_wm_size_hints(w);
			break;
		
		case DG_CORE_CONFIG_ACTION_WINDOW_LOCK_FOCUS:
			_window_set_focus_lock(w, !(w->state & DG_CORE_WINDOW_STATE_LOCKED_FOCUS));
			break;

		case DG_CORE_CONFIG_ACTION_WINDOW_REDRAW:
			_window_set_present_schedule(w, _WINDOW_PRESENT_DEFAULT);
			_window_set_render_level(w, _WINDOW_RENDER_FULL);
			break;

		default:
			return;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_sub_event_clipboard(dg_core_window_t *w, uint8_t type, uint8_t clipboard)
{
	dg_core_cell_event_t cev = {
		.clipboard = clipboard,
	};

	switch (type) {
		case DG_CORE_CONFIG_SWAP_TO_CLIPBOARD_CUT:
			cev.kind = DG_CORE_CELL_EVENT_CLIPBOARD_CUT;
			break;

		case DG_CORE_CONFIG_SWAP_TO_CLIPBOARD_COPY:
			cev.kind = DG_CORE_CELL_EVENT_CLIPBOARD_COPY;
			break;

		case DG_CORE_CONFIG_SWAP_TO_CLIPBOARD_PASTE:
			cev.kind = DG_CORE_CELL_EVENT_CLIPBOARD_PASTE;
			break;

		default:
			return;
	}
	
	_window_process_cell_event(w, w->a_focus, &cev);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_sub_event_input_button(dg_core_window_t *w, uint8_t button, xcb_key_press_event_t *x_ev)
{
	/* update focus first */

	_window_focus_by_pointer(w, x_ev->event_x, x_ev->event_y);

	/* prepare and send cell event, as well as update button trackers */
	
	dg_core_cell_event_t cev = {
		.button_id = button,
		.button_px = x_ev->event_x,
		.button_py = x_ev->event_y,
	};

	if ((x_ev->response_type & ~0x80) == XCB_BUTTON_PRESS) {
		dg_core_input_buffer_push_coord(&w->buttons, button, x_ev->event_x, x_ev->event_y);	
		cev.kind = DG_CORE_CELL_EVENT_BUTTON_PRESS;
	} else {
		dg_core_input_buffer_pull(&w->buttons, button);	
		cev.kind = DG_CORE_CELL_EVENT_BUTTON_RELEASE;
	}

	cev.button_n = w->buttons.n;
	_window_process_cell_event(w, w->a_focus, &cev);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_sub_event_input_key(dg_core_window_t *w, uint8_t keycode, xcb_key_press_event_t *x_ev)
{
	int keymap_shift = 0;

	if (x_ev->state & XCB_MOD_MASK_5) {
		keymap_shift += 2;
	}

	if ((x_ev->state & (XCB_MOD_MASK_SHIFT | XCB_MOD_MASK_LOCK)) &&
	    (!(x_ev->state & XCB_MOD_MASK_SHIFT) != !(x_ev->state & XCB_MOD_MASK_LOCK))) {
		keymap_shift += 1;
	}

	/* compose event */

	dg_core_cell_event_t cev = {
		.key_code  = keycode,
		.key_sym   = xcb_key_symbols_get_keysym(_x_ksm, keycode, keymap_shift),
		.key_utf32 = xkb_keysym_to_utf32(cev.key_sym),
		.key_utf8  = {0},
	};

	xkb_keysym_to_utf8(cev.key_sym, cev.key_utf8, 8);

	if ((x_ev->response_type & ~0x80) == XCB_KEY_PRESS) {
		cev.kind = DG_CORE_CELL_EVENT_KEY_PRESS;
	} else {
		cev.kind = DG_CORE_CELL_EVENT_KEY_RELEASE;
	}

	_window_process_cell_event(w, w->a_focus, &cev);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_sub_event_touch_begin(dg_core_window_t *w, uint32_t id, int16_t px, int16_t py)
{
	_area_t *a = _window_find_area_under_coords(w, px, py);
	if (!a) {
		return;
	}

	/* if its the first active touch then update the primary focus */

	dg_core_cell_event_t cev = {
		.kind = DG_CORE_CELL_EVENT_FOCUS_GAIN_BY_TOUCH,
		.focus_px = px,
		.focus_py = py,
	};

	if (w->touches.n == 0 && !(w->state & DG_CORE_WINDOW_STATE_LOCKED_FOCUS)) {
	    if (_window_process_cell_event(w, a, &cev)) {
			_window_set_focus(w, a);
		} else {
			return;
		}
	}

	/* send touch event and update touch trackers */

	dg_core_cell_event_t cev2 = {
		.kind = DG_CORE_CELL_EVENT_TOUCH_BEGIN,
		.touch_id = id,
		.touch_px = px,
		.touch_py = py,
		.touch_dpx = 0,
		.touch_dpy = 0,
		.touch_n   = a->touches.n + 1,
	};

	dg_core_input_buffer_push_ref(&w->touches, id, a);
	dg_core_input_buffer_push_coord(&a->touches, id, px, py);

	_window_process_cell_event(w, a, &cev2);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_sub_event_touch_end(dg_core_window_t *w, uint32_t id, int16_t px, int16_t py)
{
	_area_t *a;

	/* locate area on which the touch was started      */
	/* then check if the touch is tracked in said area */
	/* also remove the touch from input trackers       */

	size_t pos = 0;
	if (!dg_core_input_buffer_find(&w->touches, id, &pos)) {
		return;
	}

	a = (_area_t*)w->touches.inputs[pos].ref;
	dg_core_input_buffer_pull(&w->touches, id);

	if (!a || !dg_core_input_buffer_find(&a->touches, id, NULL)) {
		return;
	}

	dg_core_input_buffer_pull(&a->touches, id);
	
	/* send touch event */

	dg_core_cell_event_t cev = {
		.kind = DG_CORE_CELL_EVENT_TOUCH_END,
		.touch_id = id,
		.touch_px = px,
		.touch_py = py,
		.touch_dpx = 0,
		.touch_dpy = 0,
		.touch_n   = a->touches.n,
	};

	_window_process_cell_event(w, a, &cev);

	/* if it is the last touch remaining on the focused area then update the focus */

	if (a == w->a_focus && a->touches.n == 0 &&
	    !(w->state & DG_CORE_WINDOW_STATE_LOCKED_FOCUS) &&
		!DG_CORE_CONFIG->input_persistent_touch) {
		_window_set_focus(w, NULL);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_sub_event_touch_update(dg_core_window_t *w, uint32_t id, int16_t px, int16_t py)
{
	_area_t *a;
	size_t pos;

	/* locate area on which the touch was started      */
	/* then check if the touch is tracked in said area */

	pos = 0;
	if (!dg_core_input_buffer_find(&w->touches, id, &pos)) {
		return;
	}

	pos = 0;
	a = (_area_t*)w->touches.inputs[pos].ref;
	if (!dg_core_input_buffer_find(&a->touches, id, &pos)) {
		return;
	}
	
	/* send touch event */

	dg_core_cell_event_t cev = {
		.kind = DG_CORE_CELL_EVENT_TOUCH_UPDATE,
		.touch_id = id,
		.touch_px = px,
		.touch_py = py,
		.touch_dpx = px - a->touches.inputs[pos].x,
		.touch_dpy = py - a->touches.inputs[pos].y,
		.touch_n   = a->touches.n,
	};

	_window_process_cell_event(w, a, &cev);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_window_destroy(dg_core_window_t *w)
{
	if (!w->to_destroy) {
		return;
	}

	dg_core_cell_event_t cev = {.kind = DG_CORE_CELL_EVENT_CANCEL};

	_window_send_event_to_all(w, &cev);

	cairo_destroy(w->c_ctx);
	cairo_surface_destroy(w->c_srf);

	_x_test_cookie(xcb_unmap_window_checked(_x_con, w->x_win),   true);
	_x_test_cookie(xcb_destroy_window_checked(_x_con, w->x_win), true);

	dg_core_input_buffer_reset(&w->buttons);
	dg_core_input_buffer_reset(&w->touches);
	dg_core_stack_reset(&w->grids);
	dg_core_stack_pull(&_windows, w);
	free(w);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static dg_core_window_t *
_window_create(bool fixed)
{
	xcb_void_cookie_t xc;

	dg_core_window_t *w = malloc(sizeof(dg_core_window_t));
	if (!w) {
		dg_core_errno_set(DG_CORE_ERRNO_MEMORY);
		goto fail_alloc;
	}

	/* initialize internal variables */

	w->render_level     = _WINDOW_RENDER_NONE;
	w->present_schedule = _WINDOW_PRESENT_NONE;
	w->present_serial   = 0;
	w->last_render_time = 0;

	w->id          = 0;
	w->to_destroy  = false;
	w->name        = _class[0];
	w->name_icon   = _class[0];
	w->state       = DG_CORE_WINDOW_STATE_INITIAL;
	w->fixed       = fixed;
	w->grids       = DG_CORE_STACK_EMPTY;
	w->p_container = NULL;
	w->g_current   = NULL;
	w->a_focus     = NULL;
	w->px          = 0;
	w->py          = 0;
	w->pw          = 400;
	w->ph          = 400;
	w->cw_extra    = 0;
	w->ch_extra    = 0;

	w->callback_close  = NULL;
	w->callback_focus  = NULL;
	w->callback_grid   = NULL;
	w->callback_state  = NULL;
	w->callback_redraw = NULL;

	for (size_t i = 0 ; i < DG_CORE_CONFIG_MAX_ACCELS; i++) {
		w->accels[i].name = NULL;
		w->accels[i].fn   = NULL;
	}

	/* setup input buffers */

	bool err = false;

	err |= !dg_core_input_buffer_init(&w->buttons, DG_CORE_INPUT_BUFFER_MAX_BUTTONS, DG_CORE_INPUT_BUFFER_COORD);
	err |= !dg_core_input_buffer_init(&w->touches, DG_CORE_INPUT_BUFFER_MAX_TOUCHES, DG_CORE_INPUT_BUFFER_REF);
	if (err) {
		goto fail_buffers;
	}

	/* setup X window */

	const uint32_t mask_vals[] = {
		0x00000000,
		0x00000000,
		fixed,
		XCB_EVENT_MASK_EXPOSURE              |
			XCB_EVENT_MASK_POINTER_MOTION    |
			XCB_EVENT_MASK_BUTTON_PRESS      |
			XCB_EVENT_MASK_BUTTON_RELEASE    |
			XCB_EVENT_MASK_ENTER_WINDOW      |
			XCB_EVENT_MASK_LEAVE_WINDOW      |
			XCB_EVENT_MASK_KEYMAP_STATE      |
			XCB_EVENT_MASK_KEY_PRESS         |
			XCB_EVENT_MASK_KEY_RELEASE       |
			XCB_EVENT_MASK_FOCUS_CHANGE      |
			XCB_EVENT_MASK_VISIBILITY_CHANGE |
			XCB_EVENT_MASK_PROPERTY_CHANGE   |
			XCB_EVENT_MASK_STRUCTURE_NOTIFY,
		_x_clm,
	};

	w->x_win = xcb_generate_id(_x_con);
	xc = xcb_create_window_checked(
		_x_con,
		_x_dph->depth,
		w->x_win,
		_x_scr->root,
		w->px, w->py,
		w->pw, w->ph,
		0,
		XCB_WINDOW_CLASS_INPUT_OUTPUT,
		_x_vis->visual_id,
		XCB_CW_BACK_PIXEL | XCB_CW_BORDER_PIXEL | XCB_CW_OVERRIDE_REDIRECT | XCB_CW_EVENT_MASK | XCB_CW_COLORMAP,
		mask_vals);

	if (_x_test_cookie(xc, false)) {
		dg_core_errno_set(DG_CORE_ERRNO_XCB_CRIT);
		goto fail_x_win;
	}

	/* setup a cairo front surface linked to the window */

	w->c_srf = cairo_xcb_surface_create(_x_con, w->x_win, _x_vis, w->pw, w->ph);
	if (cairo_surface_status(w->c_srf) != CAIRO_STATUS_SUCCESS) {
		dg_core_errno_set(DG_CORE_ERRNO_CAIRO_CRIT);
		goto fail_cairo_srf;
	}

	w->c_ctx = cairo_create(w->c_srf);
	if (cairo_status(w->c_ctx) != CAIRO_STATUS_SUCCESS) {
		dg_core_errno_set(DG_CORE_ERRNO_CAIRO_CRIT);
		goto fail_cairo_ctx;
	}

	cairo_set_operator(w->c_ctx, CAIRO_OPERATOR_SOURCE);

	/* indicate that the window should receive Present extension events */

	xc = xcb_present_select_input_checked(
		_x_con,
		xcb_generate_id(_x_con),
		w->x_win,
		XCB_PRESENT_EVENT_MASK_COMPLETE_NOTIFY);

	if (_x_test_cookie(xc, false)) {
		dg_core_errno_set(DG_CORE_ERRNO_XCB_CRIT);
		goto fail_present;
	}

	/* indicate that the window should receive XI touch extension events */

	struct {
    	xcb_input_event_mask_t head;
    	xcb_input_xi_event_mask_t mask;
	} mask;

	mask.head.deviceid = XCB_INPUT_DEVICE_ALL_MASTER;
	mask.head.mask_len = 1;

	mask.mask =
		XCB_INPUT_XI_EVENT_MASK_TOUCH_BEGIN  |
		XCB_INPUT_XI_EVENT_MASK_TOUCH_END    |
		XCB_INPUT_XI_EVENT_MASK_TOUCH_UPDATE;

	xc = xcb_input_xi_select_events(_x_con, w->x_win, 1, (xcb_input_event_mask_t*)(&mask));

	if (_x_test_cookie(xc, false)) {
		dg_core_errno_set(DG_CORE_ERRNO_XCB_CRIT);
		goto fail_xi;
	}

	/* add new window to session */

	if (!dg_core_stack_push(&_windows, w, &w->id)) {
		goto fail_push;
	}

	/* set window's X properties */

	const size_t vers_n = strlen(DG_CORE_VERSION);
	const size_t cls0_n = strlen(_class[0]) + 1;
	const size_t cls1_n = strlen(_class[1]);
	const size_t pid    = getpid();

	char   host[256] = "";
	size_t host_n;

	gethostname(host, 256);
	host_n = strlen(host);

	_x_set_prop(false, w->x_win, _xa_host, XCB_ATOM_STRING,   host_n, host);
	_x_set_prop(false, w->x_win, _xa_vers, XCB_ATOM_STRING,   vers_n, DG_CORE_VERSION);
	_x_set_prop(false, w->x_win, _xa_cls,  XCB_ATOM_STRING,   cls0_n, _class[0]);
	_x_set_prop(true,  w->x_win, _xa_cls,  XCB_ATOM_STRING,   cls1_n, _class[1]);
	_x_set_prop(false, w->x_win, _xa_lead, XCB_ATOM_WINDOW,   1,      &_x_win_l);
	_x_set_prop(false, w->x_win, _xa_pid,  XCB_ATOM_CARDINAL, 1,      &pid);
	_x_set_prop(false, w->x_win, _xa_prot, XCB_ATOM_ATOM,     1,      &_xa_del);
	_x_set_prop(true,  w->x_win, _xa_prot, XCB_ATOM_ATOM,     1,      &_xa_foc);
	_x_set_prop(true,  w->x_win, _xa_prot, XCB_ATOM_ATOM,     1,      &_xa_ping);
	_x_set_prop(true,  w->x_win, _xa_prot, XCB_ATOM_ATOM,     1,      &_xa_sig);

	dg_core_window_rename(w, w->name, w->name_icon);

	/* end */

	cairo_surface_flush(w->c_srf);
	xcb_flush(_x_con);

	return w;

	/* errors */

fail_push:
fail_xi:
fail_present:
	cairo_destroy(w->c_ctx);
fail_cairo_ctx:
	cairo_surface_destroy(w->c_srf);
fail_cairo_srf:
	xcb_destroy_window(_x_con, w->x_win);
fail_x_win:
	xcb_flush(_x_con);
	dg_core_input_buffer_reset(&w->buttons);
	dg_core_input_buffer_reset(&w->touches);
fail_buffers:
	free(w);
fail_alloc:
	return NULL;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static _area_t *
_window_find_area_under_coords(dg_core_window_t *w, int16_t px, int16_t py)
{
	_area_t *a;
	_rect_t  rect;

	for (size_t i = w->g_current->areas.n - 1; i < w->g_current->areas.n; i--) {
		a    = (_area_t*)w->g_current->areas.ptr[i];
		rect = _area_get_current_geometry(a, w);
		if (dg_core_util_test_bounds(px, py, rect.x, rect.y, rect.w, rect.h)) {
			return a;
		}
	}

	return NULL;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static dg_core_grid_t *
_window_find_smallest_grid(dg_core_window_t *w)
{
	dg_core_grid_t *g_min = (dg_core_grid_t*)w->grids.ptr[0];
	dg_core_grid_t *g;

	for (int i = 1; i < w->grids.n; i++) {
		g = (dg_core_grid_t*)w->grids.ptr[i];
		if (g->cw <= g_min->cw && g->ch <= g_min->ch) {
			g_min = g;
		}
	}

	return g_min;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_window_focus_by_pointer(dg_core_window_t *w, int16_t px, int16_t py)
{
	/* do not update focus if there is an ongoing drag action or if the focus is locked */

	if (w->buttons.n > 0 || w->state & DG_CORE_WINDOW_STATE_LOCKED_FOCUS) {
		return;
	}

	/* generate and send focus event */

	_area_t *a = _window_find_area_under_coords(w, px, py);

	dg_core_cell_event_t cev = {
		.kind = DG_CORE_CELL_EVENT_FOCUS_GAIN_BY_POINTER,
		.focus_px = px,
		.focus_py = py,
	};

	if (_window_process_cell_event(w, a, &cev)) {
		_window_set_focus(w, a);
	} else if (!DG_CORE_CONFIG->input_persistent_pointer) {
		_window_set_focus(w, NULL);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static dg_core_color_t
_window_get_border_color(dg_core_window_t *w)
{
	if (!DG_CORE_CONFIG->win_dynamic_bd || !(w->state & DG_CORE_WINDOW_STATE_FOCUSED)) {
		return DG_CORE_CONFIG->win_cl_bd;
	}

	if ((w->state & DG_CORE_WINDOW_STATE_DISABLED)) {
		return DG_CORE_CONFIG->win_cl_bd_disabled;
	}

	if ((w->state & DG_CORE_WINDOW_STATE_LOCKED_GRID)) {
		return DG_CORE_CONFIG->win_cl_bd_locked;
	}

	return DG_CORE_CONFIG->win_cl_bd_focused;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_window_present(dg_core_window_t *w)
{
	if ((w->state & DG_CORE_WINDOW_STATE_OBSCURED) || !(w->state & DG_CORE_WINDOW_STATE_MAPPED)) {
		w->present_schedule = _WINDOW_PRESENT_NONE;
	}

	switch (w->present_schedule) {
		
		case _WINDOW_PRESENT_NONE:
			break;

		case _WINDOW_PRESENT_IMMEDIATE:
			_window_redraw(w);			
			break;

		case _WINDOW_PRESENT_DEFAULT:
			xcb_present_notify_msc(_x_con, w->x_win, ++w->present_serial, 0, DG_CORE_CONFIG->anim_divider, 0);
			w->present_schedule = _WINDOW_PRESENT_NONE;
			break;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
_window_process_cell_event(dg_core_window_t *w, _area_t *a, dg_core_cell_event_t *cev)
{
	if (!a || !a->c || !a->c->fn_event) {
		return false;
	}

	if (cev->kind == DG_CORE_CELL_EVENT_NONE) {
		return false;
	}

	/* event filtering depending on cell or window state */

	if ((!a->c->ena || w->state & DG_CORE_WINDOW_STATE_DISABLED) && 
		cev->kind != DG_CORE_CELL_EVENT_INFO_FOCUSED_CELL &&
		cev->kind != DG_CORE_CELL_EVENT_SEEK_CELL &&
		cev->kind != DG_CORE_CELL_EVENT_WINDOW_ENABLE &&
		cev->kind != DG_CORE_CELL_EVENT_WINDOW_DISABLE) {
		return false;
	}

	/* fill up commom fields */

	_rect_t cell_region = _area_get_current_geometry(a, w);

	cev->msg            = DG_CORE_CELL_EVENT_MSG_NONE;
	cev->focus          = _area_get_focus_type(a, w);
	cev->w_host         = w;
	cev->cell_px        = cell_region.x;
	cev->cell_py        = cell_region.y;
	cev->cell_pw        = cell_region.w;
	cev->cell_ph        = cell_region.h;
	cev->is_enabled     = a->c->ena;
	cev->win_is_enabled = !(w->state & DG_CORE_WINDOW_STATE_DISABLED),

	/* send event */

	a->c->fn_event(a->c, cev);

	/* process msg */

	if (cev->msg & DG_CORE_CELL_EVENT_MSG_REJECT) {
		return false;
	}

	if (cev->msg & DG_CORE_CELL_EVENT_MSG_REQUEST_UPDATE) {
		_window_set_render_level(w, _WINDOW_RENDER_AREAS);
		_window_set_present_schedule(w, _WINDOW_PRESENT_DEFAULT);
		a->redraw = true;
	}

	if (cev->msg & DG_CORE_CELL_EVENT_MSG_REQUEST_LOCK && a == w->a_focus && DG_CORE_CONFIG->cell_auto_lock) {
		_window_set_focus_lock(w, true);
	}

	if (cev->msg & DG_CORE_CELL_EVENT_MSG_REQUEST_UNLOCK && a == w->a_focus && DG_CORE_CONFIG->cell_auto_lock) {
		_window_set_focus_lock(w, false);
	}
		
	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_window_redraw(dg_core_window_t *w)
{
	if (w->render_level == _WINDOW_RENDER_NONE) {
		return;
	}

	const unsigned long timestamp = dg_core_util_get_time();
	const unsigned long delay     = timestamp - w->last_render_time;
	const int16_t       l         = DG_CORE_CONFIG->win_thick_bd;

	dg_core_color_t cl;
	_area_t *a;

	w->last_render_time = timestamp;
	cairo_set_operator(w->c_ctx, CAIRO_OPERATOR_SOURCE);

	/* redraw background */

	if (w->render_level == _WINDOW_RENDER_FULL && !w->p_container) {
		cl = DG_CORE_CONFIG->win_cl_bg;
		cairo_set_source_rgba(w->c_ctx, cl.r, cl.g, cl.b, cl.a);
		cairo_rectangle(w->c_ctx, l, l, w->pw - 2 * l, w->ph - 2 * l);
		cairo_fill(w->c_ctx);
	}

	/* redraw border */

	if (l > 0 && w->render_level != _WINDOW_RENDER_AREAS && !w->p_container) {
		cl = _window_get_border_color(w);
		cairo_set_source_rgba(w->c_ctx, cl.r, cl.g, cl.b, cl.a);
		cairo_rectangle(w->c_ctx, 0, 0, w->pw, l);
		cairo_rectangle(w->c_ctx, 0, l, l, w->ph - 2 * l);
		cairo_rectangle(w->c_ctx, w->pw, l, -l, w->ph - 2 * l);
		cairo_rectangle(w->c_ctx, 0, w->ph, w->pw, -l);
		cairo_fill(w->c_ctx);
	}

	/* first repaint cells with no focus at all               */
	/* then repaint cells with secondary (touch) focus if any */
	/* finaly, always render the primarily focused cell last  */

	if (w->render_level == _WINDOW_RENDER_BORDER) {
		goto skip_areas;
	}

	for (size_t i = 0; i < w->g_current->areas.n; i++) {
		a = (_area_t*)w->g_current->areas.ptr[i];
		if (_area_get_focus_type(a, w) == DG_CORE_CELL_FOCUS_NONE) {
			_area_redraw(a, w, delay);
		}
	}

	for (size_t i = 0; i < w->g_current->areas.n; i++) {
		a = (_area_t*)w->g_current->areas.ptr[i];
		if (_area_get_focus_type(a, w) == DG_CORE_CELL_FOCUS_SECONDARY) {
			_area_redraw(a, w, delay);
		}
	}

	if (w->a_focus) {
		_area_redraw(w->a_focus, w, delay);
	}

skip_areas:

	/* check if there is any requests for a new render cycle */

	w->present_schedule = _WINDOW_PRESENT_NONE;
	w->render_level     = _WINDOW_RENDER_NONE;

	for (size_t i = 0; i < w->g_current->areas.n; i++) {
		if (((_area_t*)w->g_current->areas.ptr[i])->redraw) {
			w->present_schedule = _WINDOW_PRESENT_DEFAULT;
			w->render_level     = _WINDOW_RENDER_AREAS;
			break;
		}
	}

	/* run redraw callback */

	_RUN_FN(w->callback_redraw, w, delay);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_window_refocus(dg_core_window_t *w)
{
	if (!w->a_focus) {
		return;
	}

	dg_core_cell_t *c;

	/* first check if the focused area is not a meta-cell with a subfocus */
	/* if it is, set the cell to focus to the subfocused one              */

	dg_core_cell_event_t cev = {
		.kind = DG_CORE_CELL_EVENT_INFO_FOCUSED_CELL,
		.info_focus_cell = NULL,
	};

	c = _window_process_cell_event(w, w->a_focus, &cev) ? cev.info_focus_cell : w->a_focus->c;

	/* find first area with matching cell */
	/* if none is found, lose focus       */

	_area_t *a = _grid_find_first_area(w->g_current, c);

	dg_core_cell_event_t cev2 = {
		.kind = DG_CORE_CELL_EVENT_FOCUS_GAIN_BY_REFERENCE,
		.focus_cell = c,
	};

	if (!_window_process_cell_event(w, a, &cev2)) {
		_window_set_focus(w, NULL);
		return;
	}
	
	w->a_focus = a;
	_window_update_wm_focus_hints(w);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_window_resize(dg_core_window_t *w, int16_t pw, int16_t ph)
{
	w->pw = pw;
	w->ph = ph;

	cairo_surface_flush(w->c_srf);
	cairo_xcb_surface_set_size(w->c_srf, w->pw, w->ph);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static _area_t *
_window_seek_focus_logic(dg_core_window_t *w, _area_t *a_start, _focus_seek_param_t dir)
{
	/* bounds */

	size_t id_start;
	size_t id_end;

	if (dir > 0) {
		id_start = a_start ? a_start->id + 1: 0;
		id_end   = w->g_current->areas.n;
	} else {
		id_start = a_start ? a_start->id - 1: w->g_current->areas.n - 1;
		id_end   = SIZE_MAX;
	}

	/* seek */

	size_t id = id_start;

	_area_t *a = NULL;

	while (!a && id != id_end) {
		if (((_area_t*)w->g_current->areas.ptr[id])->c->fn_event) {
			a = (_area_t*)w->g_current->areas.ptr[id];
		}
		id += dir;
	}

	return a;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static _area_t *
_window_seek_focus_ortho(
	dg_core_window_t *w,
	_area_t *a_start,
	_focus_seek_param_t axis,
	_focus_seek_param_t side,
	_focus_seek_param_t dir)
{
	_area_t *a;
	_area_t *a_new = NULL;

	if (axis == _FOCUS_SEEK_VERT) {
		goto vertical;
	}

	/* horizontal seek */

	for (size_t i = 0; i < w->g_current->areas.n; i++) {
		a = (_area_t*)w->g_current->areas.ptr[i];
		if (a->c->fn_event && dir * a->cx < dir * a_start->cx && a->cy < a_start->cy + a_start->ch && a->cy + a->ch > a_start->cy) {
			if (!a_new || dir * side * a->cx > dir * side * a_new->cx || (a->cx == a_new->cx && a->cy < a_new->cy)) {
				a_new = a;
			}
		}
	}

	return a_new;

	/* vertical seek */

vertical:

	for (size_t i = 0; i < w->g_current->areas.n; i++) {
		a = (_area_t*)w->g_current->areas.ptr[i];
		if (a->c->fn_event && dir * a->cy < dir * a_start->cy && a->cx < a_start->cx + a_start->cw && a->cx + a->cw > a_start->cx) {
			if (!a_new || dir * side * a->cy > dir * side * a_new->cy || (a->cy == a_new->cy && a->cx < a_new->cx)) {
				a_new = a;
			}
		}
	}

	return a_new;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_window_send_event_to_all(dg_core_window_t *w, dg_core_cell_event_t *cev)
{
	if (!w->g_current) {
		return;
	}

	dg_core_cell_event_t cev_copy;

	for (size_t i = 0; i < w->g_current->areas.n; i++) {
		cev_copy = *cev;
		_window_process_cell_event(w, (_area_t*)w->g_current->areas.ptr[i], &cev_copy);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_window_set_focus(dg_core_window_t *w, _area_t *a)
{
	if (a == w->a_focus) {
		return;
	}

	/* first sent a lost focus event to the previously focused cell */

	dg_core_cell_event_t cev = {.kind = DG_CORE_CELL_EVENT_FOCUS_LOSE};

	_window_process_cell_event(w, w->a_focus, &cev);

	/* it's assumed that the focus event was sent by the caller of this function */

	w->a_focus = a;

	_window_update_wm_focus_hints(w);

	/* run focus callback with focused cell and update lock if there is none         */
	/* for the callback check if the focused area is not a meta-cell with a subfocus */
	/* if it is, set the callback's focus cell to the subfocused one                 */

	dg_core_cell_t *c = NULL;

	dg_core_cell_event_t cev2 = {
		.kind = DG_CORE_CELL_EVENT_INFO_FOCUSED_CELL,
		.info_focus_cell = NULL,
	};

	if (_window_process_cell_event(w, w->a_focus, &cev2)) {
		c = cev2.info_focus_cell;
	} else if (w->a_focus) {
		c = w->a_focus->c;
	} else {
		_window_set_focus_lock(w, false);
	}

	_RUN_FN(w->callback_focus, w, c);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_window_set_focus_lock(dg_core_window_t *w, bool lock)
{
	if (!w->a_focus || lock == ((w->state & DG_CORE_WINDOW_STATE_LOCKED_FOCUS) > 0)) {
		return;
	}

	dg_core_cell_event_t cev = {
		.kind = lock ? DG_CORE_CELL_EVENT_FOCUS_LOCK : DG_CORE_CELL_EVENT_FOCUS_UNLOCK
	};

	if (!_window_process_cell_event(w, w->a_focus, &cev)) {
		return;
	}

	_window_toggle_state(w, DG_CORE_WINDOW_STATE_LOCKED_FOCUS);
	_window_set_present_schedule(w, _WINDOW_PRESENT_DEFAULT);
	_window_set_render_level(w, _WINDOW_RENDER_AREAS);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_window_set_present_schedule (dg_core_window_t *w, _window_present_schedule_t present_schedule)
{
	if (w->present_schedule != _WINDOW_PRESENT_IMMEDIATE && present_schedule != _WINDOW_PRESENT_NONE) {
		w->present_schedule = present_schedule;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_window_set_render_level (dg_core_window_t *w, _window_render_level_t render_level)
{
	if (render_level == w->render_level || w->render_level == _WINDOW_RENDER_FULL) {
		return;
	}

	switch (render_level) {

		case _WINDOW_RENDER_FULL:
			w->render_level = render_level;
			break;

		case _WINDOW_RENDER_BORDER:
		case _WINDOW_RENDER_AREAS:
			w->render_level = w->render_level == _WINDOW_RENDER_NONE ? render_level : _WINDOW_RENDER_BOTH;
			break;

		case _WINDOW_RENDER_BOTH:
			w->render_level = render_level;
			break;

		case _WINDOW_RENDER_NONE:
			break;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_window_set_state(dg_core_window_t *w, dg_core_window_state_t state, dg_core_window_state_setting_mode_t mode)
{
	const int s = w->state;

	w->state = mode == DG_CORE_WINDOW_SET_STATE ? w->state | state : w->state & ~state;

	if (s == w->state) {
		return;
	}

	_RUN_FN(w->callback_state, w, state, mode);

	/* request repaints for state updates that change the appearance of the window's border and background */

	if (DG_CORE_CONFIG->win_dynamic_bd &&
		(state & (DG_CORE_WINDOW_STATE_FOCUSED  | 
		          DG_CORE_WINDOW_STATE_DISABLED | 
				  DG_CORE_WINDOW_STATE_LOCKED_GRID))) {
		_window_set_render_level(w, _WINDOW_RENDER_BORDER);
		_window_set_present_schedule(w, _WINDOW_PRESENT_DEFAULT);
	}

	/* update _DG_CORE_WINDOW_STATE X11 property if any changed states are DG specific */

	if (!(state & (
		DG_CORE_WINDOW_STATE_ACTIVE       |
		DG_CORE_WINDOW_STATE_DISABLED     |
		DG_CORE_WINDOW_STATE_LOCKED_GRID |
		DG_CORE_WINDOW_STATE_LOCKED_FOCUS))) {
		return;
	}

	_x_test_cookie(xcb_delete_property_checked(_x_con, w->x_win, _xa_stt), true);

	if (w->state & DG_CORE_WINDOW_STATE_ACTIVE) {
		_x_set_prop(true, w->x_win, _xa_stt, XCB_ATOM_ATOM, 1, &_xa_won);
	}

	if (w->state & DG_CORE_WINDOW_STATE_DISABLED) {
		_x_set_prop(true, w->x_win, _xa_stt, XCB_ATOM_ATOM, 1, &_xa_wena);
	}

	if (w->state & DG_CORE_WINDOW_STATE_LOCKED_GRID) {
		_x_set_prop(true, w->x_win, _xa_stt, XCB_ATOM_ATOM, 1, &_xa_plck);
	}

	if (w->state & DG_CORE_WINDOW_STATE_LOCKED_FOCUS) {
		_x_set_prop(true, w->x_win, _xa_stt, XCB_ATOM_ATOM, 1, &_xa_flck);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_window_toggle_state(dg_core_window_t *w, dg_core_window_state_t state)
{
	_window_set_state(w, state, (w->state & state ? DG_CORE_WINDOW_UNSET_STATE : DG_CORE_WINDOW_SET_STATE));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_window_update_current_grid(dg_core_window_t *w)
{
	if (w->state & DG_CORE_WINDOW_STATE_LOCKED_GRID) {
		return;
	}

	dg_core_grid_t *g_old = w->g_current;

	/* first find the smallest grid, then find the biggest grid that could fit in the current */
	/* window dimensions                                                                      */

	dg_core_grid_t *g_tmp;

	w->g_current = _window_find_smallest_grid(w);

	for (size_t i = 0; i < w->grids.n; i++) {
		g_tmp = (dg_core_grid_t*)w->grids.ptr[i];
		if (g_tmp->pw <= w->pw && g_tmp->ph <= w->ph &&
		    g_tmp->pw >= w->g_current->pw && g_tmp->ph >= w->g_current->ph) {
			w->g_current = g_tmp;
		}
	}

	/* stuff to do if there actually was a change in grids */

	if (g_old == w->g_current) {
		return;
	}

	if (g_old->g_ref) {
		dg_core_window_swap_grid(w, g_old, g_old->g_ref);
	}

	_RUN_FN(w->callback_grid, w, w->g_current);
	_window_set_render_level(w, _WINDOW_RENDER_FULL);
	_window_refocus(w);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_window_update_geometries(dg_core_window_t *w, bool is_popup)
{
	dg_core_grid_t *g;

	for (size_t i = 0; i < w->grids.n; i++) {
		g = (dg_core_grid_t*)w->grids.ptr[i];
		_grid_update_geometry(g, is_popup);
		for (size_t j = 0; j < g->areas.n; j++) {
			_area_update_geometry((_area_t*)g->areas.ptr[j], g, is_popup);
		}
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_window_update_wm_focus_hints(dg_core_window_t *w)
{
	/* if there is no focus, just delete the hint property */

	if (!w->a_focus) {
		_x_test_cookie(xcb_delete_property_checked(_x_con, w->x_win, _xa_dfoc), true);
		return;
	}

	/* otherwhise send a focus info event to get the geometry of the subfocused cell in case the focused  */
	/* area hosts a meta-cell. If the event is refused, then the focused area/cell is not a meta-cell and */
	/* its grid-level geometry can be used instead                                                        */

	_rect_t rect = {0};

	dg_core_cell_event_t cev = {
		.kind = DG_CORE_CELL_EVENT_INFO_FOCUSED_CELL,
		.info_focus_cell = NULL,
	};

	if (_window_process_cell_event(w, w->a_focus, &cev)) {
		rect.x = cev.info_focus_px;
		rect.y = cev.info_focus_py;
		rect.w = cev.info_focus_pw;
		rect.h = cev.info_focus_ph;
	} else {
		rect = _area_get_current_geometry(w->a_focus, w);
	}

	_x_set_prop(
		false, w->x_win, _xa_dfoc, XCB_ATOM_CARDINAL, 4, (uint32_t[4]){rect.x, rect.y, rect.w, rect.h});
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_window_update_wm_size_hints(dg_core_window_t *w)
{
	if (w->grids.n == 0) {
		return;
	}

	const dg_core_grid_t *g_min = w->state & DG_CORE_WINDOW_STATE_LOCKED_GRID ?
	                               w->g_current : _window_find_smallest_grid(w);

	const xcb_size_hints_t x_hints = {
		.flags =
			XCB_ICCCM_SIZE_HINT_P_MIN_SIZE |
			XCB_ICCCM_SIZE_HINT_P_MAX_SIZE,
		.min_width   = g_min->pw,
		.min_height  = g_min->ph,
		.max_width   = g_min->n_fwu > 0.0 ? INT16_MAX : g_min->pw,
		.max_height  = g_min->n_fhu > 0.0 ? INT16_MAX : g_min->ph};

	_x_set_prop(
		false, w->x_win, XCB_ATOM_WM_NORMAL_HINTS, XCB_ATOM_WM_SIZE_HINTS, sizeof(xcb_size_hints_t) ,&x_hints);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static xcb_atom_t
_x_get_atom(const char *name)
{
	xcb_atom_t xa; 

	xcb_intern_atom_cookie_t xc = xcb_intern_atom(_x_con, 0, strlen(name), name);
	xcb_intern_atom_reply_t *xr = xcb_intern_atom_reply(_x_con, xc, NULL);
	if (!xr) {
		dg_core_errno_set(DG_CORE_ERRNO_XCB);
		return 0;
	}
	
	xa = xr->atom;
	free(xr);
	return xa;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static xcb_atom_t
_x_get_atom_sel(int selection)
{
	switch (selection) {
		case 0:
			return _xa_clip;
		case 1 : 
			return XCB_ATOM_PRIMARY;
		case 2 :
			return XCB_ATOM_SECONDARY;
		default :
			return XCB_ATOM_NONE;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static xcb_atom_t
_x_get_atom_sel_target(int selection)
{
	switch (selection) {
		case 0:
			return _xa_tmp1;
		case 1 : 
			return _xa_tmp2;
		case 2 :
			return _xa_tmp3;
		default :
			return XCB_ATOM_NONE;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static uint8_t 
_x_get_extension_opcode(const char *name)
{
	uint8_t opcode;

	xcb_query_extension_cookie_t xc = xcb_query_extension(_x_con, strlen(name), name);
	xcb_query_extension_reply_t *xr = xcb_query_extension_reply(_x_con, xc, NULL);
	if (!xr) {
		dg_core_errno_set(DG_CORE_ERRNO_XCB);
		return 0;
	}

	opcode = xr->major_opcode;
	free(xr);
	return opcode;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static _rect_t
_x_get_monitor_geometry_at(int16_t px, int16_t py)
{
	_rect_t rect = {0};

	xcb_randr_get_monitors_cookie_t xc = xcb_randr_get_monitors(_x_con, _x_scr->root, 1);
	xcb_randr_get_monitors_reply_t *xr = xcb_randr_get_monitors_reply(_x_con, xc, NULL);
	if (!xr) {
		dg_core_errno_set(DG_CORE_ERRNO_XCB);
		return rect;
	}

	xcb_randr_monitor_info_iterator_t xi = xcb_randr_get_monitors_monitors_iterator(xr);
	for (; xi.rem; xcb_randr_monitor_info_next(&xi)) {
		if (dg_core_util_test_bounds(px, py, xi.data->x, xi.data->y, xi.data->width, xi.data->height)) {
			rect.x = xi.data->x;
			rect.y = xi.data->y;
			rect.w = xi.data->width;
			rect.h = xi.data->height;
			break;
		}
	}

	free(xr);
	return rect;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static int
_x_get_selection_id(xcb_atom_t xa)
{
	if (xa == _xa_clip) {
		return 0;
	} else if (xa == XCB_ATOM_PRIMARY) {
		return 1;
	} else if (xa == XCB_ATOM_SECONDARY) {
		return 2;
	} else {
		return -1;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static xcb_timestamp_t
_x_get_timestamp(void)
{
	xcb_timestamp_t xt = 0;

	/*****/

	_x_set_prop(false, _x_win_l, _xa_time, _xa_time, 0, NULL);
	xcb_flush(_x_con);

	/* wait for property notification event to get time, other events are pushed to the stack buffer to */
	/* be processed by the main event tree in dg_core_look_run()                                        */

	xcb_property_notify_event_t *x_ev;

	while (true) {

		x_ev = (xcb_property_notify_event_t*)xcb_wait_for_event(_x_con);
		if (!x_ev) {
			break;
		}

		if ((x_ev->response_type & ~0x80) == XCB_PROPERTY_NOTIFY &&
		    x_ev->window == _x_win_l &&
		    x_ev->atom == _xa_time) {
			xt = x_ev->time;
			free(x_ev);
			break;
		} else {
			dg_core_stack_push(&_events, x_ev, NULL);
		}
	}

	/*****/

	return xt;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
_x_send_sel_data(int selection, xcb_window_t requestor, xcb_atom_t prop, xcb_atom_t target)
{
	if (target == _xa_utf8) {
		_x_set_prop(false, requestor, prop, _xa_utf8, _sel[selection].data_n, _sel[selection].data);
		return true;
	} else if (target == _xa_trgt) {
		_x_set_prop(false, requestor, prop, XCB_ATOM_ATOM, sizeof(_sel_targets) / sizeof(xcb_atom_t), _sel_targets);
	} else if (target == _xa_time) {
		_x_set_prop(false, requestor, prop, XCB_ATOM_INTEGER, 1, &_sel[selection].time);
	}
	
	return false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_x_set_prop(bool append, xcb_window_t x_win, xcb_atom_t x_prop, xcb_atom_t x_type, uint32_t data_n,
            const void *data)
{
	xcb_void_cookie_t xc;

	xc = xcb_change_property_checked(
		_x_con,
		append ? XCB_PROP_MODE_APPEND : XCB_PROP_MODE_REPLACE,
		x_win,
		x_prop,
		x_type,
		x_type == _xa_utf8 || x_type == _xa_time || x_type == XCB_ATOM_STRING ? 8 : 32,
		data_n,
		data);

	_x_test_cookie(xc, true);

	xcb_flush(_x_con);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
_x_test_cookie(xcb_void_cookie_t xc, bool log)
{
	bool err = false;
	xcb_generic_error_t *x_err;

	if ((x_err = xcb_request_check(_x_con, xc))) {
		err = true;
		free(x_err);
	}

	if (err && log) {
		dg_core_errno_set(DG_CORE_ERRNO_XCB);
	}

	return err;
}

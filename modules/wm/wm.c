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
#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include <xcb/xcb.h>

#include <dg/core/atom.h>
#include <dg/core/config.h>
#include <dg/core/core.h>
#include <dg/core/errno.h>

#include "wm.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/* macros for common parameter checking */

#define _IS_INIT assert(_init);

/* recursion safeguard */

#define _TREE_MAX_DEPTH 32

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/* X helpers */

static xcb_atom_t _x_get_atom (const char *name);
static bool       _x_is_leader (xcb_window_t x_win);

/* procedures with side effects */

static void _reconfig_tree (xcb_window_t x_win, unsigned int depth);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/* xcb globals */

static xcb_connection_t *_x_con = NULL;
static xcb_screen_t     *_x_scr = NULL;

/* common X atoms */

static xcb_atom_t _xa_prot = 0; /* "WM_PROTOCOLS"     */
static xcb_atom_t _xa_lead = 0; /* "WM_CLIENT_LEADER" */

/* DG custom atoms */

static xcb_atom_t _xa_sig  = 0; /* DG_CORE_ATOM_SIGNALS           */
static xcb_atom_t _xa_vers = 0; /* DG_CORE_ATOM_VERSION           */
static xcb_atom_t _xa_stt  = 0; /* DG_CORE_ATOM_WINDOW_STATES     */
static xcb_atom_t _xa_dfoc = 0; /* DG_CORE_ATOM_WINDOW_FOCUS      */
static xcb_atom_t _xa_won  = 0; /* DG_CORE_ATOM_WINDOW_ACTIVE     */
static xcb_atom_t _xa_wena = 0; /* DG_CORE_ATOM_WINDOW_DISABLED   */
static xcb_atom_t _xa_plck = 0; /* DG_CORE_ATOM_WINDOW_GRID_LOCK  */
static xcb_atom_t _xa_flck = 0; /* DG_CORE_ATOM_WINDOW_FOCUS_LOCK */
static xcb_atom_t _xa_conf = 0; /* DG_CORE_ATOM_RECONFIG          */
static xcb_atom_t _xa_acl  = 0; /* DG_CORE_ATOM_ACCEL             */

static xcb_atom_t _xa_aclx[DG_CORE_CONFIG_MAX_ACCELS] = {0}; /* "_DG_WINDOW_FNx" x = 1..12 */

/* session states */

static bool _ext_x = false;
static bool _init  = false;

/************************************************************************************************************/
/* PUBLIC - MAIN ********************************************************************************************/
/************************************************************************************************************/

void
dg_wm_init(xcb_connection_t *connection)
{
	assert(!_init);

	/* setup x connection */

	if (connection) {
		_x_con = connection;
		_ext_x = true;
	} else if (dg_core_is_init()) {
		_x_con = dg_core_get_xcb_connection();
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

	/* get atoms */

	_xa_prot = _x_get_atom("WM_PROTOCOLS");
	_xa_lead = _x_get_atom("WM_CLIENT_LEADER");
	_xa_sig  = _x_get_atom(DG_CORE_ATOM_SIGNALS);
	_xa_vers = _x_get_atom(DG_CORE_ATOM_VERSION);
	_xa_stt  = _x_get_atom(DG_CORE_ATOM_WINDOW_STATES);
	_xa_dfoc = _x_get_atom(DG_CORE_ATOM_WINDOW_FOCUS);
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

	/* end of initialisation */

	_init = true;

	return;
	
	/* errors */

err_crit:
	dg_core_errno_set(DG_CORE_ERRNO_XCB_CRIT);
	dg_wm_reset();	
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
dg_wm_is_init(void)
{
	return _init;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void dg_wm_reset(void)
{
	/* disconnect from x server */

	if (_x_con && !_ext_x) {
		xcb_disconnect(_x_con);
	}

	/* reset all global variables */

	_ext_x = false;

	_xa_prot = 0;
	_xa_lead = 0;
	_xa_sig  = 0;
	_xa_vers = 0;
	_xa_stt  = 0;
	_xa_dfoc = 0;
	_xa_won  = 0;
	_xa_wena = 0;
	_xa_plck = 0;
	_xa_flck = 0;
	_xa_conf = 0;
	_xa_acl  = 0;

	for (int i = 0; i < sizeof(_xa_aclx) / sizeof(xcb_atom_t); i++) {
		_xa_aclx[i] = 0;
	}

	/* end */
	
	_init  = false;
}

/************************************************************************************************************/
/* PUBLIC - WM HELPERS **************************************************************************************/
/************************************************************************************************************/

bool
dg_wm_is_dg_client(xcb_window_t window, char **version)
{
	_IS_INIT;

	bool is_dg;

	xcb_get_property_cookie_t xc = xcb_get_property(_x_con, 0, window, _xa_vers, XCB_ATOM_STRING, 0, UINT32_MAX);
	xcb_get_property_reply_t *xr = xcb_get_property_reply(_x_con, xc, NULL);
	if (!xr) {
		dg_core_errno_set(DG_CORE_ERRNO_XCB);
		return false;
	}

	is_dg = xr->value_len > 0;
	if (is_dg && version) {
		*version = strdup(xcb_get_property_value(xr));
		if (!*version) {
			dg_core_errno_set(DG_CORE_ERRNO_MEMORY);
		}
	}

	free(xr);
	return is_dg;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_wm_reconfig_all(void)
{
	_IS_INIT;

	_reconfig_tree(_x_scr->root, 0);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_wm_reconfig_client(xcb_window_t window)
{
	_IS_INIT;

	xcb_client_message_data_t x_data = {.data32 = {_xa_sig, _xa_conf, 0, 0, 0}};
	xcb_client_message_event_t x_ev = {
		.response_type = XCB_CLIENT_MESSAGE,
		.format   = 32,
		.sequence = 0,
		.window   = window,
		.type     = _xa_prot,
		.data     = x_data};

	xcb_send_event(_x_con, 0, window, XCB_EVENT_MASK_NO_EVENT, (char*)&x_ev);
	xcb_flush(_x_con);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
dg_wm_test_accelerator(xcb_window_t window, int accel_id, char **name)
{
	_IS_INIT;

	assert(accel_id > 0);

	// TODO

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
dg_wm_test_signals(xcb_window_t window)
{
	_IS_INIT;

	bool can_dg = false;

	xcb_get_property_cookie_t xc = xcb_get_property(_x_con, 0, window, _xa_prot, XCB_ATOM_ATOM, 0, UINT32_MAX);
	xcb_get_property_reply_t *xr = xcb_get_property_reply(_x_con, xc, NULL);
	if (!xr) {
		dg_core_errno_set(DG_CORE_ERRNO_XCB);
		return false;
	}

	for (int i = 0; i < xcb_get_property_value_length(xr); i++) {
		if (_xa_sig == ((xcb_atom_t*)xcb_get_property_value(xr))[i]) {
			can_dg = true;
			break;
		}
	}
	
	free(xr);
	return can_dg;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_wm_trigger_accelerator(xcb_window_t window, int accel_id)
{
	_IS_INIT;

	assert(accel_id > 0);

	// TODO
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static void
_reconfig_tree(xcb_window_t x_win, unsigned int depth)
{
	if (dg_wm_test_signals(x_win) && _x_is_leader(x_win)) {
		dg_wm_reconfig_client(x_win);
	}

	if (depth > _TREE_MAX_DEPTH) {
		return;
	}

	xcb_query_tree_cookie_t xc = xcb_query_tree(_x_con, x_win);
	xcb_query_tree_reply_t *xr = xcb_query_tree_reply(_x_con, xc, NULL);
	if (!xr) {
		dg_core_errno_set(DG_CORE_ERRNO_XCB);
		return;
	}

	for (int i = 0; i < xcb_query_tree_children_length(xr); i++) {
		_reconfig_tree(xcb_query_tree_children(xr)[i], depth + 1);
	}

	free(xr);
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

static bool
_x_is_leader(xcb_window_t x_win)
{
	bool is_leader;

	xcb_get_property_cookie_t xc = xcb_get_property(_x_con, 0, x_win, _xa_lead, XCB_ATOM_WINDOW, 0, UINT32_MAX);
	xcb_get_property_reply_t *xr = xcb_get_property_reply(_x_con, xc, NULL);
	if (!xr) {
		dg_core_errno_set(DG_CORE_ERRNO_XCB);
		return false;
	}

	is_leader = xr->value_len > 0 && x_win == ((xcb_window_t*)xcb_get_property_value(xr))[0];
	free(xr);
	return is_leader;
}

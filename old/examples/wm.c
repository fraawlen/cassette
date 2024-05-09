/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
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

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <dg/core/core.h>
#include <dg/base/base.h>
#include <xcb/xcb.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define _ATOM "WM_NAME"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void _create_decorations (void);
static void _create_frame       (void);

static void _destroy_decorations (void);
static void _destroy_frame       (void);

static void _xcb_event (xcb_generic_event_t *x_ev);
static void _xcb_init  (void);
static void _xcb_reset (void);

static void _event_configure (xcb_configure_notify_event_t *x_ev);
static void _get_title (void);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static xcb_connection_t *_x_con = NULL;
static xcb_screen_t     *_x_scr = NULL;
static xcb_depth_t      *_x_dph = NULL;
static xcb_visualtype_t *_x_vis = NULL;
static xcb_colormap_t    _x_clm = 0;

static xcb_window_t _frame  = 0;
static xcb_window_t _client = 0;

static dg_core_window_t *_w        = NULL;
static dg_core_grid_t   *_g1       = NULL;
static dg_core_grid_t   *_g2       = NULL;
static dg_core_grid_t   *_g3       = NULL;
static dg_core_cell_t   *_c_close  = NULL;
static dg_core_cell_t   *_c_min    = NULL;
static dg_core_cell_t   *_c_max    = NULL;
static dg_core_cell_t   *_c_title  = NULL;

static int16_t  _client_px   = 0;
static char    *_title       = NULL;
static int16_t  _thick_frame = 3;
static uint32_t _cl_frame    = 0xff393533;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

int
main(int argc, char **argv)
{
	/* options */

	int opt;
	while ((opt = getopt(argc, argv, "c:t:w:")) != -1) {
		switch (opt) {

			case 'c':
				_cl_frame = strtol(optarg, NULL, 0);
				break;

			case 't':
				_thick_frame = strtol(optarg, NULL, 0);
				break;

			case 'w':
				_client = strtol(optarg, NULL, 0);
				break;

			default:
				break;
		}
	}

	/* initialisation & UI setup */

	_xcb_init();
	dg_core_init(argc, argv, NULL, NULL, _x_con);
	dg_base_init();

	if (_client != 0) {
		_get_title();
	}

	_create_decorations();
	_create_frame();

	/* event loop */

	_client_px = dg_core_grid_get_min_pixel_width(_g1) + 2 * _thick_frame;

	if (_client != 0) {
		xcb_reparent_window(_x_con, _client, _frame, _client_px, _thick_frame);
	}

	xcb_reparent_window(_x_con, dg_core_window_get_xcb_window(_w), _frame, _thick_frame, _thick_frame);
	xcb_map_window(_x_con, _frame);
	dg_core_window_activate(_w);

	dg_core_loop_set_event_postprocessor(_xcb_event);
	dg_core_loop_run();

	/* cleanup & end */

	_destroy_decorations();
	_destroy_frame();

	dg_base_reset();
	dg_core_reset();
	_xcb_reset();

	return 0;
}

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void
_create_decorations(void)
{
	/* object instantiation */

	_w  = dg_core_window_create(DG_CORE_WINDOW_DEFAULT);
	_g1 = dg_core_grid_create(1, 2);
	_g2 = dg_core_grid_create(1, 3);
	_g3 = dg_core_grid_create(1, 4);

	_c_close = dg_base_button_create();
	_c_min   = dg_base_button_create();
	_c_max   = dg_base_button_create();
	_c_title = dg_base_label_create();

	/* cell configuration */

	dg_base_button_set_icon(_c_close, DG_BASE_BUTTON_ICON_CROSS);
	dg_base_button_set_icon(_c_min,   DG_BASE_BUTTON_ICON_MINUS);
	dg_base_button_set_icon(_c_max,   DG_BASE_BUTTON_ICON_PLUS);

	dg_base_label_set_label(_c_title,    _title ? _title : "Window title");
	dg_base_label_set_origin(_c_title,   DG_BASE_ORIGIN_BOTTOM);
	dg_base_label_set_rotation(_c_title, DG_BASE_ROTATION_RIGHT);

	/* grid configuration */

	dg_core_grid_set_column_width(_g1, 0, -1);
	dg_core_grid_set_column_width(_g2, 0, -1);
	dg_core_grid_set_column_width(_g3, 0, -1);

	dg_core_grid_set_row_height(_g1, 1, 0);

	dg_core_grid_set_row_growth(_g1, 1, 1.0);
	dg_core_grid_set_row_growth(_g2, 2, 1.0);
	dg_core_grid_set_row_growth(_g3, 3, 1.0);

	dg_core_grid_assign_cell(_g1, _c_close, 0, 0, 1, 1);
	dg_core_grid_assign_cell(_g1, _c_title, 0, 1, 1, 1);

	dg_core_grid_assign_cell(_g2, _c_close, 0, 0, 1, 1);
	dg_core_grid_assign_cell(_g2, _c_min,   0, 1, 1, 1);
	dg_core_grid_assign_cell(_g2, _c_title, 0, 2, 1, 1);

	dg_core_grid_assign_cell(_g3, _c_close, 0, 0, 1, 1);
	dg_core_grid_assign_cell(_g3, _c_min,   0, 1, 1, 1);
	dg_core_grid_assign_cell(_g3, _c_max,   0, 2, 1, 1);
	dg_core_grid_assign_cell(_g3, _c_title, 0, 3, 1, 1);

	/* window configuration */

	dg_core_window_push_grid(_w, _g3);
	dg_core_window_push_grid(_w, _g2);
	dg_core_window_push_grid(_w, _g1);
	dg_core_window_set_extra_size(_w, 0, 20);
	dg_core_window_rename(_w, "WM decorations concept", NULL);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_create_frame(void)
{
	_frame = xcb_generate_id(_x_con);
	xcb_create_window(
		_x_con,
		_x_dph->depth,
		_frame,
		_x_scr->root,
		0, 0,
		500, 500,
		0,
		XCB_WINDOW_CLASS_INPUT_OUTPUT,
		_x_vis->visual_id,
		XCB_CW_BACK_PIXEL | XCB_CW_BORDER_PIXEL | XCB_CW_EVENT_MASK | XCB_CW_COLORMAP,
		(uint32_t[4]){_cl_frame, _cl_frame, XCB_EVENT_MASK_STRUCTURE_NOTIFY, _x_clm});
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_destroy_decorations(void)
{
	dg_core_window_destroy(_w);
	dg_core_grid_destroy(_g1);
	dg_core_grid_destroy(_g2);
	dg_core_grid_destroy(_g3);
	dg_core_cell_destroy(_c_close);
	dg_core_cell_destroy(_c_min);
	dg_core_cell_destroy(_c_max);
	dg_core_cell_destroy(_c_title);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_destroy_frame(void)
{

}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_event_configure(xcb_configure_notify_event_t *x_ev)
{
	if (x_ev->window != _frame) {
		return;
	}

	int16_t l = x_ev->height - 2 * _thick_frame;

	xcb_configure_window(
		_x_con,
		dg_core_window_get_xcb_window(_w),
		XCB_CONFIG_WINDOW_HEIGHT,
		(uint32_t[1]){l});

	if (_client == 0) {
		return;
	}

	xcb_configure_window(
		_x_con,
		_client,
		XCB_CONFIG_WINDOW_WIDTH | XCB_CONFIG_WINDOW_HEIGHT,
		(uint32_t[2]){x_ev->width - _client_px - _thick_frame, l});
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/


static void
_get_title(void)
{
	xcb_atom_t xa; 

	/* get "WM_NAME" atom */

	xcb_intern_atom_cookie_t xc = xcb_intern_atom(_x_con, 0, strlen(_ATOM), _ATOM);
	xcb_intern_atom_reply_t *xr = xcb_intern_atom_reply(_x_con, xc, NULL);
	
	xa = xr->atom;
	free(xr);

	/* get name property */

	xcb_get_property_cookie_t xc2 = xcb_get_property(_x_con, 0, _client, xa, XCB_ATOM_STRING, 0, UINT32_MAX);
	xcb_get_property_reply_t *xr2 = xcb_get_property_reply(_x_con, xc2, NULL);

	_title = strdup(xcb_get_property_value(xr2));
	free(xr2);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_xcb_event(xcb_generic_event_t *x_ev)
{
	switch (x_ev->response_type & ~0x80) {

		case XCB_CONFIGURE_NOTIFY:
			_event_configure((xcb_configure_notify_event_t*)x_ev);
			break;

		default:
			break;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_xcb_init(void)
{
	_x_con = xcb_connect(NULL, NULL);
	_x_scr = xcb_setup_roots_iterator(xcb_get_setup(_x_con)).data;

	xcb_depth_iterator_t x_dph_it = xcb_screen_allowed_depths_iterator(_x_scr);
	for (; x_dph_it.rem; xcb_depth_next(&x_dph_it)) {
		if (x_dph_it.data->depth == 32) {
			_x_dph = x_dph_it.data;
			break;
		}
	}

	xcb_visualtype_iterator_t x_vis_it = xcb_depth_visuals_iterator(_x_dph);
	for (; x_vis_it.rem; xcb_visualtype_next(&x_vis_it)) {
		if (x_vis_it.data->_class == XCB_VISUAL_CLASS_TRUE_COLOR) {
			_x_vis = x_vis_it.data;
			break;
		}
	}

	_x_clm = xcb_generate_id(_x_con);
	xcb_create_colormap(_x_con, XCB_COLORMAP_ALLOC_NONE, _x_clm, _x_scr->root, _x_vis->visual_id);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_xcb_reset(void)
{
	xcb_disconnect(_x_con);
}

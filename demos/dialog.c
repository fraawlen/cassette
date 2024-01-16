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

#include <stdio.h>
#include <stdlib.h>

#include <dg/core/core.h>
#include <dg/base/base.h>
#include <dg/base/config.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void _button_click (dg_core_cell_t *c);
static void _window_close (dg_core_window_t *w);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static dg_core_window_t *_w_main = NULL;
static dg_core_grid_t   *_g_main = NULL;

static dg_core_window_t *_w_dialog = NULL;
static dg_core_grid_t   *_g_dialog = NULL;

static dg_core_cell_t *_cl  = NULL;
static dg_core_cell_t *_cg  = NULL;
static dg_core_cell_t *_cbc = NULL;
static dg_core_cell_t *_cby = NULL;
static dg_core_cell_t *_cbn = NULL;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

int
main(int argc, char **argv)
{
	/* module initialisation */

	dg_core_init(argc, argv, NULL, NULL, NULL);
	dg_base_init();

	/* object instantiation */

	_w_main = dg_core_window_create(DG_CORE_WINDOW_DEFAULT);
	_g_main = dg_core_grid_create(1, 1);

	_w_dialog = dg_core_window_create(DG_CORE_WINDOW_DEFAULT);
	_g_dialog = dg_core_grid_create(5, 2);

	_cl  = dg_base_label_create();
	_cg  = dg_base_gap_create();
	_cbc = dg_base_button_create();
	_cby = dg_base_button_create();
	_cbn = dg_base_button_create();

	/* cell configuration */

	dg_base_label_set_label(_cl, "Are you sure you want to exit ?");
	dg_base_label_set_origin(_cl, DG_BASE_ORIGIN_CENTER);

	dg_base_button_set_label(_cbc, "Close window");
	dg_base_button_set_label_origin(_cbc, DG_BASE_ORIGIN_CENTER);
	dg_base_button_set_callback_pressed(_cbc, _button_click);

	dg_base_button_set_label(_cby, "Proceed");
	dg_base_button_set_icon(_cby, DG_BASE_BUTTON_ICON_YES);
	dg_base_button_set_callback_pressed(_cby, _button_click);
		
	dg_base_button_set_label(_cbn, "Cancel");
	dg_base_button_set_icon(_cbn, DG_BASE_BUTTON_ICON_NO);
	dg_base_button_set_callback_pressed(_cbn, _button_click);

	/* grid configuration - main */

	dg_core_grid_set_column_width(_g_main, 0, 12);
	dg_core_grid_set_column_growth(_g_main, 0, 1.0);
	dg_core_grid_set_row_growth(_g_main, 0, 1.0);

	dg_core_grid_assign_cell(_g_main, _cbc, 0, 0, 1, 1);

	/* grid configuration - dialog */

	dg_core_grid_set_column_width(_g_dialog, 0,  0);
	dg_core_grid_set_column_width(_g_dialog, 1,  6);
	dg_core_grid_set_column_width(_g_dialog, 2, -1);
	dg_core_grid_set_column_width(_g_dialog, 3,  6);
	dg_core_grid_set_column_width(_g_dialog, 4, -1);
	dg_core_grid_set_column_growth(_g_dialog, 0, 1.0);
	dg_core_grid_set_row_growth(_g_dialog, 0, 1.0);

	dg_core_grid_assign_cell(_g_dialog, _cl,  0, 0, 5, 1);
	dg_core_grid_assign_cell(_g_dialog, _cg,  0, 1, 1, 1);
	dg_core_grid_assign_cell(_g_dialog, _cby, 1, 1, 2, 1);
	dg_core_grid_assign_cell(_g_dialog, _cbn, 3, 1, 2, 1);

	/* window configuration */

	dg_core_window_push_grid(_w_main, _g_main);
	dg_core_window_set_extra_size(_w_main, 20, -20);
	dg_core_window_set_callback_close(_w_main, _window_close);
	dg_core_window_rename(_w_main, "Dialog opener", NULL);
	dg_core_window_activate(_w_main);

	dg_core_window_push_grid(_w_dialog, _g_dialog);
	dg_core_window_set_extra_size(_w_dialog, 10, 0);
	dg_core_window_set_callback_close(_w_dialog, _window_close);
	dg_core_window_rename(_w_dialog, "Dialog", NULL);
	dg_core_window_transient_for(_w_dialog, _w_main);

	/* event loop */

	dg_core_loop_run();

	/* end */

	dg_core_window_destroy(_w_main);
	dg_core_window_destroy(_w_dialog);
	dg_core_grid_destroy(_g_main);
	dg_core_grid_destroy(_g_dialog);
	dg_core_cell_destroy(_cl);
	dg_core_cell_destroy(_cg);
	dg_core_cell_destroy(_cbc);
	dg_core_cell_destroy(_cby);
	dg_core_cell_destroy(_cbn);

	dg_base_reset();
	dg_core_reset();

	return 0;
}

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void
_button_click(dg_core_cell_t *c)
{
	if (c == _cbc) {
		dg_core_window_disable(_w_main);
		dg_core_window_activate(_w_dialog);
	} else if (c == _cby) {
		dg_core_loop_abort();
	} else {
		dg_core_window_enable(_w_main);
		dg_core_window_deactivate(_w_dialog);
		dg_core_window_reset_current_grid(_w_dialog);
		dg_core_window_set_extra_size(_w_dialog, 10, 0);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_window_close(dg_core_window_t *w)
{
	_button_click(w == _w_main ? _cbc : _cbn);
}

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

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void _callback_grid (dg_core_window_t *w, dg_core_grid_t *g);
static void _callback_menu (dg_core_cell_t *c);
static void _callback_misc (dg_core_cell_t *c);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static dg_core_window_t *_w = NULL;

static dg_core_grid_t *_g_big     = NULL;
static dg_core_grid_t *_g_small_1 = NULL;
static dg_core_grid_t *_g_small_2 = NULL;

static dg_core_cell_t *_c_label       = NULL;
static dg_core_cell_t *_c_gap         = NULL;
static dg_core_cell_t *_c_placeholder = NULL;
static dg_core_cell_t *_c_but_menu    = NULL;
static dg_core_cell_t *_c_but_misc_1  = NULL;
static dg_core_cell_t *_c_but_misc_2  = NULL;
static dg_core_cell_t *_c_but_misc_3  = NULL;
static dg_core_cell_t *_c_but_misc_4  = NULL;
static dg_core_cell_t *_c_switch_misc = NULL;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const char *str_menu_show = "Show menu";
static const char *str_menu_hide = "Hide menu";

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

	_w         = dg_core_window_create(DG_CORE_WINDOW_DEFAULT);
	_g_big     = dg_core_grid_create(3, 7);
	_g_small_1 = dg_core_grid_create(2, 7);
	_g_small_2 = dg_core_grid_create(2, 7);

	_c_label       = dg_base_label_create();
	_c_gap         = dg_base_gap_create();
	_c_placeholder = dg_base_placeholder_create();
	_c_but_menu    = dg_base_button_create();
	_c_but_misc_1  = dg_base_button_create();
	_c_but_misc_2  = dg_base_button_create();
	_c_but_misc_3  = dg_base_button_create();
	_c_but_misc_4  = dg_base_button_create();
	_c_switch_misc = dg_base_switch_create();

	/* cell configuration */

	dg_base_label_set_label(_c_label, "Try to resize the window");
	dg_base_label_set_origin(_c_label, DG_BASE_ORIGIN_RIGHT);

	dg_base_button_set_label(_c_but_menu, str_menu_show);
	dg_base_button_set_icon(_c_but_menu, DG_BASE_BUTTON_ICON_UP);
	dg_base_button_set_callback_pressed(_c_but_menu, _callback_menu);

	dg_base_button_set_label(_c_but_misc_1,  "Button 1");
	dg_base_button_set_label(_c_but_misc_2,  "Button 2");
	dg_base_button_set_label(_c_but_misc_3,  "Button 3");
	dg_base_button_set_label(_c_but_misc_4,  "Button 4");
	
	dg_base_button_set_callback_pressed(_c_but_misc_1, _callback_misc);
	dg_base_button_set_callback_pressed(_c_but_misc_2, _callback_misc);
	dg_base_button_set_callback_pressed(_c_but_misc_3, _callback_misc);
	dg_base_button_set_callback_pressed(_c_but_misc_4, _callback_misc);

	dg_base_switch_set_label(_c_switch_misc, "Switch");
	
	/* grid configuration - desktop layout */
	
	dg_core_grid_set_column_width(_g_big, 0, 12);
	dg_core_grid_set_column_width(_g_big, 1, -1);
	dg_core_grid_set_column_width(_g_big, 2, 24);
	dg_core_grid_set_column_growth(_g_big, 2, 1.0);
	dg_core_grid_set_row_height(_g_big, 5, 0);
	dg_core_grid_set_row_growth(_g_big, 5, 1.0);

	dg_core_grid_assign_cell(_g_big, _c_switch_misc, 0, 0, 2, 1);
	dg_core_grid_assign_cell(_g_big, _c_but_misc_1,  0, 1, 2, 1);
	dg_core_grid_assign_cell(_g_big, _c_but_misc_2,  0, 2, 2, 1);
	dg_core_grid_assign_cell(_g_big, _c_but_misc_3,  0, 3, 2, 1);
	dg_core_grid_assign_cell(_g_big, _c_but_misc_4,  0, 4, 2, 1);
	dg_core_grid_assign_cell(_g_big, _c_gap,         0, 5, 2, 2);
	dg_core_grid_assign_cell(_g_big, _c_placeholder, 2, 0, 1, 6);
	dg_core_grid_assign_cell(_g_big, _c_label,       2, 6, 1, 1);

	/* grid configuration - mobile layout */

	dg_core_grid_set_column_width(_g_small_1, 0, 23);
	dg_core_grid_set_column_width(_g_small_1, 1, -1);
	dg_core_grid_set_column_growth(_g_small_1, 0, 1.0);
	dg_core_grid_set_row_height(_g_small_1, 1, 0);
	dg_core_grid_set_row_growth(_g_small_1, 1, 1.0);

	dg_core_grid_set_column_width(_g_small_2, 0, 23);
	dg_core_grid_set_column_width(_g_small_2, 1, -1);
	dg_core_grid_set_column_growth(_g_small_2, 0, 1.0);
	dg_core_grid_set_row_height(_g_small_2, 0, 0);
	dg_core_grid_set_row_growth(_g_small_2, 0, 1.0);

	dg_core_grid_assign_cell(_g_small_1, _c_label,       0, 0, 2, 1);
	dg_core_grid_assign_cell(_g_small_1, _c_placeholder, 0, 1, 2, 5);
	dg_core_grid_assign_cell(_g_small_1, _c_but_menu,    0, 6, 2, 1);

	dg_core_grid_assign_cell(_g_small_2, _c_gap,         0, 0, 2, 1);
	dg_core_grid_assign_cell(_g_small_2, _c_but_misc_1,  0, 1, 2, 1);
	dg_core_grid_assign_cell(_g_small_2, _c_but_misc_2,  0, 2, 2, 1);
	dg_core_grid_assign_cell(_g_small_2, _c_but_misc_3,  0, 3, 2, 1);
	dg_core_grid_assign_cell(_g_small_2, _c_but_misc_4,  0, 4, 2, 1);
	dg_core_grid_assign_cell(_g_small_2, _c_switch_misc, 0, 5, 2, 1);
	dg_core_grid_assign_cell(_g_small_2, _c_but_menu,    0, 6, 2, 1);

	dg_core_grid_set_reference(_g_small_2, _g_small_1);

	/* window configuration */

	dg_core_window_push_grid(_w, _g_small_1);
	dg_core_window_push_grid(_w, _g_big);
	dg_core_window_set_extra_size(_w, 5, 15);
	dg_core_window_set_callback_grid(_w, _callback_grid);
	dg_core_window_rename(_w, "Responsive layout", NULL);
	dg_core_window_activate(_w);

	/* event loop */

	dg_core_loop_run();

	/* cleanup & end */

	dg_core_window_destroy(_w);
	dg_core_grid_destroy(_g_big);
	dg_core_grid_destroy(_g_small_1);
	dg_core_grid_destroy(_g_small_2);
	dg_core_cell_destroy(_c_label);
	dg_core_cell_destroy(_c_gap);
	dg_core_cell_destroy(_c_placeholder);
	dg_core_cell_destroy(_c_but_menu);
	dg_core_cell_destroy(_c_but_misc_1);
	dg_core_cell_destroy(_c_but_misc_2);
	dg_core_cell_destroy(_c_but_misc_3);
	dg_core_cell_destroy(_c_but_misc_4);
	dg_core_cell_destroy(_c_switch_misc);

	dg_base_reset();
	dg_core_reset();

	return 0;
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static void
_callback_grid(dg_core_window_t *w, dg_core_grid_t *g)
{
	if (g == _g_small_2) {
		dg_base_button_set_label(_c_but_menu, str_menu_hide);
		dg_base_button_set_icon(_c_but_menu, DG_BASE_BUTTON_ICON_DOWN);
	} else {
		dg_base_button_set_label(_c_but_menu, str_menu_show);
		dg_base_button_set_icon(_c_but_menu, DG_BASE_BUTTON_ICON_UP);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_callback_menu(dg_core_cell_t *c)
{
	dg_core_grid_t *g = dg_core_window_get_current_grid(_w);

	if (g == _g_big) {
		return;
	} else if (g == _g_small_1) {
		dg_core_window_swap_grid(_w, _g_small_1, _g_small_2);
	} else {
		dg_core_window_swap_grid(_w, _g_small_2, _g_small_1);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_callback_misc(dg_core_cell_t *c)
{
	int i = 0;

	if (c == _c_but_misc_1) {
		i = 1;
	} else if (c == _c_but_misc_2) {
		i = 2;
	} else if (c == _c_but_misc_3) {
		i = 3;
	} else if (c == _c_but_misc_4) {
		i = 4;
	}

	printf("triggered button %i\n", i);
}

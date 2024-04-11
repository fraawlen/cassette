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

#include <stdlib.h>

#include <dg/core/core.h>
#include <dg/base/base.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define _N_BUT 33

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void _callback_button (dg_core_cell_t *c);
static void _randomize_icons (void);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static dg_core_window_t *_w = NULL;
static dg_core_grid_t   *_g = NULL;

static dg_core_cell_t *_c_but[_N_BUT] = {NULL};
static dg_core_cell_t *_c_info        =  NULL;
static dg_core_cell_t *_c_fill        =  NULL;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const char *str_info =
	"Try to navigate with your keyboard.\n"
	"All buttons are reachable.\n"
	"The reference navigation point of each\n"
	"cell is their top left corner and the\n"
	"scan to find the next cell to focus is\n"
	"done on the cell's width or height.\n"
	"\n"
	"Default keybinds (qwerty keymap):\n"
	"\n"
	"Ctrl Esc         : Unfocus\n"
	"Ctrl Return      : Toggle focus lock\n"
	"Ctrl Left        : Move left\n"
	"Ctrl Right       : Move right\n"
	"Ctrl Up          : Move up\n"
	"Ctrl Down        : Move down\n"
	"Ctrl Shift Left  : Move all the way left\n"
	"Ctrl Shift Right : Move all the way right\n"
	"Ctrl Shift Up    : Move all the way up\n"
	"Ctrl Shift Down  : Move all the way down\n"
	"Ctrl Tab         : Move to next cell\n"
	"Ctrl Shift Tab   : Move to previous cell\n"
	"Ctrl [           : Move to first cell\n"
	"Ctrl ]           : Move to last cell\n";

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

int
main(int argc, char **argv)
{
	srand(time(NULL));

	/* module initialisation */

	dg_core_init(argc, argv, NULL, NULL, NULL);
	dg_base_init();

	/* object instantiation */

	_w = dg_core_window_create(DG_CORE_WINDOW_DEFAULT);
	_g = dg_core_grid_create(10, 9);

	_c_info = dg_base_label_create();
	_c_fill = dg_base_placeholder_create();
	for (int i = 0; i < _N_BUT; i++) {
		_c_but[i] = dg_base_button_create();
	}

	/* cell configuration */

	dg_base_label_set_label(_c_info, str_info);
	dg_base_label_set_origin(_c_info, DG_BASE_ORIGIN_TOP_LEFT);

	for (int i = 0; i < _N_BUT; i++) {
		dg_base_button_set_callback_pressed(_c_but[i], _callback_button);
	}

	_randomize_icons();

	/* grid configuration */
	
	dg_core_grid_set_column_width(_g, 0, 41);

	for (int i = 0; i < 9; i++) {
		dg_core_grid_set_column_width(_g, i + 1, -2);
		dg_core_grid_set_row_height(_g, i, 2);
	}

	dg_core_grid_assign_cell(_g, _c_info, 0, 0, 1, 9);

	dg_core_grid_assign_cell(_g, _c_fill, 5, 0, 1, 1);
	dg_core_grid_assign_cell(_g, _c_fill, 6, 0, 1, 1);
	dg_core_grid_assign_cell(_g, _c_fill, 8, 0, 1, 1);
	dg_core_grid_assign_cell(_g, _c_fill, 9, 0, 1, 1);
	dg_core_grid_assign_cell(_g, _c_fill, 5, 1, 1, 1);
	dg_core_grid_assign_cell(_g, _c_fill, 7, 1, 1, 1);
	dg_core_grid_assign_cell(_g, _c_fill, 8, 1, 1, 1);
	dg_core_grid_assign_cell(_g, _c_fill, 6, 2, 3, 3);
	dg_core_grid_assign_cell(_g, _c_fill, 1, 5, 1, 1);
	dg_core_grid_assign_cell(_g, _c_fill, 2, 5, 1, 1);
	dg_core_grid_assign_cell(_g, _c_fill, 7, 5, 2, 2);
	dg_core_grid_assign_cell(_g, _c_fill, 2, 6, 1, 1);
	dg_core_grid_assign_cell(_g, _c_fill, 9, 6, 1, 1);
	dg_core_grid_assign_cell(_g, _c_fill, 1, 7, 1, 1);
	dg_core_grid_assign_cell(_g, _c_fill, 2, 7, 1, 1);
	dg_core_grid_assign_cell(_g, _c_fill, 3, 7, 1, 1);
	dg_core_grid_assign_cell(_g, _c_fill, 4, 7, 1, 1);
	dg_core_grid_assign_cell(_g, _c_fill, 5, 7, 1, 1);
	dg_core_grid_assign_cell(_g, _c_fill, 6, 7, 2, 2);
	dg_core_grid_assign_cell(_g, _c_fill, 9, 7, 1, 1);
	dg_core_grid_assign_cell(_g, _c_fill, 1, 8, 1, 1);
	dg_core_grid_assign_cell(_g, _c_fill, 8, 8, 1, 1);
	dg_core_grid_assign_cell(_g, _c_fill, 9, 8, 1, 1);
	
	dg_core_grid_assign_cell(_g, _c_but[ 0], 1, 0, 1, 1);
	dg_core_grid_assign_cell(_g, _c_but[ 1], 2, 0, 1, 1);
	dg_core_grid_assign_cell(_g, _c_but[ 2], 3, 0, 1, 1);
	dg_core_grid_assign_cell(_g, _c_but[ 3], 4, 0, 1, 1);
	dg_core_grid_assign_cell(_g, _c_but[ 4], 7, 0, 1, 1);
	dg_core_grid_assign_cell(_g, _c_but[ 5], 1, 1, 1, 1);
	dg_core_grid_assign_cell(_g, _c_but[ 6], 2, 1, 1, 1);
	dg_core_grid_assign_cell(_g, _c_but[ 7], 3, 1, 1, 1);
	dg_core_grid_assign_cell(_g, _c_but[ 8], 4, 1, 1, 1);
	dg_core_grid_assign_cell(_g, _c_but[ 9], 6, 1, 1, 1);
	dg_core_grid_assign_cell(_g, _c_but[10], 9, 1, 1, 1);
	dg_core_grid_assign_cell(_g, _c_but[11], 1, 2, 1, 1);
	dg_core_grid_assign_cell(_g, _c_but[12], 2, 2, 1, 1);
	dg_core_grid_assign_cell(_g, _c_but[13], 3, 2, 1, 1);
	dg_core_grid_assign_cell(_g, _c_but[14], 4, 2, 1, 1);
	dg_core_grid_assign_cell(_g, _c_but[15], 5, 2, 1, 2);
	dg_core_grid_assign_cell(_g, _c_but[26], 9, 2, 1, 1);
	dg_core_grid_assign_cell(_g, _c_but[17], 1, 3, 1, 2);
	dg_core_grid_assign_cell(_g, _c_but[18], 2, 3, 3, 2);
	dg_core_grid_assign_cell(_g, _c_but[19], 9, 3, 1, 1);
	dg_core_grid_assign_cell(_g, _c_but[20], 5, 4, 1, 1);
	dg_core_grid_assign_cell(_g, _c_but[21], 9, 4, 1, 1);
	dg_core_grid_assign_cell(_g, _c_but[22], 3, 5, 1, 1);
	dg_core_grid_assign_cell(_g, _c_but[23], 4, 5, 2, 1);
	dg_core_grid_assign_cell(_g, _c_but[24], 6, 5, 1, 1);
	dg_core_grid_assign_cell(_g, _c_but[25], 9, 5, 1, 1);
	dg_core_grid_assign_cell(_g, _c_but[26], 1, 6, 1, 1);
	dg_core_grid_assign_cell(_g, _c_but[27], 3, 6, 1, 1);
	dg_core_grid_assign_cell(_g, _c_but[28], 4, 6, 3, 1);
	dg_core_grid_assign_cell(_g, _c_but[29], 8, 7, 1, 1);
	dg_core_grid_assign_cell(_g, _c_but[30], 2, 8, 2, 1);
	dg_core_grid_assign_cell(_g, _c_but[31], 4, 8, 1, 1);
	dg_core_grid_assign_cell(_g, _c_but[32], 5, 8, 1, 1);

	/* window configuration */

	dg_core_window_push_grid(_w, _g);
	dg_core_window_rename(_w, "Navigation tester", NULL);
	dg_core_window_activate(_w);

	/* event loop */

	dg_core_loop_run();

	/* cleanup & end */

	dg_core_window_destroy(_w);
	dg_core_grid_destroy(_g);
	dg_core_cell_destroy(_c_info);
	dg_core_cell_destroy(_c_fill);
	for (int i = 0; i < _N_BUT; i++) {
		dg_core_cell_destroy(_c_but[i]);
	}

	dg_base_reset();
	dg_core_reset();

	return 0;
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static void
_callback_button(dg_core_cell_t *c)
{
	_randomize_icons();
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_randomize_icons(void)
{
	for (int i = 0; i < _N_BUT; i++) {
		dg_base_button_set_icon(_c_but[i], rand() % 14);
	}
}

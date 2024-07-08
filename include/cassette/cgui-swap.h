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

#pragma once

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
enum cgui_swap_type
{
	CGUI_INPUT_SWAP_TO_DEFAULT = 0,
	CGUI_INPUT_SWAP_TO_NONE,
	CGUI_INPUT_SWAP_TO_VALUE,
	CGUI_INPUT_SWAP_TO_ACCELERATOR,
	CGUI_INPUT_SWAP_TO_CLIPBOARD_CUT,
	CGUI_INPUT_SWAP_TO_CLIPBOARD_COPY,
	CGUI_INPUT_SWAP_TO_CLIPBOARD_PASTE,
	CGUI_INPUT_SWAP_TO_ACTION_CELL,
	CGUI_INPUT_SWAP_TO_ACTION_FOCUS,
	CGUI_INPUT_SWAP_TO_ACTION_WINDOW,
	CGUI_INPUT_SWAP_TO_ACTION_MISC,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
enum cgui_swap_action
{
	CGUI_INPUT_SWAP_NONE = 0,
	
	/* cell */

	CGUI_INPUT_SWAP_CELL_REDRAW,
	CGUI_INPUT_SWAP_CELL_SELECT_LESS,
	CGUI_INPUT_SWAP_CELL_SELECT_MORE,
	CGUI_INPUT_SWAP_CELL_SELECT_NONE,
	CGUI_INPUT_SWAP_CELL_SELECT_ALL,
	CGUI_INPUT_SWAP_CELL_TRIGGER_1,
	CGUI_INPUT_SWAP_CELL_TRIGGER_2,
	CGUI_INPUT_SWAP_CELL_TRIGGER_3,
	CGUI_INPUT_SWAP_CELL_TRIGGER_4,
	CGUI_INPUT_SWAP_CELL_TRIGGER_5,

	/* focus */

	CGUI_INPUT_SWAP_FOCUS_LEFT,
	CGUI_INPUT_SWAP_FOCUS_RIGHT,
	CGUI_INPUT_SWAP_FOCUS_UP,
	CGUI_INPUT_SWAP_FOCUS_DOWN,
	CGUI_INPUT_SWAP_FOCUS_LEFTMOST,
	CGUI_INPUT_SWAP_FOCUS_RIGHTMOST,
	CGUI_INPUT_SWAP_FOCUS_TOP,
	CGUI_INPUT_SWAP_FOCUS_BOTTOM,
	CGUI_INPUT_SWAP_FOCUS_NEXT,
	CGUI_INPUT_SWAP_FOCUS_PREV,
	CGUI_INPUT_SWAP_FOCUS_FIRST,
	CGUI_INPUT_SWAP_FOCUS_LAST,
	CGUI_INPUT_SWAP_FOCUS_NONE,

	/* window */

	CGUI_INPUT_SWAP_WINDOW_LOCK_GRID,
	CGUI_INPUT_SWAP_WINDOW_LOCK_FOCUS,
	CGUI_INPUT_SWAP_WINDOW_REDRAW,

	/* misc */

	CGUI_INPUT_SWAP_RECONFIG,
	CGUI_INPUT_SWAP_EXIT,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
struct cgui_swap
{
	uint8_t type;
	uint8_t value;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

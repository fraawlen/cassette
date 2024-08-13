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

#include <cassette/cgui.h>

/************************************************************************************************************/
/* GLOBALS **************************************************************************************************/
/************************************************************************************************************/

static const struct cgui_config config_default =
{
	.init   = false,
	.scale  = 1.0,
	.modkey = CGUI_CONFIG_MOD_CTRL,

	/* font */

	.font_face                = "Monospace",
	.font_size                = 14,
	.font_spacing_horizontal  = 0,
	.font_spacing_vertical    = 2,
	.font_offset_x            = 0,
	.font_offset_y            = 0,
	.font_override_width      = 7,
	.font_override_ascent     = 14,
	.font_override_descent    = 0,
	.font_enable_overrides    = false,
	.font_enable_hint_metrics = true,
	.font_antialias           = CGUI_CONFIG_ANTIALIAS_SUBPIXEL,
	.font_subpixel            = CGUI_CONFIG_SUBPIXEL_RGB,

	/* grid */

	.grid_padding = 10,
	.grid_spacing = 10,

	/* window */

	.window_color_background      = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
	.window_color_border          = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
	.window_color_border_disabled = { .r = 0.400, .g = 0.400, .b = 0.400, .a = 1.000 },
	.window_color_border_focused  = { .r = 0.671, .g = 0.671, .b = 0.671, .a = 1.000 },
	.window_color_border_locked   = { .r = 0.500, .g = 0.100, .b = 0.100, .a = 1.000 },

	.window_border   = 2,
	.window_padding  = 10,

	.window_enable_disabled = true,
	.window_enable_focused  = true,
	.window_enable_locked   = true,

	/* popup */

	.popup_color_background = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
	.popup_color_border     = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },

	.popup_border          = 2,
	.popup_padding         = 10,
	.popup_max_width       = 0,
	.popup_max_height      = 0,
	.popup_override_width  = 0,
	.popup_override_height = 0,
	.popup_override_x      = 0,
	.popup_override_y      = 0,

	.popup_enable_override_position = false,
	.popup_enable_override_width    = false,
	.popup_enable_override_height   = false,

	/* behavior */

	.cell_auto_lock           = true,
	.input_persistent_pointer = false,
	.input_persistent_touch   = false,
	.anim_divider             = 1,

	/* keys */

	.keys = {{{0}}},

	.keys[ 67][CGUI_CONFIG_SWAP_DIRECT] = { CGUI_INPUT_SWAP_TO_ACCELERATOR,   1                                 },
	.keys[ 68][CGUI_CONFIG_SWAP_DIRECT] = { CGUI_INPUT_SWAP_TO_ACCELERATOR,   2                                 },
	.keys[ 69][CGUI_CONFIG_SWAP_DIRECT] = { CGUI_INPUT_SWAP_TO_ACCELERATOR,   3                                 },
	.keys[ 70][CGUI_CONFIG_SWAP_DIRECT] = { CGUI_INPUT_SWAP_TO_ACCELERATOR,   4                                 },
	.keys[ 71][CGUI_CONFIG_SWAP_DIRECT] = { CGUI_INPUT_SWAP_TO_ACCELERATOR,   5                                 },
	.keys[ 72][CGUI_CONFIG_SWAP_DIRECT] = { CGUI_INPUT_SWAP_TO_ACCELERATOR,   6                                 },
	.keys[ 73][CGUI_CONFIG_SWAP_DIRECT] = { CGUI_INPUT_SWAP_TO_ACCELERATOR,   7                                 },
	.keys[ 74][CGUI_CONFIG_SWAP_DIRECT] = { CGUI_INPUT_SWAP_TO_ACCELERATOR,   8                                 },
	.keys[ 75][CGUI_CONFIG_SWAP_DIRECT] = { CGUI_INPUT_SWAP_TO_ACCELERATOR,   9                                 },
	.keys[ 76][CGUI_CONFIG_SWAP_DIRECT] = { CGUI_INPUT_SWAP_TO_ACCELERATOR,   10                                },
	.keys[ 95][CGUI_CONFIG_SWAP_DIRECT] = { CGUI_INPUT_SWAP_TO_ACCELERATOR,   11                                },
	.keys[ 96][CGUI_CONFIG_SWAP_DIRECT] = { CGUI_INPUT_SWAP_TO_ACCELERATOR,   12                                },

	.keys[  9][CGUI_CONFIG_SWAP_MOD   ] = { CGUI_INPUT_SWAP_TO_ACTION_FOCUS,  CGUI_INPUT_SWAP_FOCUS_NONE        },
	.keys[ 23][CGUI_CONFIG_SWAP_MOD   ] = { CGUI_INPUT_SWAP_TO_ACTION_FOCUS,  CGUI_INPUT_SWAP_FOCUS_NEXT        },
	.keys[ 34][CGUI_CONFIG_SWAP_MOD   ] = { CGUI_INPUT_SWAP_TO_ACTION_FOCUS,  CGUI_INPUT_SWAP_FOCUS_FIRST       },
	.keys[ 35][CGUI_CONFIG_SWAP_MOD   ] = { CGUI_INPUT_SWAP_TO_ACTION_FOCUS,  CGUI_INPUT_SWAP_FOCUS_LAST        },
	.keys[113][CGUI_CONFIG_SWAP_MOD   ] = { CGUI_INPUT_SWAP_TO_ACTION_FOCUS,  CGUI_INPUT_SWAP_FOCUS_LEFT        },
	.keys[114][CGUI_CONFIG_SWAP_MOD   ] = { CGUI_INPUT_SWAP_TO_ACTION_FOCUS,  CGUI_INPUT_SWAP_FOCUS_RIGHT       },
	.keys[111][CGUI_CONFIG_SWAP_MOD   ] = { CGUI_INPUT_SWAP_TO_ACTION_FOCUS,  CGUI_INPUT_SWAP_FOCUS_UP          },
	.keys[116][CGUI_CONFIG_SWAP_MOD   ] = { CGUI_INPUT_SWAP_TO_ACTION_FOCUS,  CGUI_INPUT_SWAP_FOCUS_DOWN        },
	.keys[ 23][CGUI_CONFIG_SWAP_SHIFT ] = { CGUI_INPUT_SWAP_TO_ACTION_FOCUS,  CGUI_INPUT_SWAP_FOCUS_PREV        },
	.keys[113][CGUI_CONFIG_SWAP_SHIFT ] = { CGUI_INPUT_SWAP_TO_ACTION_FOCUS,  CGUI_INPUT_SWAP_FOCUS_LEFTMOST    },
	.keys[114][CGUI_CONFIG_SWAP_SHIFT ] = { CGUI_INPUT_SWAP_TO_ACTION_FOCUS,  CGUI_INPUT_SWAP_FOCUS_RIGHTMOST   },
	.keys[111][CGUI_CONFIG_SWAP_SHIFT ] = { CGUI_INPUT_SWAP_TO_ACTION_FOCUS,  CGUI_INPUT_SWAP_FOCUS_TOP         },
	.keys[116][CGUI_CONFIG_SWAP_SHIFT ] = { CGUI_INPUT_SWAP_TO_ACTION_FOCUS,  CGUI_INPUT_SWAP_FOCUS_BOTTOM      },

	.keys[ 22][CGUI_CONFIG_SWAP_MOD   ] = { CGUI_INPUT_SWAP_TO_ACTION_CELL,   CGUI_INPUT_SWAP_CELL_REDRAW       },
	.keys[ 22][CGUI_CONFIG_SWAP_SHIFT ] = { CGUI_INPUT_SWAP_TO_ACTION_WINDOW, CGUI_INPUT_SWAP_WINDOW_REDRAW     },
	.keys[ 36][CGUI_CONFIG_SWAP_MOD   ] = { CGUI_INPUT_SWAP_TO_ACTION_WINDOW, CGUI_INPUT_SWAP_WINDOW_LOCK_FOCUS },
	.keys[ 36][CGUI_CONFIG_SWAP_SHIFT ] = { CGUI_INPUT_SWAP_TO_ACTION_WINDOW, CGUI_INPUT_SWAP_WINDOW_LOCK_GRID  },

	.keys[ 27][CGUI_CONFIG_SWAP_MOD   ] = { CGUI_INPUT_SWAP_TO_ACTION_MISC,   CGUI_INPUT_SWAP_RECONFIG          },
	.keys[ 54][CGUI_CONFIG_SWAP_MOD   ] = { CGUI_INPUT_SWAP_TO_ACTION_MISC,   CGUI_INPUT_SWAP_EXIT              },

	/* buttons */
	
	.buttons = {{{0}}},

	.buttons[4][CGUI_CONFIG_SWAP_MOD] = { CGUI_INPUT_SWAP_TO_VALUE, 6 },
	.buttons[5][CGUI_CONFIG_SWAP_MOD] = { CGUI_INPUT_SWAP_TO_VALUE, 7 },
};

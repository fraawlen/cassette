/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Graphics (CGUI) library.
 *
 * This library is free software; you can redistribute it and/or modify it either under the terms of the GNU
 * Lesser General Public License as published by the Free Software Foundation; either version 3.0 of the
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

static const struct cgui_window_style blank_window =
{
	.cn_type = {CGUI_CORNER_SQUARE, CGUI_CORNER_SQUARE, CGUI_CORNER_SQUARE, CGUI_CORNER_SQUARE},
	.bd_cl   = { .r = 0.671, .g = 0.671, .b = 0.671, .a = 1.000 },
	.bg_cl   = { .r = 0.200, .g = 0.200, .b = 0.200, .a = 0.800 },
	.cn_size = { 0.0, 0.0, 0.0, 0.0 },
	.bd_size =  10.0,
	.ena     = false,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const struct cgui_box blank_box =
{
	.cn_type     = {CGUI_CORNER_SQUARE, CGUI_CORNER_SQUARE, CGUI_CORNER_SQUARE, CGUI_CORNER_SQUARE},
	.cn_size     = {0.0, 0.0, 0.0, 0.0},
	.ol_size     =  0.0,
	.bd_size     = 10.0,
	.pad         =  0.0,
	.margin      =  0.0,
	.sd_offset_x =  0.0,
	.sd_offset_y =  0.0,
	.ol_cl       = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
	.bd_cl       = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
	.bg_cl       = { .r = 0.200, .g = 0.000, .b = 0.000, .a = 1.000 },
	.sd_cl       = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
	.ol_shape    = true,
	.bd_shape    = true,
	.draw        = true,
	.sd_draw     = false,
	.cn_smart    = true,
	.ol_hit      = false,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const struct cgui_text blank_text =
{
	.cl      = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
	.bg_cl   = { .r = 0.900, .g = 0.900, .b = 0.900, .a = 1.000 },
	.bg_draw = false,
	.bold    = false,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const struct cgui_config config_default =
{
	.loads = 0,

	/* rendering */

	.render_mode          = CGUI_RENDER_DEFERRED,
	.render_sync_vblank   = true,
	.render_sync_bypass   = true,
	.render_partial       = true,
	.render_scale         = 1.0,
	.render_fps_async_cap = DBL_MAX,
	.render_fps_sync_div  = 1,

	/* inputs */

	.input_modkey         = CGUI_MOD_CTRL,
	.input_auto_lock      = true,
	.input_sticky_pointer = false,
	.input_sticky_touch   = false,
	.input_wm_move        = 0,
	.input_wm_resize      = 0,
	.input_wm_fullscreen  = 0,

	/* font */

	.font_pad_pattern = "_",
	.font_face        = "Monospace",
	.font_size        = 14.0,
	.font_hgap        = 0.0,
	.font_vgap        = 2.0,
	.font_offset_x    = 0.0,
	.font_offset_y    = 0.0,
	.font_width       = 7.0,
	.font_ascent      = 14.0,
	.font_descent     = 0.0,
	.font_bg_hpad     = 0.0,
	.font_bg_vpad     = 0.0,
	.font_override    = false,
	.font_hints       = true,
	.font_antialias   = CGUI_ANTIALIAS_SUBPIXEL,
	.font_subpixel    = CGUI_SUBPIXEL_RGB,

	/* shadows */
	
	.shadows_reactive       = false,
	.shadows_max_light_dist = 0.0,
	.shadows_max_offset     = 0.0,

	/* grid */

	.grid_pad = 10.0,
	.grid_gap = 10.0,

	/* window */

	.window           = blank_window,
	.window_focused   = blank_window,
	.window_locked    = blank_window,
	.window_disabled  = blank_window,
	.window_pad       = 20.0,
	.window_pre_focus = true,

	/* popup */

	.popup     = blank_window,
	.popup_pad = 10.0,

	/* keys */

	.keys = {{{0}}},

	.keys[ 67][CGUI_SWAP_DIRECT] = { CGUI_SWAP_TO_ACCELERATOR,   1                           }, /* F1   */
	.keys[ 68][CGUI_SWAP_DIRECT] = { CGUI_SWAP_TO_ACCELERATOR,   2                           }, /* F2   */
	.keys[ 69][CGUI_SWAP_DIRECT] = { CGUI_SWAP_TO_ACCELERATOR,   3                           }, /* F3   */
	.keys[ 70][CGUI_SWAP_DIRECT] = { CGUI_SWAP_TO_ACCELERATOR,   4                           }, /* F4   */
	.keys[ 71][CGUI_SWAP_DIRECT] = { CGUI_SWAP_TO_ACCELERATOR,   5                           }, /* F5   */
	.keys[ 72][CGUI_SWAP_DIRECT] = { CGUI_SWAP_TO_ACCELERATOR,   6                           }, /* F6   */
	.keys[ 73][CGUI_SWAP_DIRECT] = { CGUI_SWAP_TO_ACCELERATOR,   7                           }, /* F7   */
	.keys[ 74][CGUI_SWAP_DIRECT] = { CGUI_SWAP_TO_ACCELERATOR,   8                           }, /* F8   */
	.keys[ 75][CGUI_SWAP_DIRECT] = { CGUI_SWAP_TO_ACCELERATOR,   9                           }, /* F9   */
	.keys[ 76][CGUI_SWAP_DIRECT] = { CGUI_SWAP_TO_ACCELERATOR,   10                          }, /* F10  */
	.keys[ 95][CGUI_SWAP_DIRECT] = { CGUI_SWAP_TO_ACCELERATOR,   11                          }, /* F11  */
	.keys[ 96][CGUI_SWAP_DIRECT] = { CGUI_SWAP_TO_ACCELERATOR,   12                          }, /* F12  */

	.keys[  9][CGUI_SWAP_MOD   ] = { CGUI_SWAP_TO_FOCUS,         CGUI_FOCUS_NONE             }, /* Esc  */
	.keys[ 23][CGUI_SWAP_MOD   ] = { CGUI_SWAP_TO_FOCUS,         CGUI_FOCUS_NEXT             }, /* Tab  */
	.keys[110][CGUI_SWAP_MOD   ] = { CGUI_SWAP_TO_FOCUS,         CGUI_FOCUS_FIRST            }, /* Home */
	.keys[115][CGUI_SWAP_MOD   ] = { CGUI_SWAP_TO_FOCUS,         CGUI_FOCUS_LAST             }, /* End  */
	.keys[ 23][CGUI_SWAP_SHIFT ] = { CGUI_SWAP_TO_FOCUS,         CGUI_FOCUS_PREV             }, /* Tab  */

	.keys[ 22][CGUI_SWAP_MOD   ] = { CGUI_SWAP_TO_ACTION_CELL,   CGUI_SWAP_CELL_REDRAW       }, /* Bspc */
	.keys[ 22][CGUI_SWAP_SHIFT ] = { CGUI_SWAP_TO_ACTION_WINDOW, CGUI_SWAP_WINDOW_REDRAW     }, /* Bspc */
	.keys[ 46][CGUI_SWAP_MOD   ] = { CGUI_SWAP_TO_ACTION_WINDOW, CGUI_SWAP_WINDOW_LOCK_FOCUS }, /* L    */
	.keys[ 46][CGUI_SWAP_SHIFT ] = { CGUI_SWAP_TO_ACTION_WINDOW, CGUI_SWAP_WINDOW_LOCK_GRID  }, /* L    */

	.keys[ 27][CGUI_SWAP_MOD   ] = { CGUI_SWAP_TO_ACTION_APP,    CGUI_SWAP_RECONFIG          }, /* R    */
	.keys[ 54][CGUI_SWAP_MOD   ] = { CGUI_SWAP_TO_ACTION_APP,    CGUI_SWAP_EXIT              }, /* C    */

	/* buttons */
	
	.buttons = {{{0}}},

	.buttons[2][CGUI_SWAP_MOD] = { CGUI_SWAP_TO_ACCELERATOR, 1 },
	.buttons[4][CGUI_SWAP_MOD] = { CGUI_SWAP_TO_VALUE,       6 },
	.buttons[5][CGUI_SWAP_MOD] = { CGUI_SWAP_TO_VALUE,       7 },

	/* cell - filler */

	.filler_frame = blank_box,

	/* cell - stripes */

	.stripes_frame      = blank_box,
	.stripes_line_cl    = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
	.stripes_line_width = 20.0,
	.stripes_line_gap   = 20.0,

	/* cell - placeholder */

	.placeholder_frame      = blank_box,
	.placeholder_line_cl    = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
	.placeholder_line_width = 20.0,

	/* cell - button */

	.button_frame_idle     = blank_box,
	.button_frame_focused  = blank_box,
	.button_frame_pressed  = blank_box,
	.button_frame_disabled = blank_box,
	.button_text_idle      = blank_text,
	.button_text_focused   = blank_text,
	.button_text_pressed   = blank_text,
	.button_text_disabled  = blank_text,

	/* cell - label */

	.label_frame = blank_box,
	.label_text  = blank_text,

	/* cell - beacon */

	.beacon_frame_off      = blank_box,
	.beacon_frame_on       = blank_box,
	.beacon_frame_crit_off = blank_box,
	.beacon_frame_crit_on  = blank_box,
	.beacon_text_off       = blank_text,
	.beacon_text_on        = blank_text,
	.beacon_text_crit_off  = blank_text,
	.beacon_text_crit_on   = blank_text,
	.beacon_blink_on       = 500,
	.beacon_blink_off      = 500,

	/* cell - gauge */

	.gauge_frame      = blank_box,
	.gauge_bar        = blank_box,
	.gauge_cursor     = blank_box,
	.gauge_text       = blank_text,
	.gauge_min_length = 20.0,
	.gauge_max_thick  = DBL_MAX,
};

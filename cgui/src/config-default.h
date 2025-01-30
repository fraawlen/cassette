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

	.window =
	{
		.corner           = {CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT},
		.color_border     = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_background = { .r = 0.200, .g = 0.200, .b = 0.200, .a = 0.800 },
		.size_corner      = { 0.0, 0.0, 0.0, 0.0 },
		.size_border      = 10,
		.ena              = false,
	},

	.window_focused =
	{
		.corner           = {CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT},
		.color_border     = { .r = 0.671, .g = 0.671, .b = 0.671, .a = 1.000 },
		.color_background = { .r = 0.200, .g = 0.200, .b = 0.200, .a = 0.800 },
		.size_corner      = { 0.0, 0.0, 0.0, 0.0 },
		.size_border      = 10,
		.ena              = false,
	},

	.window_locked =
	{
		.corner           = {CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT},
		.color_border     = { .r = 0.500, .g = 0.100, .b = 0.100, .a = 1.000 },
		.color_background = { .r = 0.200, .g = 0.200, .b = 0.200, .a = 0.800 },
		.size_corner      = { 0.0, 0.0, 0.0, 0.0 },
		.size_border      = 10,
		.ena              = false,
	},

	.window_disabled =
	{
		.corner           = {CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT},
		.color_border     = { .r = 0.400, .g = 0.400, .b = 0.400, .a = 1.000 },
		.color_background = { .r = 0.200, .g = 0.200, .b = 0.200, .a = 0.800 },
		.size_corner      = { 0.0, 0.0, 0.0, 0.0 },
		.size_border      = 10,
		.ena              = false,
	},

	.window_pad       = 20.0,
	.window_pre_focus = true,

	/* popup */

	.popup_pad = 10.0,
	.popup =
	{
		.corner           = {CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT},
		.color_border     = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_background = { .r = 0.200, .g = 0.200, .b = 0.200, .a = 0.800 },
		.size_corner      = { 0.0, 0.0, 0.0, 0.0 },
		.size_border      = 10,
	},

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

	.keys[ 27][CGUI_SWAP_MOD   ] = { CGUI_SWAP_TO_ACTION_MISC,   CGUI_SWAP_RECONFIG          }, /* R    */
	.keys[ 54][CGUI_SWAP_MOD   ] = { CGUI_SWAP_TO_ACTION_MISC,   CGUI_SWAP_EXIT              }, /* C    */

	/* buttons */
	
	.buttons = {{{0}}},

	.buttons[2][CGUI_SWAP_MOD] = { CGUI_SWAP_TO_ACCELERATOR, 1 },
	.buttons[4][CGUI_SWAP_MOD] = { CGUI_SWAP_TO_VALUE,       6 },
	.buttons[5][CGUI_SWAP_MOD] = { CGUI_SWAP_TO_VALUE,       7 },

	/* cell - filler */

	.filler_frame =
	{
		.corner           = {CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT},
		.size_corner      = {0, 0, 0, 0},
		.size_outline     =  0,
		.size_border      = 10,
		.padding          = 10,
		.margin           =  0,
		.shadow_x_offset  =  0,
		.shadow_y_offset  =  0,
		.color_outline    = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_border     = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_background = { .r = 0.200, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_shadow     = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.shape_outline    = true,
		.shape_border     = true,
		.draw             = true,
		.draw_shadow      = false,
		.smart_corners    = true,
		.hit_outline      = false,
	},

	/* cell - stripes */

	.stripes_frame =
	{
		.corner           = {CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT},
		.size_corner      = {0, 0, 0, 0},
		.size_outline     =  0,
		.size_border      = 10,
		.padding          = 10,
		.margin           =  0,
		.shadow_x_offset  =  0,
		.shadow_y_offset  =  0,
		.color_outline    = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_border     = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_background = { .r = 0.200, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_shadow     = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.shape_outline    = true,
		.shape_border     = true,
		.draw             = true,
		.draw_shadow      = false,
		.smart_corners    = true,
		.hit_outline      = false,
	},

	.stripes_line_cl    = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
	.stripes_line_width = 20.0,
	.stripes_line_gap   = 20.0,

	/* cell - placeholder */

	.placeholder_frame =
	{
		.corner           = {CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT},
		.size_corner      = {0, 0, 0, 0},
		.size_outline     =  0,
		.size_border      = 10,
		.padding          = 10,
		.margin           =  0,
		.shadow_x_offset  =  0,
		.shadow_y_offset  =  0,
		.color_outline    = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_border     = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_background = { .r = 0.200, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_shadow     = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.shape_outline    = true,
		.shape_border     = true,
		.draw             = true,
		.draw_shadow      = false,
		.smart_corners    = true,
		.hit_outline      = false,
	},

	.placeholder_line_cl    = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
	.placeholder_line_width = 20.0,

	/* cell - button */

	.button_frame_idle =
	{
		.corner           = {CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT},
		.size_corner      = {0, 0, 0, 0},
		.size_outline     =  0,
		.size_border      = 10,
		.padding          = 10,
		.margin           =  0,
		.shadow_x_offset  =  0,
		.shadow_y_offset  =  0,
		.color_outline    = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_border     = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_background = { .r = 0.200, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_shadow     = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.shape_outline    = true,
		.shape_border     = true,
		.draw             = true,
		.draw_shadow      = false,
		.smart_corners    = true,
		.hit_outline      = false,
	},

	.button_frame_focused =
	{
		.corner           = {CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT},
		.size_corner      = {0, 0, 0, 0},
		.size_outline     =  0,
		.size_border      = 10,
		.padding          = 10,
		.margin           =  0,
		.shadow_x_offset  =  0,
		.shadow_y_offset  =  0,
		.color_outline    = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_border     = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_background = { .r = 0.200, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_shadow     = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.shape_outline    = true,
		.shape_border     = true,
		.draw             = true,
		.draw_shadow      = false,
		.smart_corners    = true,
		.hit_outline      = false,
	},

	.button_frame_pressed =
	{
		.corner           = {CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT},
		.size_corner      = {0, 0, 0, 0},
		.size_outline     =  0,
		.size_border      = 10,
		.padding          = 10,
		.margin           =  0,
		.shadow_x_offset  =  0,
		.shadow_y_offset  =  0,
		.color_outline    = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_border     = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_background = { .r = 0.200, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_shadow     = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.shape_outline    = true,
		.shape_border     = true,
		.draw             = true,
		.draw_shadow      = false,
		.smart_corners    = true,
		.hit_outline      = false,
	},

	.button_frame_disabled =
	{
		.corner           = {CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT},
		.size_corner      = {0, 0, 0, 0},
		.size_outline     =  0,
		.size_border      = 10,
		.padding          = 10,
		.margin           =  0,
		.shadow_x_offset  =  0,
		.shadow_y_offset  =  0,
		.color_outline    = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_border     = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_background = { .r = 0.200, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_shadow     = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.shape_outline    = true,
		.shape_border     = true,
		.draw             = true,
		.draw_shadow      = false,
		.smart_corners    = true,
		.hit_outline      = false,
	},

	.button_text_idle =
	{
		.color            = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_background = { .r = 0.900, .g = 0.900, .b = 0.900, .a = 1.000 },
		.draw_background  = false,
		.bold             = false,
	},

	.button_text_focused =
	{
		.color            = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_background = { .r = 0.900, .g = 0.900, .b = 0.900, .a = 1.000 },
		.draw_background  = false,
		.bold             = false,
	},

	.button_text_pressed =
	{
		.color            = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_background = { .r = 0.900, .g = 0.900, .b = 0.900, .a = 1.000 },
		.draw_background  = false,
		.bold             = false,
	},

	.button_text_disabled =
	{
		.color            = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_background = { .r = 0.900, .g = 0.900, .b = 0.900, .a = 1.000 },
		.draw_background  = false,
		.bold             = false,
	},

	/* cell - label */

	.label_frame =
	{
		.corner           = {CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT},
		.size_corner      = {0, 0, 0, 0},
		.size_outline     =  0,
		.size_border      = 10,
		.padding          = 10,
		.margin           =  0,
		.shadow_x_offset  =  0,
		.shadow_y_offset  =  0,
		.color_outline    = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_border     = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_background = { .r = 0.200, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_shadow     = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.shape_outline    = true,
		.shape_border     = true,
		.draw             = true,
		.draw_shadow      = false,
		.smart_corners    = true,
		.hit_outline      = false,
	},

	.label_text =
	{
		.color            = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_background = { .r = 0.900, .g = 0.900, .b = 0.900, .a = 1.000 },
		.draw_background  = false,
		.bold             = false,
	},

	/* cell - beacon */

	.beacon_frame_off =
	{
		.corner           = {CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT},
		.size_corner      = {0, 0, 0, 0},
		.size_outline     =  0,
		.size_border      = 10,
		.padding          = 10,
		.margin           =  0,
		.shadow_x_offset  =  0,
		.shadow_y_offset  =  0,
		.color_outline    = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_border     = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_background = { .r = 0.200, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_shadow     = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.shape_outline    = true,
		.shape_border     = true,
		.draw             = true,
		.draw_shadow      = false,
		.smart_corners    = true,
		.hit_outline      = false,
	},

	.beacon_frame_on =
	{
		.corner           = {CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT},
		.size_corner      = {0, 0, 0, 0},
		.size_outline     =  0,
		.size_border      = 10,
		.padding          = 10,
		.margin           =  0,
		.shadow_x_offset  =  0,
		.shadow_y_offset  =  0,
		.color_outline    = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_border     = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_background = { .r = 0.200, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_shadow     = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.shape_outline    = true,
		.shape_border     = true,
		.draw             = true,
		.draw_shadow      = false,
		.smart_corners    = true,
		.hit_outline      = false,
	},

	.beacon_frame_crit_off =
	{
		.corner           = {CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT},
		.size_corner      = {0, 0, 0, 0},
		.size_outline     =  0,
		.size_border      = 10,
		.padding          = 10,
		.margin           =  0,
		.shadow_x_offset  =  0,
		.shadow_y_offset  =  0,
		.color_outline    = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_border     = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_background = { .r = 0.200, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_shadow     = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.shape_outline    = true,
		.shape_border     = true,
		.draw             = true,
		.draw_shadow      = false,
		.smart_corners    = true,
		.hit_outline      = false,
	},

	.beacon_frame_crit_on =
	{
		.corner           = {CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT},
		.size_corner      = {0, 0, 0, 0},
		.size_outline     =  0,
		.size_border      = 10,
		.padding          = 10,
		.margin           =  0,
		.shadow_x_offset  =  0,
		.shadow_y_offset  =  0,
		.color_outline    = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_border     = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_background = { .r = 0.200, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_shadow     = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.shape_outline    = true,
		.shape_border     = true,
		.draw             = true,
		.draw_shadow      = false,
		.smart_corners    = true,
		.hit_outline      = false,
	},

	.beacon_text_off =
	{
		.color            = { .r = 0.900, .g = 0.900, .b = 0.900, .a = 1.000 },
		.color_background = { .r = 0.900, .g = 0.900, .b = 0.900, .a = 1.000 },
		.draw_background  = false,
		.bold             = false,
	},

	.beacon_text_on =
	{
		.color            = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_background = { .r = 0.900, .g = 0.900, .b = 0.900, .a = 1.000 },
		.draw_background  = false,
		.bold             = true,
	},

	.beacon_text_crit_off =
	{
		.color            = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_background = { .r = 0.900, .g = 0.900, .b = 0.900, .a = 1.000 },
		.draw_background  = false,
		.bold             = false,
	},

	.beacon_text_crit_on =
	{
		.color            = { .r = 0.900, .g = 0.900, .b = 0.900, .a = 1.000 },
		.color_background = { .r = 0.900, .g = 0.900, .b = 0.900, .a = 1.000 },
		.draw_background  = false,
		.bold             = true,
	},

	.beacon_blink_on  = 500,
	.beacon_blink_off = 500,

	/* cell - gauge */

	.gauge_frame =
	{
		.corner           = {CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT},
		.size_corner      = {0, 0, 0, 0},
		.size_outline     =  0,
		.size_border      = 10,
		.padding          = 10,
		.margin           =  0,
		.shadow_x_offset  =  0,
		.shadow_y_offset  =  0,
		.color_outline    = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_border     = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_background = { .r = 0.200, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_shadow     = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.shape_outline    = true,
		.shape_border     = true,
		.draw             = true,
		.draw_shadow      = false,
		.smart_corners    = true,
		.hit_outline      = false,
	},

	.gauge_bar =
	{
		.corner           = {CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT},
		.size_corner      = {0, 0, 0, 0},
		.size_outline     =  0,
		.size_border      = 10,
		.padding          =  0,
		.margin           =  0,
		.shadow_x_offset  =  0,
		.shadow_y_offset  =  0,
		.color_outline    = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_border     = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_background = { .r = 0.200, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_shadow     = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.shape_outline    = true,
		.shape_border     = true,
		.draw             = true,
		.draw_shadow      = false,
		.smart_corners    = true,
		.hit_outline      = false,
	},

	.gauge_cursor =
	{
		.corner           = {CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT, CGUI_CORNER_STRAIGHT},
		.size_corner      = {0, 0, 0, 0},
		.size_outline     =  0,
		.size_border      = 10,
		.padding          =  0,
		.margin           =  0,
		.shadow_x_offset  =  0,
		.shadow_y_offset  =  0,
		.color_outline    = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_border     = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_background = { .r = 0.200, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_shadow     = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.shape_outline    = true,
		.shape_border     = true,
		.draw             = true,
		.draw_shadow      = false,
		.smart_corners    = true,
		.hit_outline      = false,
	},

	.gauge_text =
	{
		.color            = { .r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000 },
		.color_background = { .r = 0.900, .g = 0.900, .b = 0.900, .a = 1.000 },
		.draw_background  = false,
		.bold             = false,
	},

	.gauge_min_length = 20.0,
	.gauge_max_thick  = DBL_MAX,
};

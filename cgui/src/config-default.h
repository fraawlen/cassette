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
/************************************************************************************************************/
/************************************************************************************************************/

#define COLOR(X) { ((X >> 16) & 0xFF) / 255.0, \
                   ((X >>  8) & 0xFF) / 255.0, \
                    (X        & 0xFF) / 255.0, 1.0 }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

#define WIN(BD_CL) \
{ \
	.cn_type = {CGUI_CORNER_SQUARE, CGUI_CORNER_SQUARE, CGUI_CORNER_SQUARE, CGUI_CORNER_SQUARE}, \
	.bd_cl   = COLOR(BD_CL), \
	.bg_cl   = COLOR(0x202020), \
	.cn_size = {0.0, 0.0, 0.0, 0.0}, \
	.bd_size = 6.0, \
	.ena     = true, \
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

#define BOX(MARGIN, PAD, BD_SIZE, OL_SIZE, BG_CL, BD_CL, OL_CL) \
{ \
	.cn_type     = {CGUI_CORNER_SQUARE, CGUI_CORNER_SQUARE, CGUI_CORNER_SQUARE, CGUI_CORNER_SQUARE}, \
	.cn_size     = {0.0, 0.0, 0.0, 0.0}, \
	.ol_size     = OL_SIZE, \
	.bd_size     = BD_SIZE, \
	.pad         = PAD, \
	.margin      = MARGIN, \
	.sd_offset_x =  0.0, \
	.sd_offset_y =  0.0, \
	.ol_cl       = COLOR(OL_CL), \
	.bd_cl       = COLOR(BD_CL), \
	.bg_cl       = COLOR(BG_CL), \
	.sd_cl       = COLOR(0x000000), \
	.ol_shape    = true, \
	.bd_shape    = true, \
	.draw        = true, \
	.sd_draw     = false, \
	.cn_smart    = true, \
	.ol_hit      = false, \
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

#define TXT(CL, BOLD) \
{ \
	.cl      = COLOR(CL), \
	.bg_cl   = COLOR(0x000000), \
	.bg_draw = false, \
	.bold    = BOLD, \
}

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
	.render_overlap       = false,
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
	.shadows_max_light_dist = 200.0,
	.shadows_max_offset     = 10.0,

	/* grid */

	.grid_pad = 18.0,
	.grid_gap = 10.0,

	/* window */

	.window           = WIN(0x808080),
	.window_focused   = WIN(0xCCCCCC),
	.window_locked    = WIN(0xCC0000),
	.window_disabled  = WIN(0x000000),
	.window_pad       = 19.0,
	.window_pre_focus = true,

	/* popup */

	.popup     = WIN(0x00CCCC),
	.popup_pad = 13.0,

	/* keys */

	.keys = {{{0}}},

	.keys[ 67][CGUI_SWAP_DIRECT] = { CGUI_SWAP_TO_ACCELERATOR,     1                           }, /* F1   */
	.keys[ 68][CGUI_SWAP_DIRECT] = { CGUI_SWAP_TO_ACCELERATOR,     2                           }, /* F2   */
	.keys[ 69][CGUI_SWAP_DIRECT] = { CGUI_SWAP_TO_ACCELERATOR,     3                           }, /* F3   */
	.keys[ 70][CGUI_SWAP_DIRECT] = { CGUI_SWAP_TO_ACCELERATOR,     4                           }, /* F4   */
	.keys[ 71][CGUI_SWAP_DIRECT] = { CGUI_SWAP_TO_ACCELERATOR,     5                           }, /* F5   */
	.keys[ 72][CGUI_SWAP_DIRECT] = { CGUI_SWAP_TO_ACCELERATOR,     6                           }, /* F6   */
	.keys[ 73][CGUI_SWAP_DIRECT] = { CGUI_SWAP_TO_ACCELERATOR,     7                           }, /* F7   */
	.keys[ 74][CGUI_SWAP_DIRECT] = { CGUI_SWAP_TO_ACCELERATOR,     8                           }, /* F8   */
	.keys[ 75][CGUI_SWAP_DIRECT] = { CGUI_SWAP_TO_ACCELERATOR,     9                           }, /* F9   */
	.keys[ 76][CGUI_SWAP_DIRECT] = { CGUI_SWAP_TO_ACCELERATOR,     10                          }, /* F10  */
	.keys[ 95][CGUI_SWAP_DIRECT] = { CGUI_SWAP_TO_ACCELERATOR,     11                          }, /* F11  */
	.keys[ 96][CGUI_SWAP_DIRECT] = { CGUI_SWAP_TO_ACCELERATOR,     12                          }, /* F12  */

	.keys[ 53][CGUI_SWAP_MOD   ] = { CGUI_SWAP_TO_CLIPBOARD_CUT,   1                           }, /* X */
	.keys[ 53][CGUI_SWAP_SHIFT ] = { CGUI_SWAP_TO_CLIPBOARD_CUT,   2                           }, /* X */
	.keys[ 54][CGUI_SWAP_MOD   ] = { CGUI_SWAP_TO_CLIPBOARD_COPY,  1                           }, /* C */
	.keys[ 54][CGUI_SWAP_SHIFT ] = { CGUI_SWAP_TO_CLIPBOARD_COPY,  2                           }, /* C */
	.keys[ 33][CGUI_SWAP_MOD   ] = { CGUI_SWAP_TO_CLIPBOARD_PASTE, 1                           }, /* P */
	.keys[ 33][CGUI_SWAP_SHIFT ] = { CGUI_SWAP_TO_CLIPBOARD_PASTE, 2                           }, /* P */
 
	.keys[  9][CGUI_SWAP_MOD   ] = { CGUI_SWAP_TO_FOCUS,           CGUI_FOCUS_NONE             }, /* Esc  */
	.keys[ 23][CGUI_SWAP_MOD   ] = { CGUI_SWAP_TO_FOCUS,           CGUI_FOCUS_NEXT             }, /* Tab  */
	.keys[110][CGUI_SWAP_MOD   ] = { CGUI_SWAP_TO_FOCUS,           CGUI_FOCUS_FIRST            }, /* Home */
	.keys[115][CGUI_SWAP_MOD   ] = { CGUI_SWAP_TO_FOCUS,           CGUI_FOCUS_LAST             }, /* End  */
	.keys[ 23][CGUI_SWAP_SHIFT ] = { CGUI_SWAP_TO_FOCUS,           CGUI_FOCUS_PREV             }, /* Tab  */

	.keys[ 22][CGUI_SWAP_MOD   ] = { CGUI_SWAP_TO_ACTION_CELL,     CGUI_SWAP_CELL_REDRAW       }, /* Bspc */
	.keys[ 22][CGUI_SWAP_SHIFT ] = { CGUI_SWAP_TO_ACTION_WINDOW,   CGUI_SWAP_WINDOW_REDRAW     }, /* Bspc */
	.keys[ 46][CGUI_SWAP_MOD   ] = { CGUI_SWAP_TO_ACTION_WINDOW,   CGUI_SWAP_WINDOW_LOCK_FOCUS }, /* L    */
	.keys[ 46][CGUI_SWAP_SHIFT ] = { CGUI_SWAP_TO_ACTION_WINDOW,   CGUI_SWAP_WINDOW_LOCK_GRID  }, /* L    */

	.keys[ 27][CGUI_SWAP_MOD   ] = { CGUI_SWAP_TO_ACTION_APP,      CGUI_SWAP_RECONFIG          }, /* R    */
	.keys[ 24][CGUI_SWAP_MOD   ] = { CGUI_SWAP_TO_ACTION_APP,      CGUI_SWAP_EXIT              }, /* Q    */

	/* buttons */
	
	.buttons = {{{0}}},

	.buttons[2][CGUI_SWAP_MOD] = { CGUI_SWAP_TO_ACCELERATOR, 1 },
	.buttons[4][CGUI_SWAP_MOD] = { CGUI_SWAP_TO_VALUE,       6 },
	.buttons[5][CGUI_SWAP_MOD] = { CGUI_SWAP_TO_VALUE,       7 },

	/* cell - filler */

	.filler_frame           = BOX(0.0, 15.0, 3.0, 0.0, 0x404040, 0x808080, 0x808080),

	/* cell - stripes */

	.stripes_frame          = BOX(0.0,  0.0, 3.0, 0.0, 0x404040, 0x808080, 0x808080),
	.stripes_line_cl        = COLOR(0x808080),
	.stripes_line_width     =  3.0,
	.stripes_line_gap       = 15.0,

	/* cell - placeholder */

	.placeholder_frame      = BOX(0.0,  0.0, 3.0, 0.0, 0x404040, 0x808080, 0x808080),
	.placeholder_line_cl    = COLOR(0x808080),
	.placeholder_line_width = 3.0,

	/* cell - button */

	.button_frame_idle      = BOX(0.0, 15.0, 3.0, 0.0, 0x602000, 0x808080, 0x808080),
	.button_frame_focused   = BOX(0.0, 15.0, 3.0, 0.0, 0xA04000, 0xCCCCCC, 0x808080),
	.button_frame_pressed   = BOX(0.0, 15.0, 3.0, 0.0, 0xFFFFFF, 0x808080, 0x808080),
	.button_frame_disabled  = BOX(0.0, 15.0, 3.0, 0.0, 0x301000, 0x808080, 0x808080),
	.button_text_idle       = TXT(0xFFFFFF, false),
	.button_text_focused    = TXT(0xFFFFFF, false),
	.button_text_pressed    = TXT(0x000000, true),
	.button_text_disabled   = TXT(0x808080, false),

	/* cell - label */

	.label_frame            = BOX(0.0, 15.0, 3.0, 0.0, 0x404040, 0x808080, 0x808080),
	.label_text             = TXT(0xFFFFFF, false),

	/* cell - beacon */

	.beacon_frame_off       = BOX(3.0,  9.0, 6.0, 3.0, 0x404040, 0x202020, 0x808080),
	.beacon_frame_on        = BOX(3.0,  9.0, 6.0, 3.0, 0x9B2E21, 0x401A16, 0x808080),
	.beacon_frame_crit_off  = BOX(3.0,  9.0, 6.0, 3.0, 0x404040, 0x202020, 0x808080),
	.beacon_frame_crit_on   = BOX(3.0,  9.0, 6.0, 3.0, 0x9B2E21, 0x401A16, 0x9B2E21),
	.beacon_text_off        = TXT(0xFFFFFF, false),
	.beacon_text_on         = TXT(0xFFFFFF, true),
	.beacon_text_crit_off   = TXT(0xFFFFFF, false),
	.beacon_text_crit_on    = TXT(0xFFFFFF, true),
	.beacon_blink_on        = 500,
	.beacon_blink_off       = 500,

	/* cell - gauge */

	.gauge_frame            = BOX(0.0,  3.0, 3.0, 0.0, 0x404040, 0x808080, 0x808080),
	.gauge_bar              = BOX(0.0, -3.0, 3.0, 0.0, 0x003030, 0x808080, 0x808080),
	.gauge_cursor           = BOX(0.0,  9.0, 3.0, 0.0, 0x004545, 0x808080, 0x808080),
	.gauge_text             = TXT(0xFFFFFF, false),
	.gauge_min_length       = 0.0,
	.gauge_max_thick        = DBL_MAX,
	.gauge_clip             = false,
};

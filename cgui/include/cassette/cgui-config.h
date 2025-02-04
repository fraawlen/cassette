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

#include <cassette/ccfg.h>
#include <cassette/cobj.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#include "cgui-attributes.h"
#include "cgui-box.h"
#include "cgui-text.h"
#include "cgui-swap.h"
#include "cgui-window.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/* GLOBALS **************************************************************************************************/
/************************************************************************************************************/

#define CGUI_CONFIG_STR_LEN 64
#define CGUI_CONFIG_ACCELS  12
#define CGUI_CONFIG_BUTTONS 12
#define CGUI_CONFIG_KEYS    128
#define CGUI_CONFIG_TOUCHES 10

/************************************************************************************************************/
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
struct cgui_config
{
	unsigned long loads;

	/* rendering */

	int render_mode;
	bool render_sync_vblank;
	bool render_sync_bypass;
	bool render_partial;
	bool render_overlap;
	double render_scale;
	double render_fps_async_cap;
	unsigned long render_fps_sync_div;

	/* inputs */

	int input_modkey;
	bool input_auto_lock;
	bool input_sticky_pointer;
	bool input_sticky_touch;
	uint8_t input_wm_move;
	uint8_t input_wm_resize;
	uint8_t input_wm_fullscreen;

	/* input swaps */

	struct cgui_swap    keys[CGUI_CONFIG_KEYS    + 1][3];
	struct cgui_swap buttons[CGUI_CONFIG_BUTTONS + 1][3];

	/* font */

	char font_face[CGUI_CONFIG_STR_LEN];
	char font_pad_pattern[CGUI_CONFIG_STR_LEN];
	double font_size;
	double font_width;
	double font_height;
	double font_ascent;
	double font_descent;
	double font_hgap;
	double font_vgap;
	double font_offset_x;
 	double font_offset_y;
	double font_bg_vpad;
	double font_bg_hpad;
	bool font_override;
	bool font_hints;
	int font_antialias;
	int font_subpixel;

	/* shadows */

	bool shadows_reactive;
	double shadows_max_light_dist;
	double shadows_max_offset;

	/* grid */

	double grid_pad;
	double grid_gap;

	/* window */

	bool window_pre_focus;
	double window_pad;
	struct cgui_window_style window;
	struct cgui_window_style window_focused;
	struct cgui_window_style window_disabled;
	struct cgui_window_style window_locked;

	/* popup */
	
	struct cgui_window_style popup;
	double popup_pad;

	/* cell - placeholder */

	struct cgui_box filler_frame;

	/* cell - stripes */

	struct cgui_box stripes_frame;
	struct ccolor stripes_line_cl;
	double stripes_line_width;
	double stripes_line_gap;

	/* cell - placeholder */

	struct cgui_box placeholder_frame;
	struct ccolor placeholder_line_cl;
	double placeholder_line_width;

	/* cell - button */

	struct cgui_box button_frame_idle;
	struct cgui_box button_frame_focused;
	struct cgui_box button_frame_pressed;
	struct cgui_box button_frame_disabled;
	struct cgui_text button_text_idle;
	struct cgui_text button_text_focused;
	struct cgui_text button_text_pressed;
	struct cgui_text button_text_disabled;

	/* cell - label */

	struct cgui_box  label_frame;
	struct cgui_text label_text;

	/* cell - beacon */

	struct cgui_box beacon_frame_off;
	struct cgui_box beacon_frame_on;
	struct cgui_box beacon_frame_crit_off;
	struct cgui_box beacon_frame_crit_on;
	struct cgui_text beacon_text_off;
	struct cgui_text beacon_text_on;
	struct cgui_text beacon_text_crit_off;
	struct cgui_text beacon_text_crit_on;
	unsigned long beacon_blink_on;
	unsigned long beacon_blink_off;

	/* cell - gauge */

	struct cgui_box gauge_frame;
	struct cgui_box gauge_bar;
	struct cgui_box gauge_cursor;
	struct cgui_text gauge_text;
	double gauge_min_length;
	double gauge_max_thick;
	bool gauge_clip;
};

/************************************************************************************************************/
/* IMPURE METHODS *******************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
void
cgui_config_on_load(void (*fn)(ccfg *cfg));

/**
 *
 */
void
cgui_config_style_box(const char *name, struct cgui_box *box)
CGUI_NONNULL(1, 2);

/**
 *
 */
void
cgui_config_style_text(const char *name, struct cgui_text *text)
CGUI_NONNULL(1, 2);

/************************************************************************************************************/
/* PURE METHODS *********************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
size_t
cgui_config_fit_cols(double width)
CGUI_PURE;

/**
 *
 */
size_t
cgui_config_fit_rows(double height)
CGUI_PURE;

/**
 *
 */
const struct cgui_config *
cgui_config_get(void)
CGUI_NONNULL_RETURN
CGUI_PURE;

/**
 *
 */
ccfg *
cgui_config_get_parser(void)
CGUI_NONNULL_RETURN
CGUI_PURE;

/**
 *
 */
double
cgui_config_str_height(ssize_t rows)
CGUI_PURE;

/**
 *
 */
double
cgui_config_str_width(ssize_t cols)
CGUI_PURE;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

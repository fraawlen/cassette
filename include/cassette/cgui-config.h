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

#include <cassette/ccfg.h>
#include <cassette/cobj.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#include "cgui-attributes.h"
#include "cgui-box.h"
#include "cgui-swap.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/* GLOBALS **************************************************************************************************/
/************************************************************************************************************/

#define CGUI_CONFIG_STR_LEN    64
#define CGUI_CONFIG_CLIPBOARDS 3
#define CGUI_CONFIG_ACCELS     12
#define CGUI_CONFIG_BUTTONS    12
#define CGUI_CONFIG_KEYS       128

/************************************************************************************************************/
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
enum cgui_config_modkey
{
	CGUI_CONFIG_MOD_NONE  = 0,      /* cannor be used as modkey config option */
	CGUI_CONFIG_MOD_LOCK  = 1,      /* cannot be used as modkey config option */
	CGUI_CONFIG_MOD_CTRL  = 1 << 1, 
	CGUI_CONFIG_MOD_1     = 1 << 2, 
	CGUI_CONFIG_MOD_2     = 1 << 3, /* cannot be used as modkey config option */
	CGUI_CONFIG_MOD_3     = 1 << 4, /* cannot be used as modkey config option */
	CGUI_CONFIG_MOD_4     = 1 << 5,
	CGUI_CONFIG_MOD_5     = 1 << 6, /* cannot be used as modkey config option */
};

/**
 *
 */
enum cgui_config_antialias
{
	CGUI_CONFIG_ANTIALIAS_NONE,
	CGUI_CONFIG_ANTIALIAS_GRAY,
	CGUI_CONFIG_ANTIALIAS_SUBPIXEL,
};

/**
 *
 */
enum cgui_config_subpixel
{
	CGUI_CONFIG_SUBPIXEL_RGB,
	CGUI_CONFIG_SUBPIXEL_BGR,
	CGUI_CONFIG_SUBPIXEL_VRGB,
	CGUI_CONFIG_SUBPIXEL_VBGR,
};

/**
 *
 */
enum cgui_config_swap_level
{
	CGUI_CONFIG_SWAP_DIRECT = 0,
	CGUI_CONFIG_SWAP_MOD    = 1,
	CGUI_CONFIG_SWAP_SHIFT  = 2,
};

/**
 *
 */
struct cgui_config
{
	bool init;
	double scale;
	enum cgui_config_modkey modkey;

	/* font */

	char font_face[CGUI_CONFIG_STR_LEN];

	double font_size;
	double font_width;
	double font_height;
	double font_ascent;
	double font_descent;
	double font_spacing_horizontal;
	double font_spacing_vertical;
	double font_override_width;
	double font_override_ascent;
	double font_override_descent;
	double font_offset_x;
 	double font_offset_y;

	bool font_enable_overrides;
	bool font_enable_hint_metrics;
	enum cgui_config_antialias font_antialias;
	enum cgui_config_subpixel font_subpixel;

	/* grid */

	double grid_padding;
	double grid_spacing;

	/* window */

	enum cgui_box_corner window_corner[4];

	double window_size_corner[4];
	double window_size_border;
	double window_padding;

	struct ccolor window_color_border;
	struct ccolor window_color_border_focused;
	struct ccolor window_color_border_disabled;
	struct ccolor window_color_border_locked;
	struct ccolor window_color_background;

	bool window_focus_on_activation;
	bool window_enable_disabled;
	bool window_enable_focused;
	bool window_enable_locked;
	
	/* popup */

	struct ccolor popup_color_background;
	struct ccolor popup_color_border;

	double popup_border;
	double popup_padding;
	double popup_max_width;
	double popup_max_height;
	double popup_override_width;
	double popup_override_height;
	double popup_override_x;
	double popup_override_y;

	bool popup_enable_override_position;
	bool popup_enable_override_width;
	bool popup_enable_override_height;

	/* behavior */

	bool cell_auto_lock;
	bool input_persistent_pointer;
	bool input_persistent_touch;
	unsigned long anim_divider;

	/* input swaps */

	struct cgui_swap    keys[CGUI_CONFIG_KEYS    + 1][3];
	struct cgui_swap buttons[CGUI_CONFIG_BUTTONS + 1][3];

	/* cell - placeholder */

	struct cgui_box placeholder_frame;
};

/************************************************************************************************************/
/* IMPURE METHODS *******************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
void
cgui_config_on_load(void (*fn)(ccfg *cfg));

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
double
cgui_config_str_height(size_t rows)
CGUI_PURE;

/**
 *
 */
double
cgui_config_str_width(size_t cols)
CGUI_PURE;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

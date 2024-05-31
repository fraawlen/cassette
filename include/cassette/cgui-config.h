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

#ifndef CGUI_CONFIG_H
#define CGUI_CONFIG_H

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#include <cassette/ccfg.h>
#include <cassette/cobj.h>

#include "cgui-input-swap.h"
#include "cgui-style.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define CGUI_CONFIG_CLIPBOARDS 3
#define CGUI_CONFIG_ACCELS     12
#define CGUI_CONFIG_BUTTONS    12
#define CGUI_CONFIG_KEYS       128

#define CGUI_CONFIG_SWAP_DIRECT 0
#define CGUI_CONFIG_SWAP_MOD    1
#define CGUI_CONFIG_SWAP_SHIFT  2

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cgui_config_modkey_t
{
	CGUI_CONFIG_MOD_NONE  = 0,       /* cannor be used as modkey config option */
	CGUI_CONFIG_MOD_LOCK  = 1U << 1, /* cannot be used as modkey config option */
	CGUI_CONFIG_MOD_CTRL  = 1U << 2, 
	CGUI_CONFIG_MOD_1     = 1U << 3, 
	CGUI_CONFIG_MOD_2     = 1U << 4, /* cannot be used as modkey config option */
	CGUI_CONFIG_MOD_3     = 1U << 5, /* cannot be used as modkey config option */
	CGUI_CONFIG_MOD_4     = 1U << 6,
	CGUI_CONFIG_MOD_5     = 1U << 7, /* cannot be used as modkey config option */
};

typedef size_t cgui_config_modkey_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cgui_config_font_antialias_t
{
	CGUI_CONFIG_ANTIALIAS_NONE,
	CGUI_CONFIG_ANTIALIAS_GRAY,
	CGUI_CONFIG_ANTIALIAS_SUBPIXEL,
};

typedef size_t cgui_config_font_antialias_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cgui_config_font_subpixel_t
{
	CGUI_CONFIG_SUBPIXEL_RGB,
	CGUI_CONFIG_SUBPIXEL_BGR,
	CGUI_CONFIG_SUBPIXEL_VRGB,
	CGUI_CONFIG_SUBPIXEL_VBGR,
};

typedef size_t cgui_config_font_subpixel_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct cgui_config_t
{
	bool init;
	double scale;
	cgui_config_modkey_t modkey;

	/* font */

	char font_face[CCFG_MAX_WORD_BYTES];

	uint16_t font_size;
	uint16_t font_width;
	uint16_t font_height;
	uint16_t font_ascent;
	uint16_t font_descent;
	uint16_t font_spacing_horizontal;
	uint16_t font_spacing_vertical;
	uint16_t font_override_width;
	uint16_t font_override_ascent;
	uint16_t font_override_descent;
	int16_t  font_offset_x;
	int16_t  font_offset_y;

	bool font_enable_overrides;
	bool font_enable_hint_metrics;
	cgui_config_font_antialias_t font_antialias;
	cgui_config_font_subpixel_t font_subpixel;

	/* window */

	cgui_style_window_t popup_style;
	
	/* popup */
	
	cgui_style_window_t window_style;

	int16_t  popup_override_x;
	int16_t  popup_override_y;
	uint16_t popup_max_width;
	uint16_t popup_max_height;
	uint16_t popup_override_width;
	uint16_t popup_override_height;

	bool popup_enable_override_position;
	bool popup_enable_override_width;
	bool popup_enable_override_height;

	/* behavior */

	bool cell_auto_lock;
	bool input_persistent_pointer;
	bool input_persistent_touch;
	unsigned int anim_divider;

	/* input swaps */

	cgui_input_swap_t    keys[CGUI_CONFIG_KEYS    + 1][3];
	cgui_input_swap_t buttons[CGUI_CONFIG_BUTTONS + 1][3];
};

typedef struct cgui_config_t cgui_config_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const cgui_config_t *cgui_config_get(void);

ccfg_t *cgui_config_get_parser(void);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* CGUI_CONFIG_H */

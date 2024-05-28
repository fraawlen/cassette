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

#include <cassette/ccfg.h>
#include <cassette/cobj.h>

#include "cgui-style.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define CGUI_CONFIG_MAX_ACCELS 12
#define CGUI_CONFIG_MAX_STRING 32

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cgui_config_font_antialias_t
{
	CGUI_CONFIG_ANTIALIAS_NONE,
	CGUI_CONFIG_ANTIALIAS_GRAY,
	CGUI_CONFIG_ANTIALIAS_SUBPIXEL,
};

typedef enum cgui_config_font_antialias_t cgui_config_font_antialias_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cgui_config_font_subpixel_t
{
	CGUI_CONFIG_SUBPIXEL_RGB,
	CGUI_CONFIG_SUBPIXEL_BGR,
	CGUI_CONFIG_SUBPIXEL_VRGB,
	CGUI_CONFIG_SUBPIXEL_VBGR,
};

typedef enum cgui_config_font_subpixel_t cgui_config_font_subpixel_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct cgui_config_t
{
	bool init;
	double scale;

	/* font */

	char font_face[CGUI_CONFIG_MAX_STRING];

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

	/* misc */

	/* input swaps */
};

typedef struct cgui_config_t cgui_config_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const cgui_config_t *cgui_config_get(void);

ccfg_t *cgui_config_get_object(void);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* CGUI_CONFIG_H */

/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Objects (COBJ) library.
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

#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/**
 * Common color definitions.
 */
#define COBJ_COLOR_TRANSPARENT (cobj_color_t){0.000, 0.000, 0.000, 0.000}
#define COBJ_COLOR_WHITE       (cobj_color_t){1.000, 1.000, 1.000, 1.000}
#define COBJ_COLOR_BLACK       (cobj_color_t){0.000, 0.000, 0.000, 1.000}
#define COBJ_COLOR_RED         (cobj_color_t){1.000, 0.000, 0.000, 1.000}
#define COBJ_COLOR_GREEN       (cobj_color_t){0.000, 1.000, 0.000, 1.000}
#define COBJ_COLOR_BLUE        (cobj_color_t){0.000, 0.000, 1.000, 1.000}
#define COBJ_COLOR_YELLOW      (cobj_color_t){1.000, 1.000, 0.000, 1.000}
#define COBJ_COLOR_MAGENTA     (cobj_color_t){1.000, 0.000, 1.000, 1.000}
#define COBJ_COLOR_CYAN        (cobj_color_t){0.000, 1.000, 1.000, 1.000}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Represention of a RGBA color. Double types bound between 0.0 and 1.0 values are used to pass them to
 * cairo's functions without needing to convert them. 
 *
 * @param r Red   color component
 * @param g Green color component
 * @param b Blue  color component
 * @param a Alpha color component
 */
struct cobj_color_t
{
	double r;
	double g;
	double b;
	double a;
};

typedef struct cobj_color_t cobj_color_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Converts a 32-bits ARGB color representation into a color object.
 *
 * @param argb Color uint to convert
 *
 * @return Color object
 */
cobj_color_t cobj_color_convert_argb_uint(uint32_t argb);

/**
 * Converts a RGBA color representation with channel values bounded between 0 and 255 into a color object.
 *
 * @param r Red   color component
 * @param g Green color component
 * @param b Blue  color component
 * @param a Alpha color component
 *
 * @return Color object
 */
cobj_color_t cobj_color_convert_rgba(uint8_t r, uint8_t g, uint8_t b, uint8_t a);

/**
 * Converts an RGBA HEX string into a color object. A valid color string should contain either 6 (rrggbb) or
 * 8 (rrggbbaa) valid hex characters. If the alpha parameter is ommited, a default value of 0xFF is assumed.
 *
 * @param str Source string to convert
 * @param err Optional, set to false if the string was converted with no issue, true otherwhise
 *
 * @return Color object. Check *err to be certain of the return's validity
 */
cobj_color_t cobj_color_convert_str(const char *str, bool *err);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Interpolates a color between two given colors.
 *
 * @param color_1 First  color
 * @param color_2 Second color
 * @param ratio Second / first color ratio used for the interpolation. Values are bounded between 0.0 and 1.0
 *
 * @return : interpolated color
 */
cobj_color_t cobj_color_interpolate(cobj_color_t color_1, cobj_color_t color_2, double ratio);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/*
 * Converts a given color object to its equivalent ARGB representation within a single 32-bit unsigned int.
 * Useful when using colors directly with XCB.
 *
 * @param color Color object to convert
 *
 * @return 32-bit argb color value
 */
uint32_t cobj_color_get_argb_uint(cobj_color_t color);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

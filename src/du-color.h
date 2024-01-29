/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Derelict Utilities (DU) library.
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

#ifndef DU_COLOR_H
#define DU_COLOR_H

#include <stdbool.h>
#include <stdint.h>

#include "du-types.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define DU_COLOR_TRANSPARENT (du_color_t){0.000, 0.000, 0.000, 0.000}
#define DU_COLOR_WHITE       (du_color_t){1.000, 1.000, 1.000, 1.000}
#define DU_COLOR_BLACK       (du_color_t){0.000, 0.000, 0.000, 1.000}
#define DU_COLOR_RED         (du_color_t){1.000, 0.000, 0.000, 1.000}
#define DU_COLOR_GREEN       (du_color_t){0.000, 1.000, 0.000, 1.000}
#define DU_COLOR_BLUE        (du_color_t){0.000, 0.000, 1.000, 1.000}
#define DU_COLOR_YELLOW      (du_color_t){1.000, 1.000, 0.000, 1.000}
#define DU_COLOR_MAGENTA     (du_color_t){1.000, 0.000, 1.000, 1.000}
#define DU_COLOR_CYAN        (du_color_t){0.000, 1.000, 1.000, 1.000}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Represention of a RGBA color. Ratio type values are used to pass them to cairo's functions without needing
 * to convert them. 
 *
 * @param r : red   color component
 * @param g : green color component
 * @param b : blue  color component
 * @param a : alpha color component
 */
typedef struct {
	du_ratio_t r;
	du_ratio_t g;
	du_ratio_t b;
	du_ratio_t a;
} du_color_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Gets the rgba values in a color struct by converting a string.
 * The string format should be one the following hexadecimal sequences, with the alpha paramter being
 * optional :
 * 	#rrggbbaa
 * 	#rrggbb
 * 	rrggbbaa
 * 	rrggbb
 *
 * @param str : source string to convert
 * @param err : optional, set to true if the string was converted with no issue, false otherwhise
 *
 * @return : a filled color struct, if no alpha was provided in the string, .a is set to 1.0. In case of 
 *           a formating error all fields of the returned color are set to 0.0, check *err to be certain of
 *           the return's validity
 */
du_color_t du_color_from_str(const char *str, bool *err);

/**
 * Interpolates a color between two given colors.
 *
 * @param cl_1  : first  color
 * @param cl_2  : second color
 * @param ratio : second/first color ratio used for the interpolation. Values are bounded between 0.0 and 1.0
 *
 * @return : interpolated color
 */
du_color_t du_color_interpolate(du_color_t cl_1, du_color_t cl_2, double ratio);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/*
 * Converts a given color struct to its equivalent ARGB representation within a single 32-bit unsigned int.
 * Useful when using colors directly with XCB.
 *
 * @param cl : color struct to convert
 *
 * @return : 32-bit argb color value
 */
uint32_t du_color_to_argb_uint(du_color_t cl);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DU_COLOR_H */

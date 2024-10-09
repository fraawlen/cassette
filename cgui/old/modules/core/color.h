/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Derelict Graphics (DG) GUI library.
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

#ifndef DG_CORE_COLOR_H
#define DG_CORE_COLOR_H

#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define DG_CORE_COLOR_TRANSPARENT (dg_core_color_t){0.000, 0.000, 0.000, 0.000}
#define DG_CORE_COLOR_WHITE       (dg_core_color_t){1.000, 1.000, 1.000, 1.000}
#define DG_CORE_COLOR_BLACK       (dg_core_color_t){0.000, 0.000, 0.000, 1.000}
#define DG_CORE_COLOR_RED         (dg_core_color_t){1.000, 0.000, 0.000, 1.000}
#define DG_CORE_COLOR_GREEN       (dg_core_color_t){0.000, 1.000, 0.000, 1.000}
#define DG_CORE_COLOR_BLUE        (dg_core_color_t){0.000, 0.000, 1.000, 1.000}
#define DG_CORE_COLOR_YELLOW      (dg_core_color_t){1.000, 1.000, 0.000, 1.000}
#define DG_CORE_COLOR_MAGENTA     (dg_core_color_t){1.000, 0.000, 1.000, 1.000}
#define DG_CORE_COLOR_CYAN        (dg_core_color_t){0.000, 1.000, 1.000, 1.000}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Represention of a RGBA color. Double type values are used to pass them to cairo's functions without needing
 * to convert them. Said values are bound between 0.0 and 1.0.
 *
 * @param r : red   color component
 * @param g : green color component
 * @param b : blue  color component
 * @param a : alpha color component
 */
typedef struct {
	double r;
	double g;
	double b;
	double a;
} dg_core_color_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Interpolates a color between two given colors.
 *
 * @param cl_1  : first  color
 * @param cl_2  : second color
 * @param ratio : second/first color ratio used for the interpolation. Values are bounded between 0.0 and 1.0
 *
 * @return : self-explanatory
 */
dg_core_color_t dg_core_color_interpolate(dg_core_color_t cl_1, dg_core_color_t cl_2, double ratio);

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
dg_core_color_t dg_core_color_from_str(const char *str, bool *err);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DG_CORE_COLOR_H */

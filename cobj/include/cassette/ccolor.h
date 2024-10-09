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

#if __GNUC__ > 4
	#define CCOLOR_NONNULL(...) __attribute__((nonnull (__VA_ARGS__)))
	#define CCOLOR_PURE         __attribute__((pure))
	#define CCOLOR_CONST        __attribute__((const))
#else
	#define CCOLOR_NONNULL(...)
	#define CCOLOR_PURE
	#define CCOLOR_CONST
#endif

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

/**
 * Represention of a RGBA color. Double types bound inside the [0.0 and 1.0] range are used, so that they
 * could be passed to cairo's function without conversion.
 *
 * @param r Red   color component
 * @param g Green color component
 * @param b Blue  color component
 * @param a Alpha color component
 */
struct ccolor
{
	double r;
	double g;
	double b;
	double a;
};

/************************************************************************************************************/
/* GLOBALS **************************************************************************************************/
/************************************************************************************************************/

/**
 * Basic colors.
 */
#define CCOLOR_TRANSPARENT (struct ccolor){0.000, 0.000, 0.000, 0.000}
#define CCOLOR_WHITE       (struct ccolor){1.000, 1.000, 1.000, 1.000}
#define CCOLOR_BLACK       (struct ccolor){0.000, 0.000, 0.000, 1.000}
#define CCOLOR_RED         (struct ccolor){1.000, 0.000, 0.000, 1.000}
#define CCOLOR_GREEN       (struct ccolor){0.000, 1.000, 0.000, 1.000}
#define CCOLOR_BLUE        (struct ccolor){0.000, 0.000, 1.000, 1.000}
#define CCOLOR_YELLOW      (struct ccolor){1.000, 1.000, 0.000, 1.000}
#define CCOLOR_MAGENTA     (struct ccolor){1.000, 0.000, 1.000, 1.000}
#define CCOLOR_CYAN        (struct ccolor){0.000, 1.000, 1.000, 1.000}

/************************************************************************************************************/
/* PURE METHODS *********************************************************************************************/
/************************************************************************************************************/

/**
 * Converts a 32-bits ARGB color representation into a color object.
 *
 * @param argb : Color uint to convert
 *
 * @return : Color object
 */
struct ccolor
ccolor_from_argb_uint(uint32_t argb)
CCOLOR_CONST;

/**
 * Converts a RGBA color representation with channel values bounded between 0 and 255 into a color object.
 *
 * @param r : Red   color component
 * @param g : Green color component
 * @param b : Blue  color component
 * @param a : Alpha color component
 *
 * @return : Color object
 */
struct ccolor
ccolor_from_rgba(uint8_t r, uint8_t g, uint8_t b, uint8_t a)
CCOLOR_CONST;

/**
 * Converts a C string into a color object. The given string is interpreted as a char representation of an
 * unsigned 32-bit ARGB value (which will get converted with stroul(); see the documentation of this function
 * for more information), unless there is a leading '#' character, in which case the string gets interpreted
 * as a 6-8 digit "#rrggbbaa" hex. In this representation, if the optional alpha parameter is omitted, a 0xFF
 * value is assumed. If the err parameter is provided, this function will set it to true if the string to
 * convert is invalid. Otherwise, it's set to false.
 *
 * @param str : Source string to convert
 * @param err : Optional, conversion error check
 *
 * @return : Color object. Check *err to be certain of the return's validity
 */
struct ccolor
ccolor_from_str(const char *str, bool *err)
CCOLOR_NONNULL(1);

/**
 * Interpolates a color between two given colors.
 *
 * @param color_1 : First  color
 * @param color_2 : Second color
 * @param ratio   : Second / first color ratio in the [0.0 1.0] range used for the interpolation.
 *
 * @return : interpolated color
 */
struct ccolor
ccolor_interpolate(struct ccolor color_1, struct ccolor color_2, double ratio)
CCOLOR_CONST;

/*
 * Converts a given color object to its equivalent ARGB representation within a single 32-bit unsigned int.
 * Useful when using colors directly with XCB.
 *
 * @param color : Color object to convert
 *
 * @return : 32-bit argb color value
 */
uint32_t
ccolor_to_argb_uint(struct ccolor color)
CCOLOR_CONST;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

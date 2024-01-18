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

#ifndef DG_BASE_STRING_H
#define DG_BASE_STRING_H

#include <stdbool.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define DG_BASE_STRING_EMPTY (dg_base_string_t){.chars = NULL, .n_rows = 0, .n_cols = 0, .n_chars = 0, \
                                                .n_codepoints = 0}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Wrapper struct holding 2D data about an UTF8 string.
 *
 * @param chars        : raw C string array
 * @param n_rows       : number of UTF8 chars rows taken by the string
 * @param n_cols       : number of UTF8 chars columns taken by the string
 * @param n_chars      : number of raw bytes taken by the string
 * @param n_codepoints : number of UTF8 chars / codepoints taken by the string
 */
typedef struct {
	char *chars;
	size_t n_rows;
	size_t n_cols;
	size_t n_chars;
	size_t n_codepoints;
} dg_base_string_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Adds a C string to the end of a given DG string
 * The string's geometry and length will be automatically recalculated.
 *
 * @param str     : DG string to modify
 * @param str_raw : raw C string to append
 *
 * @error DG_CORE_ERRNO_MEMORY : memory allocation failed
 */
void dg_base_string_append(dg_base_string_t *str, const char *str_raw);

/**
 * Frees memory and zeros all paramaters of a given DG string.
 *
 * @param str : DG string to clear
 */
void dg_base_string_clear(dg_base_string_t *str);

/**
 * Convenience function wrapper around dg_base_string_pad_custom() (see this function for more details) that
 * pads the given DG string with the module's standard padding character '_'.
 *
 * @param str        : string pointer to pad
 * @param pad_n      : maximum lenght of the string to be padded
 * @param align_left : align the padded string to the left (or right if false)
 *
 * @error DG_CORE_ERRNO_MEMORY : inherited from dg_base_utd8_pad_custom()
 */
void dg_base_string_pad(dg_base_string_t *str, size_t pad_n, bool align_left);

/**
 * Pads a given DG string. If said string is shorter than pad_n, the padder C string will be appended or
 * prepended (depends on the align_left parameter) until the pad_n length is reached. If the given DG string
 * is already longer, nothing happens. It only takes in account the DG string's n_codepoint length, and not
 * the other n_* parameters.
 * The string's geometry and length will be automatically recalculated.
 *
 * @param str        : DG string to pad
 * @param padder     : C string to use as padding
 * @param pad_n      : Codepoint length to reach
 * @param align_left : align the padded string to the left (or right if false)
 *
 * @error DG_CORE_ERRNO_MEMORY : memory allocation failed
 */
void dg_base_string_pad_custom(dg_base_string_t *str, const char *padder, size_t pad_n, bool align_left);

/**
 * Adds a C string to the begining of a given DG string
 * The string's geometry and length will be automatically recalculated.
 *
 * @param str     : DG string to modify
 * @param str_raw : raw C string to prepend
 *
 * @error DG_CORE_ERRNO_MEMORY : memory allocation failed
 */
void dg_base_string_prepend(dg_base_string_t *str, const char *str_raw);

/**
 * Recalculates a DG string's geometry and size.
 *
 * @param str : DG string to use
 */
void dg_base_string_recalculate_n_values(dg_base_string_t *str);

/**
 * Sets the contents of a DG string.
 * The string's geometry and length will be automatically recalculated.
 *
 * @param str     : DG string to modify
 * @param str_raw : raw C string to set as content
 *
 * @error DG_CORE_ERRNO_MEMORY : memory allocation failed
 */
void dg_base_string_set(dg_base_string_t *str, const char *str_raw);

/**
 * Wraps the contents of a given DG string after a given UTF8 character column limit.
 * The string's geometry and length will be automatically recalculated.
 *
 * @param str : DG string to modify
 * @param cw  : column width limit
 *
 * @error DG_CORE_ERRNO_MEMORY : memory allocation failed
 */
void dg_base_string_wrap(dg_base_string_t *str, size_t cw);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Allocates enough memory and converts a double into a DG string with the given decimal precision.
 * Freeing the returned string with dg_base_string_clear() is the responsibility of the caller.
 *
 * @param d         : double to convert
 * @param precision : amount of decimal to include, it follows printf's "%.*f" rules.
 *
 * @return : self-explanatory
 *
 * @error DG_CORE_ERRNO_MEMORY : memory allocation failed
 */
dg_base_string_t dg_base_string_convert_double(double d, int precision);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Returns the amount of row a given DG string will be after a potential wrapping operation. The string is not
 * actually wrapped.
 *
 * @param str : DG string to test
 * @param cw  : column width limit
 *
 * @return : amount of rows
 */
size_t dg_base_string_test_wrap(const dg_base_string_t *str, size_t cw);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DG_BASE_STRING_H */

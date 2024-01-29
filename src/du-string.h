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


#ifndef DU_STRING_H
#define DU_STRING_H

#include <stdbool.h>
#include <stdlib.h>

#include "du-status.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define DU_STRING_EMPTY {.chars = NULL, .n_rows = 0, .n_cols = 0, .n_chars = 0, .n_codepoints = 0, \
                         .status = DU_STATUS_SUCCESS}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Wrapper struct for storing a multi-row UTF8 string.
 * If status is set to DU_STATUS_FAILURE all handler functions will have no effect with the exception of
 * du_string_clear().
 *
 * @param chars        : raw C string array
 * @param n_rows       : number of UTF8 chars rows taken by the string
 * @param n_cols       : number of UTF8 chars columns taken by the string
 * @param n_chars      : number of raw bytes taken by the string
 * @param n_codepoints : number of UTF8 chars / codepoints taken by the string
 * @param status       : error state
 */
typedef struct {
	char *chars;
	size_t n_rows;
	size_t n_cols;
	size_t n_chars;
	size_t n_codepoints;
	du_status_t status;
} du_string_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Adds a C string to the end of a given DU string. The given C string is copied into the DU string.
 * The string's geometry and length will be automatically recalculated.
 * In case of error, str->status will be set to DU_STATUS_FAILURE.
 *
 * @param str   : DU string to modify
 * @param c_str : raw C string to append
 */
void du_string_append(du_string_t *str, const char *c_str);

/**
 * Frees memory and zeros all paramaters of a given DU string. stk->status will also be set to
 * DU_STATUS_SUCCESS.
 *
 * @param str : DU string to clear
 */
void du_string_clear(du_string_t *str);

/**
 * Pads a given DU string. If said string is shorter than pad_n, the padder C string will be appended or
 * prepended (depends on the align_left parameter) until the pad_n length is reached. If the given DU string
 * is already longer, nothing happens. It only takes in account the DU string's n_codepoint length, and not
 * the other n_* parameters.
 * The string's geometry and length will be automatically recalculated.
 * In case of error, str->status will be set to DU_STATUS_FAILURE.
 *
 * @param str        : DU string to pad
 * @param padder     : C string to use as padding
 * @param pad_n      : Codepoint length to reach
 * @param align_left : align the padded string to the left (or right if false)
 */
void du_string_pad(du_string_t *str, const char *padder, size_t pad_n, bool align_left);

/**
 * Adds a C string to the begining of a given DU string. The given C string is copied into the DU string.
 * The string's geometry and length will be automatically recalculated.
 * In case of error, str->status will be set to DU_STATUS_FAILURE.
 *
 * @param str   : DU string to modify
 * @param c_str : raw C string to prepend
 */
void du_string_prepend(du_string_t *str, const char *c_str);

/**
 * Recalculates a DU string's geometry and size.
 *
 * @param str : DU string to use
 */
void du_string_recalculate_n_values(du_string_t *str);

/**
 * Sets the contents of a DU string. The given C string is copied into the DU string.
 * The string's geometry and length will be automatically recalculated.
 * In case of error, str->status will be set to DU_STATUS_FAILURE.
 *
 * @param str   : DU string to modify
 * @param c_str : raw C string to set as content
 */
void du_string_set(du_string_t *str, const char *c_str);

/**
 * Wraps the contents of a given DU string after a given UTF8 character column limit.
 * The string's geometry and length will be automatically recalculated.
 * In case of error, str->status will be set to DU_STATUS_FAILURE.
 *
 * @param str      : DU string to modify
 * @param max_cols : column width limit
 */
void du_string_wrap(du_string_t *str, size_t max_cols);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Return a copy of the given DU string.
 * Freeing the returned string with du_string_clear() is the responsibility of the caller.
 * In case of error, str->status of the returned string will be set to DU_STATUS_FAILURE.
 *
 * @param str : DU string to duplicate, NULL value is not allowed.
 *
 * @return : duplicated DU string
 */
du_string_t du_string_duplicate(const du_string_t *str);

/**
 * Converts a double into a DU string with the given decimal precision.
 * Freeing the returned string with du_string_clear() is the responsibility of the caller.
 * In case of error, str->status of the returned string will be set to DU_STATUS_FAILURE.
 *
 * @param d         : double to convert
 * @param precision : amount of decimal to include, it follows printf's "%.*f" rules.
 *
 * @return : self-explanatory
 */
du_string_t du_string_from_double(double d, int precision);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Returns the amount of row a given DU string will be after a potential wrapping operation. The string is
 * however not actually wrapped or modified.
 *
 * @param str      : DU string to test
 * @param max_cols : column width limit
 *
 * @return : amount of rows
 */
size_t du_string_test_wrap(const du_string_t *str, size_t max_cols);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DU_STRING_H */

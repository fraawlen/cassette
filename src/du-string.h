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

/**
 * Wrapper struct for storing a multi-row UTF8 string.
 * If status is not set to DU_STATUS_SUCCESS all handler functions will have no effect with the exception of
 * du_string_reset(), du_string_init() and du_string_duplicate().
 *
 * @param chars        : raw C string array
 * @param n_rows       : number of UTF8 chars rows taken by the string
 * @param n_cols       : number of UTF8 chars columns taken by the string
 * @param n_chars      : number of bytes taken by the string
 * @param n_codepoints : number of UTF8 codepoints taken by the string
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
 * Initialises a DU string and copies into it the given C string. This function is similar to
 * du_string_replace(). But unlike du_string_replace(), this function does not free the str->chars value
 * before copying the given string into it.
 * The string's geometry and length will be automatically calculated.
 * In case of error, str->status will be set to DU_STATUS_FAILURE. It's set to DU_STATUS_SUCCESS otherwhise.
 *
 * @param str   : DU string to modify
 * @param c_str : raw C string to set as content, passing NULL is the same as passing ""
 */
void du_string_init(du_string_t *str, const char *c_str);

/**
 * Frees memory and zeros all paramaters of a given DU string then puts it in a uninitialised state by setting
 * stk->status to DU_STATUS_NOT_INIT.
 *
 * @param str : DU string to clear
 */
void du_string_reset(du_string_t *str);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Adds a C string to the end of a given DU string. The given C string is copied into the DU string.
 * The string's geometry and length will be automatically recalculated.
 * In case of error, str->status will be set to DU_STATUS_FAILURE.
 * The given structure needs to be initialised beforehand.
 *
 * @param str   : DU string to modify
 * @param c_str : raw C string to append, passing NULL is the same as passing ""
 */
void du_string_append(du_string_t *str, const char *c_str);

/**
 * Pads a given DU string. If the given string is shorter than pad_n, the padder C string will be appended or
 * prepended (depends on the align_left parameter) until the pad_n length is reached. If the given DU string
 * is already longer, nothing happens. It only takes in account the DU string's n_codepoint length, and not
 * the other n_* parameters.
 * The string's geometry and length will be automatically recalculated.
 * In case of error, str->status will be set to DU_STATUS_FAILURE.
 * The given structure needs to be initialised beforehand.
 *
 * @param str        : DU string to pad
 * @param padder     : C sub-string to use as padding, NULL value is not allowed
 * @param pad_n      : Codepoint length to reach
 * @param align_left : align the padded string to the left (or right if false)
 */
void du_string_pad(du_string_t *str, const char *padder, size_t pad_n, bool align_left);

/**
 * Adds a C string to the beginning of a given DU string. The given C string is copied into the DU string.
 * The string's geometry and length will be automatically recalculated.
 * In case of error, str->status will be set to DU_STATUS_FAILURE.
 * The given structure needs to be initialised beforehand.
 *
 * @param str   : DU string to modify
 * @param c_str : raw C string to prepend, passing NULL is the same as passing ""
 */
void du_string_prepend(du_string_t *str, const char *c_str);

/**
 * Recalculates a DU string's geometry and size.
 * The given structure needs to be initialised beforehand.
 *
 * @param str : DU string to use
 */
void du_string_recalculate_n_values(du_string_t *str);

/**
 * Replaces the contents of a DU string. The old str-chars value gets freed then the given C string is copied
 * into the DU string. This function is similar to du_string_init(). But unlike du_string_init() this function
 * expects the given DU string to be already initialised.
 * The string's geometry and length will be automatically recalculated.
 * In case of error, str->status will be set to DU_STATUS_FAILURE.
 * The given structure needs to be initialised beforehand.
 *
 * @param str   : DU string to modify
 * @param c_str : raw C string to set as content, passing NULL is the same as passing ""
 */
void du_string_replace(du_string_t *str, const char *c_str);

/**
 * Wraps the contents of a given DU string after a given UTF8 character column limit.
 * The string's geometry and length will be automatically recalculated.
 * In case of error, str->status will be set to DU_STATUS_FAILURE.
 * The given structure needs to be initialised beforehand.
 *
 * @param str      : DU string to modify
 * @param max_cols : column width limit, value 0 is not allowed
 */
void du_string_wrap(du_string_t *str, size_t max_cols);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Return a copy of the given DU string.
 * Freeing the returned string with du_string_reset() is the responsibility of the caller.
 * In case of error, str->status of the returned string will be set to DU_STATUS_FAILURE.
 * The given structure needs to be initialised beforehand.
 *
 * @param str : DU string to duplicate, NULL value is not allowed.
 *
 * @return : duplicated DU string
 */
du_string_t du_string_duplicate(const du_string_t *str);

/**
 * Converts a double into a DU string with the given decimal precision.
 * Freeing the returned string with du_string_reset() is the responsibility of the caller.
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
 * Returns the amount of row a given DU string will take after a potential wrapping operation. However, the
 * string is not actually wrapped or modified.
 * The given structure needs to be initialised beforehand.
 *
 * @param str      : DU string to test
 * @param max_cols : column width limit, value 0 is not allowed
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

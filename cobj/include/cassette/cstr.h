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
#include <stdlib.h>

#include "cerr.h"

#if __GNUC__ > 4
	#define CSTR_NONNULL_RETURN __attribute__((returns_nonnull))
	#define CSTR_NONNULL(...)   __attribute__((nonnull (__VA_ARGS__)))
	#define CSTR_PURE           __attribute__((pure))
	#define CSTR_CONST          __attribute__((const))
#else
	#define CSTR_NONNULL_RETURN
	#define CSTR_NONNULL(...)
	#define CSTR_PURE
	#define CSTR_CONST
#endif

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

/**
 * Opaque string object. Theses strings have the particularity of storing not just the string char array along
 * with its size, but they also keep track of the number of UTF-8 multi-byte characters, the number of rows 
 * and columns. It's assumed that all UTF-8 characters are 1 column wide. But it does not differencies between
 * printable or non printable characters. The only exception to this width rule are tab characters, whose
 * width can be customized with cstr_set_tab_width().
 *
 * Some methods, upon failure, will set an error that can be checked with cstr_error(). If any error is set
 * all string methods will exit early with default return values and no side-effects. It's possible to clear
 * errors with cstr_repair().
 */
typedef struct cstr cstr;

/************************************************************************************************************/
/* GLOBALS **************************************************************************************************/
/************************************************************************************************************/

/**
 * A macro that gives uninitialized string objects a non-NULL value that is safe to use with the string's
 * related functions. However, any function called with a handle set to this value will return early and
 * without any side effects.
 */
#define CSTR_PLACEHOLDER (&cstr_placeholder_instance)

/**
 * Global string object instance with the error state set to CERR_INVALID. This instance is only made
 * available to allow the static initialization of string object pointers with the macro CSTR_PLACEHOLDER.
 */
extern cstr cstr_placeholder_instance;

/************************************************************************************************************/
/* CONSTRUCTORS / DESTRUCTORS *******************************************************************************/
/************************************************************************************************************/

/**
 * Create a string instance and deep copy the contents of another string instance into it.
 *
 * @param str : String to copy contents from
 *
 * @return     : New string instance
 * @return_err : CSTR_PLACEHOLDER
 */
cstr *
cstr_clone(const cstr *str)
CSTR_NONNULL_RETURN
CSTR_NONNULL(1);

/**
 * Creates an empty string instance.
 *
 * @return     : New string instance
 * @return_err : CSTR_PLACEHOLDER
 */
cstr *
cstr_create(void)
CSTR_NONNULL_RETURN;

/**
 * Destroys the given string and frees memory.
 *
 * @param str : String to interact with
 */
void
cstr_destroy(cstr *str)
CSTR_NONNULL(1);

/************************************************************************************************************/
/* IMPURE METHODS *******************************************************************************************/
/************************************************************************************************************/

/**
 * Convenience generic wrapper to insert new data at the end of a string.
 */
#define cstr_append(DST, SRC) \
	_Generic (SRC, \
		cstr *       : cstr_insert_cstr,   \
		char *       : cstr_insert_raw,    \
		const char * : cstr_insert_raw,    \
		float        : cstr_insert_double, \
		double       : cstr_insert_double, \
		default      : cstr_insert_long    \
	)(DST, SRC, SIZE_MAX)

/**
 * Convenience generic wrapper to insert new data at a specific UTF-8 character offset.
 */
#define cstr_insert(DST, SRC, OFFSET) \
	_Generic (SRC, \
		cstr *       : cstr_insert_cstr,   \
		char *       : cstr_insert_raw,    \
		const char * : cstr_insert_raw,    \
		float        : cstr_insert_double, \
		double       : cstr_insert_double, \
		default      : cstr_insert_long    \
	)(DST, SRC, OFFSET)

/**
 * Convenience generic wrapper to insert new data at the beginning of a string.
 */
#define cstr_prepend(DST, SRC) \
	_Generic (SRC, \
		cstr *       : cstr_insert_cstr,   \
		char *       : cstr_insert_raw,    \
		const char * : cstr_insert_raw,    \
		float        : cstr_insert_double, \
		double       : cstr_insert_double, \
		default      : cstr_insert_long    \
	)(DST, SRC, 0)

/**
 * Clears the contents of a given string. Allocated memory is not freed, use cstr_destroy() for that.
 *
 * @param str : String to interact with
 */
void
cstr_clear(cstr *str)
CSTR_NONNULL(1);

/**
 * Removes a set number of UTF-8 characters at a specific offset.
 * This function is bounds-protected, meaning that offset + length parameters will be capped at the string's
 * length, even if a SIZE_MAX value is supplied.
 *
 * @param str    : String to interact with
 * @param offset : UTF-8 character position to start cutting from
 * @param length : number of UTF-8 characters to remove
 */
void
cstr_cut(cstr *str, size_t offset, size_t length)
CSTR_NONNULL(1);

/**
 * Insert the contents of str_src at a specific offset.
 * The string's allocated memory will be automatically extended if needed to accommodate the inserted data.
 * This function comes with overlap detection, so a raw_str obtained from cstr_char*() can be used. This
 * function is bounds-protected, so the offset parameter is capped at the string's length, even if a SIZE_MAX
 * value is supplied.
 *
 * @param str     : String to insert new data to
 * @param str_src : String to ger new data from
 * @param offset  : UTF-8 character position to insert the new data at
 *
 * @error CERR_OVERFLOW : The size of the resulting string will be > SIZE_MAX
 * @error CERR_MEMORY   : Failed memory allocation
 */
void
cstr_insert_cstr(cstr *str, const cstr *str_src, size_t offset)
CSTR_NONNULL(1, 2);

/**
 * Converts a double into a character array then inserts it at a specific offset. The double digits number is
 * controlled with cstr_set_double_digits().
 * The string's allocated memory will be automatically extended if needed to accommodate the inserted data.
 * This function comes with overlap detection, so a raw_str obtained from cstr_char*() can be used. This
 * function is bounds-protected, so the offset parameter is capped at the string's length, even if a SIZE_MAX
 * value is supplied.
 *
 * @param str     : String to insert new data to
 * @param d       : Double value to insert
 * @param offset  : UTF-8 character position to insert the new data at
 *
 * @error CERR_OVERFLOW : The size of the resulting string will be > SIZE_MAX
 * @error CERR_MEMORY   : Failed memory allocation
 */
void
cstr_insert_double(cstr *str, double d, size_t offset)
CSTR_NONNULL(1);

/**
 * Converts a long into a character array then inserts it at a specific offset.
 * The string's allocated memory will be automatically extended if needed to accommodate the inserted data.
 * This function comes with overlap detection, so a raw_str obtained from cstr_char*() can be used. This
 * function is bounds-protected, so the offset parameter is capped at the string's length, even if a SIZE_MAX
 * value is supplied.
 *
 * @param str     : String to insert new data to
 * @param l       : Long value to insert
 * @param offset  : UTF-8 character position to insert the new data at
 *
 * @error CERR_OVERFLOW : The size of the resulting string will be > SIZE_MAX
 * @error CERR_MEMORY   : Failed memory allocation
 */
void
cstr_insert_long(cstr *str, long long l, size_t offset)
CSTR_NONNULL(1);

/**
 * Insert a raw C string at a specific offset.
 * The string's allocated memory will be automatically extended if needed to accommodate the inserted data.
 * This function comes with overlap detection, so a raw_str obtained from cstr_char*() can be used. This
 * function is bounds-protected, so the offset parameter is capped at the string's length, even if a SIZE_MAX
 * value is supplied.
 *
 * @param str     : String to insert new data to
 * @param raw_str : Raw C string to insert
 * @param offset  : UTF-8 character position to insert the new data at
 *
 * @error CERR_OVERFLOW : The size of the resulting string will be > SIZE_MAX
 * @error CERR_MEMORY   : Failed memory allocation
 */
void
cstr_insert_raw(cstr *str, const char *raw_str, size_t offset)
CSTR_NONNULL(1, 2);

/**
 * Pads a string with a repeated sequence of characters set by pattern until its length matches length_target.
 * This function has no effects if the string's length is bigger than the target length or if the given
 * pattern is empty. The sequence of padding characters will be inserted at the given offset. This function is
 * bounds-protected, so the offset parameter is capped at the string's length, even if a SIZE_MAX value is
 * supplied.
 *
 * Example :
 *
 *	cstr_clear(str);
 *	cstr_append(str, "test");
 *	cstr_pad(str, "_Ͳ", 1, 9);
 *	printf("%s\n", cstr_chars(str));
 *
 *	--> t_Ͳ_Ͳ_est
 *
 * @param str           : String to interact with
 * @param pattern       : UTF-8 character to use as padding
 * @param offset        : UTF-8 character position to insert the padded sequence at
 * @param length_target : Resulting string length that should be reached
 *
 * @error CERR_OVERFLOW : The size of the resulting string will be > SIZE_MAX
 * @error CERR_MEMORY   : Failed memory allocation
 */
void
cstr_pad(cstr *str, const char *pattern, size_t offset, size_t length_target)
CSTR_NONNULL(1, 2);

/** 
 * Preallocates a set number of bytes to avoid triggering multiple automatic reallocs when adding data to the
 * string. This function has no effect if the requested number of bytes is smaller than the previously
 * allocated number.
 *
 * @param str         : String to interact with
 * @param byte_length : Number of bytes
 *
 * @error CERR_MEMORY : Failed memory allocation
 */
void
cstr_prealloc(cstr *str, size_t byte_length)
CSTR_NONNULL(1);

/** 
 * Clears errors and puts the string back into an usable state. The only unrecoverable error is CSTR_INVALID.
 *
 * @param str : String to interact with
 */
void
cstr_repair(cstr *str)
CSTR_NONNULL(1);

/**
 * Sets the number of digits to show when a double value gets inserted. The effects of the int values are
 * limited by the printf's "%.*Lf" operator.
 *
 * @param str    : String to interact with
 * @param digits : Number of decimal digits
 */
void
cstr_set_precision(cstr *str, int precision)
CSTR_NONNULL(1);

/**
 * Sets the width of a '\t' character. This will affect the results of 2d functions like cstr_coords_offset(),
 * cstr_test_wrap(), cstr_width() and, cstr_wrap().
 *
 * @param str   : String to interact with
 * @param width : Tab width
 */
void
cstr_set_tab_width(cstr *str, size_t width)
CSTR_NONNULL(1);

/**
 * Slices out a set number of UTF-8 characters at a specific offset and discards the rest.
 *
 * @param str    : String to interact with
 * @param offset : UTF-8 character position to start slicing from
 * @param length : number of UTF-8 characters to slice out
 */
void
cstr_slice(cstr *str, size_t offset, size_t length)
CSTR_NONNULL(1);

/**
 * Removes extra leading and trailing whitespaces (space and tab characters).
 *
 * @param str : String to interact with
 */
void
cstr_trim(cstr *str)
CSTR_NONNULL(1);

/**
 * Wraps a string by adding newlines to rows that are longer than max_width. Old newlines are also kept.
 * This function has no effects if max_width is bigger than the string's width. A max_width of 0 is
 * illegal.
 *
 * @param str       : String to interact with
 * @param max_width : Width after which a newline is added to the string
 *
 * @error CERR_PARAM    : Invalid width = 0 was given
 * @error CERR_OVERFLOW : The size of the resulting string will be > SIZE_MAX
 * @error CERR_MEMORY   : Failed memory allocation
 */
void
cstr_wrap(cstr *str, size_t max_width)
CSTR_NONNULL(1);

/**
 * Similar to cstr_clear() but all of the allocated memory is also zeroed.
 *
 * @param str : String to interact with
 */
void
cstr_zero(cstr *str)
CSTR_NONNULL(1);

/************************************************************************************************************/
/* PURE METHODS *********************************************************************************************/
/************************************************************************************************************/

/**
 * Gets the string's length in bytes, including the NUL terminator.
 *
 * @param str : String to interact with
 *
 * @return     : Number of bytes
 * @return_err : 0
 */
size_t
cstr_byte_length(const cstr *str)
CSTR_NONNULL(1)
CSTR_PURE;

/**
 * Converts the given UTF-8 character offset into a byte offset.
 * This function is bounds-protected, so the offset parameter is capped at the string's length, even if
 * a SIZE_MAX value is supplied.
 *
 * @param str    : String to interact with
 * @param offset : UTF-8 character offset
 *
 * @return     : Converted offset in bytes
 * @return_err : 0
 */
size_t
cstr_byte_offset(const cstr *str, size_t offset)
CSTR_NONNULL(1)
CSTR_PURE;

/**
 * Gets the raw NUL terminated C string.
 *
 * @param str : String to interact with
 *
 * @return     : Raw C string
 * @return_err : "\0"
 */
const char *
cstr_chars(const cstr *str)
CSTR_NONNULL_RETURN
CSTR_NONNULL(1)
CSTR_PURE;

/**
 * Gets the raw NUL terminated C string offseted by 2d coordinates.
 * This function is bounds-protected, so the row and col parameter are capped at the string's height and
 * width respectively, even if SIZE_MAX values are supplied.
 *
 * @param str : String to interact with
 * @param row : Row index
 * @param col : Columns index
 *
 * @return     : Raw C string
 * @return_err : "\0"
 */
const char *
cstr_chars_at_coords(const cstr *str, size_t row, size_t col)
CSTR_NONNULL_RETURN
CSTR_NONNULL(1)
CSTR_PURE;

/**
 * Gets the raw NUL terminated C string offseted by an specific number of UTF-8 characters.
 * This function is bounds-protected, so the offset parameter is capped at the string's length, even if
 * a SIZE_MAX value is supplied.
 *
 * @param str    : String to interact with
 * @param offset : UTF-8 character offset
 *
 * @return     : Raw C string
 * @return_err : "\0"
 */
const char *
cstr_chars_at_offset(const cstr *str, size_t offset)
CSTR_NONNULL_RETURN
CSTR_NONNULL(1)
CSTR_PURE;

/**
 * Converts the given 2d coordinates into a UTF-8 character offset.
 * This function is bounds-protected, so the row and col parameter are capped at the string's height and
 * width respectively, even if SIZE_MAX values are supplied.
 *
 * @param str : String to interact with
 * @param row : Row index
 * @param col : Columns index
 *
 * @return     : Converted offset in number of UTF-8 characters
 * @return_err : 0
 */
size_t
cstr_coords_offset(const cstr *str, size_t row, size_t col)
CSTR_NONNULL(1)
CSTR_PURE;

/**
 * Gets the error state.
 *
 * @param str : String to interact with
 *
 * @return : Error value
 */
enum cerr
cstr_error(const cstr *str)
CSTR_NONNULL(1)
CSTR_PURE;

/**
 * Get the number of rows.
 *
 * @param str : String to interact with
 *
 * @return     : Number of rows
 * @return_err : 0
 */
size_t
cstr_height(const cstr *str)
CSTR_NONNULL(1)
CSTR_PURE;

/**
 * Gets the number of UTF-8 characters a string is made of. Unlike cstr_byte_length(), the NUL terminator is
 * not included.
 *
 * @param str : String to interact with
 *
 * @return     : Number of UTF-8 characters
 * @return_err : 0
 */
size_t
cstr_length(const cstr *str)
CSTR_NONNULL(1)
CSTR_PURE;

/**
 * Calculates the number of rows a string wrapped with max_width will have. But unlike cstr_wrap() the string
 * is not modified.
 *
 * @param str       : String to interact with
 * @param max_width : Width after which a newline is added to the string
 *
 * @return     : Number of rows
 * @return_err : 0
 */
size_t
cstr_test_wrap(const cstr *str, size_t max_width)
CSTR_NONNULL(1)
CSTR_PURE;

/**
 * Converts the UTF-8 character offset of a wrapped string into an offset that matches the character position
 * of the unwrapped string. It is assumed the difference between str_wrap and str is a single cstr_wrap()
 * operation and that the tab width of both strings is equal. Check out the provided example for more
 * details about this function use case.
 * This function is bounds-protected, so the offset parameter is capped at the string's length, even if
 * a SIZE_MAX value is supplied.
 *
 * @param str      : Reference string
 * @param str_wrap : Wrapped string
 * @param offset   : UTF-8 character offset
 *
 * @return     : Converted offset in number of UTF-8 characters
 * @return_err : 0
 */
size_t
cstr_unwrapped_offset(const cstr *str, const cstr *str_wrap, size_t offset)
CSTR_NONNULL(1, 2)
CSTR_PURE;

/**
 * Gets the number of columns. The NUL terminator and newline characters are not included.
 *
 * @param str : String to interact with
 *
 * @return     : Number of columns
 * @return_err : 0
 */
size_t
cstr_width(const cstr *str)
CSTR_NONNULL(1)
CSTR_PURE;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

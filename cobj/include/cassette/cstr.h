/**
 * Copyright © 2024-2025 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette library.
 *
 * This library is free software; you can redistribute it and/or modify it either under the terms of the GNU
 * Lesser General Public License as published by the Free Software Foundation; either version 3.0 of the
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
#include <stddef.h>
#include <stdlib.h>

#include "cerr.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Opaque UTF-8 string object implemented as a dynamically sized array. Cassette strings were
 * 	developped specifically to be used within GUI/TUI applications, but they can be used as a
 * 	convenient general purpose string library. They keep track of their byte length (including the
 * 	NUL terminator), number of Unicode codepoints, rows and columns. Thanks to these parameters,
 * 	it is possible to layout the string inside a 2D space without advanced text shaping libraries.
 * 	However, it assumes that monospaced single-width fonts are used to render the string.
 *
 * 	Currently only single codepoint unicode characters are supported, but support for Unicode
 * 	plane 0 graphemes is planned. Because only single-codepoint graphemes characters are supported
 * 	right now, the methods length and offset parameters will work with codepoint. These parameters
 * 	will use graphemes once support for them is implemented.
 * 	Support for variable tab widths is also planned.
 *
 * 	Some methods may fail and set an internal error, which can be checked using cstr_error().
 * 	If an error is set, all methods will exit early with default return values and no side
 * 	effects, leaving only the destruction function available.
 */
typedef struct cstr cstr;

/************************************************************************************************************/
/* CONSTRUCTORS / DESTRUCTORS *******************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Destroys a string and frees all associated memory.
 * 	Calling this function on a NULL string has no effect.
 *
 * [Parameters]
 *
 * 	str - String to destroy.
 *
 * [Returns]
 *
 * 	To prevent dangling pointers while keeping this function a one-liner, this function
 * 	conveniently returns nullptr.
 */
[[nodiscard]] nullptr_t cstr_destroy(cstr *str);

/**
 * [Description]
 *
 * 	Create a string instance and deep copy the contents of another string instance into it.
 *
 * [Parameters]
 *
 * 	str - String to copy.
 *
 * [Returns]
 *
 * 	On succes, a pointer to a newly allocated instance. Returns nullptr on failure.
 * 	If the string is NULL or in a critical error state, this function always returns nullptr.
 * 	The caller is responsible for freeing the returned instance using cstr_destroy().
 */
[[nodiscard]] [[gnu::malloc(cstr_destroy)]] cstr *cstr_clone(const cstr *str);

/**
 * [Description]
 *
 * 	Creates a new, empty string instance.
 *
 * [Returns]
 *
 * 	On succes, a pointer to a newly allocated instance. Returns nullptr on failure.
 * 	The caller is responsible for freeing the returned instance using cstr_destroy().
 */
[[nodiscard]] [[gnu::malloc(cstr_destroy)]] cstr *cstr_create(void);

/************************************************************************************************************/
/* IMPURE METHODS *******************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Convenience generic wrapper to insert new data at a specific codepoint offset.
 */
#define cstr_insert(DST, SRC, OFFSET) \
	_Generic (SRC, \
		cstr *       : cstr_insert_str,    \
		char *       : cstr_insert_bytes,  \
		const char * : cstr_insert_bytes,  \
		float        : cstr_insert_double, \
		double       : cstr_insert_double, \
		default      : cstr_insert_long    \
	)(DST, SRC, OFFSET)

/**
 * [Description]
 *
 * 	Convenience generic wrapper to insert new data at the end of a string.
 */
#define cstr_append(DST, SRC) cstr_insert(DST, SRC, SIZE_MAX)

/**
 * [Description]
 *
 * 	Convenience generic wrapper to insert new data at the beginning of a string.
 */
#define cstr_prepend(DST, SRC) cstr_insert(DST, SRC, 0)

/**
 * [Description]
 *
 * 	Clears the contents of a string.
 * 	Allocated memory is not freed, use cstr_destroy() for that.
 * 	Calling this function on a NULL string has no effect.
 *
 * [Parameters]
 *
 * 	str - String to modify.
 */
void cstr_clear(cstr *str);

/**
 * [Description]
 *
 * 	Clears any warning error the string may have. Does not clears criticial errors.
 * 	Calling this function on a NULL string has no effect.
 *
 * [Parameters]
 *
 * 	str - string to modify.
 */
void cstr_clear_warnings(cstr *str);

/**
 * [Description]
 *
 * 	Removes a set number of codepoints from a specific offset.
 * 	Calling this function on a NULL string has no effect.
 *
 * 	This function is bounds-protected. Offset + length will be capped at the string's length,
 * 	even if SIZE_MAX is passed.
 *
 * [Parameters]
 *
 * 	str    - String to modify.
 * 	offset - Codepoint index to start cutting from.
 * 	length - Number of codepoints to remove.
 */
void cstr_cut(cstr *str, size_t offset, size_t length);

/**
 * [Description]
 *
 * 	Insert a NUL terminated byte array at a specific offset.
 * 	The string will automatically grow if needed to accommodate the inserted data.
 * 	This function comes with memory overlap detection, raw_str obtained from str can be used.
 *
 * 	Calling this function on a NULL str or str_src has no effect.
 * 	This function is bounds-protected. Offset is capped at the string's length, even if
 * 	SIZE_MAX is passed.
 *
 * [Parameters]
 *
 * 	str    - String to modify.
 * 	bytes  - NUL terminated C string to get new data from.
 * 	offset - Codepoint index to insert the new data at.
 *
 * [Errors]
 *
 * 	CERR_OVERFLOW
 * 	CERR_MEMORY
 */
void cstr_insert_bytes(cstr *str, const char *bytes, size_t offset);

/**
 * [Description]
 *
 * 	Converts a double into a string then inserts it at a specific offset.
 * 	The number of digits can be set with cstr_set_double_digits().
 * 	The string will automatically grow if needed to accommodate the inserted data.
 *
 * 	Calling this function on a NULL string has no effect.
 * 	This function is bounds-protected. Offset is capped at the string's length, even if
 * 	SIZE_MAX is passed.
 *
 * [Parameters]
 *
 * 	str    - String to modify.
 * 	d      - Double value to insert.
 * 	offset - Codepoint index to insert the new data at.
 *
 * [Errors]
 *
 * 	CERR_OVERFLOW
 * 	CERR_MEMORY
 */
void cstr_insert_double(cstr *str, double d, size_t offset);

/**
 * [Description]
 *
 * 	Converts a long integer into a string then inserts it at a specific offset.
 * 	The number of digits can be set with cstr_set_double_digits().
 * 	The string will automatically grow if needed to accommodate the inserted data.
 *
 * 	Calling this function on a NULL string has no effect.
 * 	This function is bounds-protected. Offset is capped at the string's length, even if
 * 	SIZE_MAX is passed.
 *
 * [Parameters]
 *
 * 	str    - String to modify.
 * 	l      - Long value to insert.
 * 	offset - Codepoint index to insert the new data at.
 *
 * [Errors]
 *
 * 	CERR_OVERFLOW
 * 	CERR_MEMORY
 */
void cstr_insert_long(cstr *str, long long l, size_t offset);

/**
 * [Description]
 *
 * 	Insert the contents of str_src at a specific offset.
 * 	The string will automatically grow if needed to accommodate the inserted data.
 * 	This function comes with memory overlap detection, raw_str can be str.
 *
 * 	Calling this function on a NULL str or str_src has no effect.
 * 	This function is bounds-protected. Offset is capped at the string's length, even if
 * 	SIZE_MAX is passed.
 *
 * [Parameters]
 *
 * 	str     - String to modify.
 * 	str_src - String to get new data from.
 * 	offset  - Codepoint index to insert the new data at.
 *
 * [Errors]
 *
 * 	CERR_OVERFLOW
 * 	CERR_MEMORY
 */
void cstr_insert_str(cstr *str, const cstr *str_src, size_t offset);

/**
 * [Description]
 *
 * 	Pads a string with a repeated pattern of bytes until the string reaches a target length.
 * 	The pattern may end up truncated to exactly match the target length.
 * 	The string will automatically grow if needed to accommodate the inserted data.
 *
 * 	This function has no effect if the string is NULL, the pattern is NULL or empty, or if the
 * 	string initial length is bigger than the target length.
 * 	This function is bounds-protected. Offset is capped at the string's length, even if
 * 	SIZE_MAX is passed.
 *
 * [Example]
 *
 * 	cstr_clear(str);
 * 	cstr_append(str, "test");
 * 	cstr_pad(str, "_Ͳ", 1, 9);
 * 	printf("%s\n", cstr_chars(str));
 *
 * 	--> t_Ͳ_Ͳ_est
 *
 * [Parameters]
 *
 * 	str     - String to modify.
 * 	pattern - Byte sequence to use as padding.
 * 	offset  - Codepoint index to pad from.
 * 	length  - Target string length.
 *
 * [Errors]
 *
 * 	CERR_OVERFLOW
 * 	CERR_MEMORY
 */
void cstr_pad(cstr *str, const char *pattern, size_t offset, size_t length);

/** 
 * [Description]
 *
 * 	Preallocates a set number of bytes to prevent multiple automatic reallocations when
 * 	inserting new data.
 * 	This function has no effect is the string is NULL or if the requested number is smaller
 * 	than the previously allocated amount.
 *
 * [Parameters]
 *
 * 	str          - String to modify.
 * 	bytes_number - Number of bytes.
 *
 * [Errors]
 *
 * 	CERR_MEMORY
 */
void cstr_prealloc(cstr *str, size_t bytes_number);

/**
 * [Description]
 *
 * 	Sets the number of digits to keep when a double is converted into a string.
 * 	The effects of the digit value is limited by the printf's "%.*Lf" operator.
 * 	By default, a string has a precision of 0 (no digits).
 * 	Calling this function on a NULL string has no effect.
 *
 * [Parameters]
 *
 * 	str    - String to modify.
 * 	digits - Number of decimal digits.
 */
void cstr_set_precision(cstr *str, int digits);

/**
 * [Description]
 *
 * 	Slices out a set range of codepoints from a string and discard the rest.
 * 	Calling this function on a NULL string has no effect.
 *
 * [Parameters]
 *
 * 	str    - String to modify.
 * 	offset - Codepoint to start slicing from.
 * 	length - Number of codepoints to keep.
 */
void cstr_slice(cstr *str, size_t offset, size_t length);

/**
 * [Description]
 *
 * 	Removes extra leading and trailing whitespaces (space and tab characters).
 * 	Calling this function on a NULL string has no effect.
 *
 * [Parameters]
 *
 * 	str - String to modify.
 */
void cstr_trim(cstr *str);

/**
 * [Description]
 *
 * 	Wraps a string around a column limit. To do so, extra newlines are inserted. Rows shorter
 * 	than the passed width are not modified, and existing newlines are kept.
 * 	The string will automatically grow if needed to accommodate the inserted newlines.
 *
 * 	Calling this function on a NULL string has no effect.
 * 	A width of 0 is illegal.
 *
 * [Parameters]
 *
 * 	str   - String to modify.
 * 	width - Maximum width of the resulting string.
 *
 * [Errors]
 *
 * 	CERR_PARAM
 * 	CERR_OVERFLOW
 * 	CERR_MEMORY
 */
void cstr_wrap(cstr *str, size_t width);

/**
 * [Description]
 *
 * 	Clears the contents of a string and zeroes all of the allocated memory.
 * 	Allocated memory is not freed, use cstr_destroy() for that.
 * 	Calling this function on a NULL string has no effect.
 *
 * [Parameters]
 *
 * 	str - String to modify.
 */
void cstr_zero(cstr *str);

/************************************************************************************************************/
/* PURE METHODS *********************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Retrieves the string's byte length, including the NUL terminator.
 *
 * [Parameters]
 *
 * 	str - String to inspect
 *
 * [Returns]
 *
 * 	The number of bytes.
 * 	If the string is NULL or in a critical error state, this function always returns 0.
 */
[[gnu::pure]] size_t cstr_byte_length(const cstr *str);

/**
 * [Description]
 *
 * 	Converts a codepoint offset into a byte offset.
 * 	This function is bounds-protected. Offset is capped at the string's length, even if
 * 	SIZE_MAX is passed.
 *
 * [Parameters]
 *
 * 	str    - String to inspect.
 * 	offset - Codepoint index to convert.
 *
 * [Returns]
 *
 * 	The converted codepoint offset in bytes.
 * 	If the string is NULL or in a critical error state, this function always returns 0.
 */
[[gnu::pure]] size_t cstr_byte_offset(const cstr *str, size_t offset);

/**
 * [Description]
 *
 * 	Retrieves the raw NUL terminated C string.
 *
 * [Parameters]
 *
 * 	str - String to inspect.
 *
 * [Returns]
 *
 * 	A NUL terminated array of chars.
 * 	If the string is NULL or in a critical error state, this function always returns '\0'.
 * 	This function never returns nullptr.
 */
[[gnu::pure]] [[gnu::returns_nonnull]] const char *cstr_bytes(const cstr *str);

/**
 * [Description]
 *
 * 	Retrieves the raw NUL terminated C string offseted by 2D coordinates.
 * 	This function is bounds-protected. Row and col are capped at the string's height and width
 * 	even if SIZE_MAX is passed.
 *
 * [Parameters]
 *
 * 	str - String to inspect.
 * 	row - Row index.
 * 	col - Column index.
 *
 * [Returns]
 *
 * 	A NUL terminated array of chars.
 * 	If the string is NULL or in a critical error state, this function always returns '\0'.
 * 	This function never returns nullptr.
 */
[[gnu::pure]] [[gnu::returns_nonnull]] const char *cstr_bytes_at_coords(const cstr *str, size_t row, size_t col);

/**
 * [Description]
 *
 * 	Retrieves the raw NUL terminated C string offseted by a codepoint index.
 * 	This function is bounds-protected. Offset is capped at the string's length, even if
 * 	SIZE_MAX is passed.
 *
 * [Parameters]
 *
 * 	str    - String to inspect.
 * 	offset - Codepoint index.
 *
 * [Returns]
 *
 * 	A NUL terminated array of chars.
 * 	If the string is NULL or in a critical error state, this function always returns '\0'.
 * 	This function never returns nullptr.
 */
[[gnu::pure]] [[gnu::returns_nonnull]] const char *cstr_bytes_at_offset(const cstr *str, size_t offset);

/**
 * [Description]
 *
 * 	Converts 2D coordinates into a codepoint offset.
 * 	This function is bounds-protected. Row and col are capped at the string's height and width,
 * 	even if SIZE_MAX us passed.
 *
 * [Parameters]
 *
 * 	str - String to inspect.
 * 	row - Row index.
 * 	col - Columns index.
 *
 * [Returns]
 *
 * 	The converted 2D coordinates into a codepoint offset.
 * 	If the string is NULL or in a critical error state, this function always returns 0.
 */
[[gnu::pure]] size_t cstr_coords_offset(const cstr *str, size_t row, size_t col);

/**
 * [Description]
 *
 * 	Retrieves the string's current error state.
 *
 * [Parameters]
 *
 * 	str - String to inspect.
 *
 * [Returns]
 *
 * 	The current error code.
 * 	If the string is NULL, this function always returns CERR_INVALID.
 */
[[gnu::pure]] enum cerr cstr_error(const cstr *str);

/**
 * [Description]
 *
 * 	Retrieves the number of rows.
 * 	An empty string will still have a height of 1.
 *
 * [Parameters]
 *
 * 	str - String to inspect.
 *
 * [Returns]
 *
 * 	Total number of rows.
 * 	If the string is NULL or in a critical error state, this function always returns 0.
 */
[[gnu::pure]] size_t cstr_height(const cstr *str);

/**
 * [Description]
 *
 * 	Retrieves the length of the string.
 * 	The NUL terminator is not included.
 *
 * [Parameters]
 *
 * 	str - String to inspect.
 *
 * [Returns]
 *
 * 	Total number of codepoints.
 * 	If the string is NULL or in a critical error state, this function always returns 0.
 */
[[gnu::pure]] size_t cstr_length(const cstr *str);

/**
 * [Description]
 *
 * 	Retrieves width of a specific row.
 * 	The NUL terminator and newline characters are not included.
 * 	This function is bounds-protected. Row is capped at the string's height, even if
 * 	SIZE_MAX is passed.
 *
 * [Parameters]
 *
 * 	str - String to inspect.
 *
 * [Returns]
 *
 * 	Total number of columns.
 * 	If the string is NULL or in a critical error state, this function always returns 0.
 */
[[gnu::pure]] size_t cstr_row_width(const cstr *str, size_t row);

/**
 * [Description]
 *
 * 	Calculates the number of rows a string will have after a wrapping operation. But unlike
 * 	cstr_wrap(), the string is not modified, its geometry is not recalculated, and no memory
 * 	reallocations can happen.
 * 	This function is intended to be used before cstr_wrap() when displaying large amount of text
 * 	inside widgets, to check whether or not a scrollbar needs to be shown.
 *
 * [Parameters]
 *
 * 	str   - String to inspect.
 * 	width - Maximum width of the resulting string.
 *
 * [Returns]
 *
 * 	The resulting number of rows.
 * 	If the string is NULL, in a critical error state, or the illegal width value 0 is passed,
 * 	then this function always returns 0.
 */
[[gnu::pure]] size_t cstr_test_wrap(const cstr *str, size_t width);

/**
 * [Description]
 *
 * 	Converts a wrapped string codepoint offset into an equivalent unwrapped string codepoint
 * 	offset. It is assumed the difference between str_wrap and str is a single cstr_wrap()
 * 	operation. Check out the provided example for more details about this function's use case.
 * 	This function is bounds-protected. Offset is capped at str_wrap's length, even if
 * 	SIZE_MAX is passed.
 *
 * [Example]
 *
 * 	Here str1 is used to keep data in its original form. And str2 is the string displayed inside
 * 	a widget or terminal. The end user only see and interacts with str2. Any modifications to str2
 * 	(like data insertion) should be passed first to str1, and only then str2 can be updated.
 * 	cstr_unwrapped offset() helps with that.
 * 	In the second part, a "_" character is supposed to be insert into str1 at a position that
 * 	matches the 7th codepoint in str2 (this offset value includes newlines). After the new
 * 	character is inserted into str1, str2 is regenerated.
 *
 * 	cstr_clear(str1);
 * 	cstr_clear(str2);
 * 	cstr_append(str1, "1234567890");
 * 	cstr_append(str2, str1);
 * 	cstr_wrap(str2, 4);
 *
 * 	--> str1 = 1234567890
 * 	--> str2 = 1234
 * 	           5678
 * 	           90
 *
 * 	cstr_insert(str1, "_", cstr_unwrapped_offset(str1, str2, 7);
 * 	cstr_clear(str2);
 * 	cstr_append(str2, str1);
 * 	cstr_wrap(str2, 4);
 *
 * 	-->str1 = 12345_6890
 * 	-->str2 = 1234
 * 	          5_67
 * 	          890
 *
 * [Parameters]
 *
 * 	str      : Reference string.
 * 	str_wrap : Wrapped string.
 * 	offset   : Copedpoint index to convert.
 *
 * [Returns]
 *
 * 	The converted codepoint offset.
 * 	If the str or str_wrap are NULL or in a critical error state, this function always returns 0.
 */
[[gnu::pure]] size_t cstr_unwrapped_offset(const cstr *str, const cstr *str_wrap, size_t offset);

/**
 * [Description]
 *
 * 	Retrieves the number of columns.
 * 	The NUL terminator and newline are not included.
 *
 * [Parameters]
 *
 * 	str - String to inspect.
 *
 * [Returns]
 *
 * 	Total number of columns.
 * 	If the string is NULL or in a critical error state, this function always returns 0.
 */
[[gnu::pure]] size_t cstr_width(const cstr *str);

/************************************************************************************************************/
/* EXTRAS ***************************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Traverses a NUL terminated byte array until the start of the next codepoint.
 * 	This function EXPECTS a non-NULL byte pointer, it's undefined behavior otherwhise.
 *
 * [Parameters]
 *
 * 	byte - Starting point.
 *
 * [Returns]
 *
 * 	Pointer to the start of the next codepoint.
 * 	This function never returns nullptr.
 */
[[gnu::pure]] [[gnu::nonnull(1)]] [[gnu::returns_nonnull]] const char *cstr_next_codepoint(const char *byte);

/**
 * [Description]
 *
 * 	Traverses a NUL terminated byte array until the start of the next row.
 * 	This function EXPECTS a non-NULL byte pointer, it's undefined behavior otherwhise.
 * 	The width parameter is optional
 *
 * [Parameters]
 *
 * 	byte  - Starting point.
 * 	width - If given, the width of the row is written into it.
 * 	        NUL terminator and newline are not included.
 *
 * [Returns]
 *
 * 	Pointer to the start of the next row.
 * 	This function never returns nullptr.
 */
[[gnu::pure]] [[gnu::nonnull(1)]] [[gnu::returns_nonnull]] const char *cstr_next_row(const char *byte, size_t *width);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

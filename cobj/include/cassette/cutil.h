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
#include <stdlib.h>

#include "cerr.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/* IMPURE METHODS *******************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Convenience macro to not have to cast ptr and get param addresses.
 */
#define CUTIL_REALLOC(PTR, N_STORE, N_NEW, SIZE, ERR) \
	cutil_realloc((void**)&PTR, &N_STORE, N_NEW, SIZE, &ERR)

/**
 * [Description]
 *
 * 	Sort the pointed values so that *d_1 < *d_2.
 * 	If d_1 or d_2 is NULL, then this function has no effects.
 *
 * [Parameters]
 *
 * 	d_1 - First value.
 * 	d_2 - Second value.
 */
void cutil_sort_pair(double *d_1, double *d_2);

/**
 * [Description]
 *
 * 	Cassette's realloc wrapper for arrays.
 * 	If the function fails, a Cassette error is set.
 * 	The realloc size (n_new * size) should not be 0.
 * 	On success, the size of the new memory area is written into n_store if it's not NULL.
 *
 * [Parameters]
 *
 * 	ptr     - Pointer to memory to realloc.
 * 	n_store - Optional parameter to store the total size of the new memory area.
 * 	n_new   - Number of elements.
 * 	size    - Size of each element.
 * 	err     - Optional parameter to store an error in case of failure.
 *
 * [Returns]
 *
 * 	True when the realloc is successful, false otherwhise.
 * 	This function can fail when: ptr is NULL, n_new * size overflows, the realloc fails.
 * 	In case of failure, ptr and n_store are not modified.
 */
bool cutil_realloc(void **ptr, size_t *n_store, size_t n_new, size_t size, enum cerr *err);

/************************************************************************************************************/
/* PURE METHODS *********************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Limits the value of a double between two boundaries.
 * 	Boundaries can be in any order.
 *
 * [Parameters]
 *
 * 	d     - Value to clamp.
 * 	lim_1 - First boundary.
 * 	lim_2 - Second boundary.
 *
 * [Returns]
 *
 * 	Clamped value.
 */
[[gnu::const]] double cutil_clamp(double d, double lim_1, double lim_2);

/**
 * [Description]
 *
 * 	Checks if an environment variable exists and its value is not empty.
 *
 * [Parameters]
 *
 * 	name - Environment variable to check.
 *
 * [Returns]
 *
 * 	True if the variable is set and not empty, false otherwhise.
 */
bool cutil_env_exists(const char *name);

/**
 * [Description]
 *
 * 	Calculates a new value between two others using linear interpolation.
 * 	Boundaries can be in any order.
 *
 * [Parameters]
 *
 * 	d_1   - First boundary.
 * 	d_2   - Second boundary.
 * 	ratio - Interpolation ratio between 0.0 and 1.0.
 *
 * [Returns]
 *
 * 	Interpolated value.
 */
[[gnu::const]] double cutil_interpolate(double d_1, double d_2, double ratio);

/**
 * [Description]
 *
 * 	Checks if a given 2D point is inside a rectangular area.
 * 	The rectangle is defined like so:
 *
 * 	x,y --------------> width (invert if < 0.0)
 * 	 |
 * 	 |
 * 	 v
 * 	height (invert if < 0.0)
 *
 * [Parameters]
 *
 * 	x_check - X coordinate of the point.
 * 	y_check - Y coordinate of the point.
 * 	x       - X origin of the rectangle.
 * 	y       - Y origin of the rectangle.
 * 	width   - Width  of the rectangle, can be negative.
 * 	height  - Height of the rectangle, can be negative.
 *
 * [Returns]
 *
 * 	True if the point is inside (including borders), false otherwise.
 */
[[gnu::const]] bool cutil_point_inside(double x_check, double y_check, double x, double y, double width, double height);

/**
 * [Description]
 * 
 * 	After clampling the given double between two boudaries, calculate the progression % of that
 * 	double within the boundaries.
 * 	Boundaries can be in any order.
 *
 * [Parameters]
 *
 * 	d     - Value to convert to ratio.
 * 	lim_1 - First boundary.
 * 	lim_2 - Second boundary.
 *
 * [Returns]
 *
 * 	Progression ratio between the two boundary as a double between 0.0 and 1.0.
 */
[[gnu::const]] double cutil_ratio(double d, double lim_1, double lim_2);

/**
 * [Description]
 *
 * 	Converts a NUL terminated C string into a double floating value clamped between
 * 	lim_1 and lim_2.
 * 	Boundaries can be in any order.
 *
 * [Parameters]
 *
 * 	str   - String to convert.
 * 	lim_1 - First boundary.
 * 	lim_2 - Second boundary.
 *
 * [Returns]
 *
 * 	Converted value.
 * 	If str is NULL, this function always return lim_1.
 */
[[gnu::pure]] double cutil_str_to_double(const char *str, double lim_1, double lim_2);

/**
 * [Description]
 *
 * 	Converts a NUL terminated C string into a long integer clamped between lim_1 and lim_2.
 * 	Boundaries can be in any order.
 *
 * [Parameters]
 *
 * 	str   - String to convert.
 * 	lim_1 - First boundary.
 * 	lim_2 - Second boundary.
 *
 * [Returns]
 *
 * 	Converted value.
 * 	If str is NULL, this function always return lim_1.
 */
[[gnu::pure]] long cutil_str_to_long(const char *str, long lim_1, long lim_2);

/**
 * [Description]
 *
 * 	Retrieves the current Unix monotonic time.
 *
 * [Returns]
 *
 * 	Unix timestamp in microseconds.
 */
unsigned long cutil_time(void);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

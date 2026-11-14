/************************************************************************************************************/
/* PRELUDE **************************************************************************************************/
/************************************************************************************************************/

#pragma once

#include <stdbool.h>
#include <stdlib.h>

#include "cerr.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/* MUTATION *************************************************************************************************/
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
void cutil_sort_pair(double *d_1, double *d_2) [[reproducible]];

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
/* ACCESS ***************************************************************************************************/
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
double cutil_clamp(double d, double lim_1, double lim_2) [[unsequenced]];

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
double cutil_interpolate(double d_1, double d_2, double ratio) [[unsequenced]];

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
bool cutil_point_inside(double x_check, double y_check, double x, double y, double width, double height) [[unsequenced]];

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
double cutil_ratio(double d, double lim_1, double lim_2) [[unsequenced]];

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
double cutil_str_to_double(const char *str, double lim_1, double lim_2) [[reproducible]];

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
long cutil_str_to_long(const char *str, long lim_1, long lim_2) [[reproducible]];

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

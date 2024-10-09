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

#ifndef DG_CORE_ERRNO_H
#define DG_CORE_ERRNO_H

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/**
 * Error values.
 */
typedef enum {
	DG_CORE_ERRNO_NONE = 0,
	DG_CORE_ERRNO_MEMORY,
	DG_CORE_ERRNO_STACK,
	DG_CORE_ERRNO_HASHTABLE,
	DG_CORE_ERRNO_CAIRO,
	DG_CORE_ERRNO_CAIRO_CRIT,
	DG_CORE_ERRNO_XCB,
	DG_CORE_ERRNO_XCB_CRIT,
	DG_CORE_ERRNO_DEPENDENCY,
} dg_core_errno_t;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/**
 * Get the last error code that has been submitted. Everytime it's called the internal error tracker is reset
 * to DG_CORE_ERRNO_NONE. The internal error counter is not affected.
 *
 * @return : error code of the last error if any, else DG_CORE_ERRNO_NONE
 */
dg_core_errno_t dg_core_errno_get(void);

/**
 * Get the last error code that has been submitted. Unlike dg_core_errno_get(), the internal error tracker is
 * not reset reseted after each call.
 *
 * @return : error code of the last error if any, else DG_CORE_ERRNO_NONE
 */
dg_core_errno_t dg_core_errno_get_no_reset(void);

/**
 * Set an error code. Interal error counter is incremented by 1.
 * If the error code is DG_CORE_ERRNO_NONE then this function has no effect (including the counter).
 *
 * @param err : error code to set (see dg_core_errno_t)
 */
void dg_core_errno_set(dg_core_errno_t err);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Get the number of times an error has been submitted.
 *
 * @return : self-explanatory
 */
unsigned int dg_core_errno_count(void);

/**
 * Convert an error code id to a human readable string.
 *
 * @param err : error code to translate (see dg_core_errno_t)
 *
 * @return : string representing the error message
 */
const char *dg_core_errno_to_str(dg_core_errno_t err);

/**
 * Set a callback function that gets executed when an error is set with dg_core_errno_set().
 * Call with fn = NULL to unset the callback.
 *
 * @param fn : function to set as callback
 *
 * @subparam fn.err : the error code that was set
 */
void dg_core_errno_set_callback(void (*fn)(dg_core_errno_t err));

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DG_CORE_ERRNO_H */

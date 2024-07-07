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
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

#pragma once

#include <stdbool.h>
#include <stdlib.h>

#if __GNUC__ > 4
	#define CREF_NONNULL_RETURN __attribute__((returns_nonnull))
	#define CREF_NONNULL(...)   __attribute__((nonnull (__VA_ARGS__)))
	#define CREF_PURE           __attribute__((pure))
#else
	#define CREF_NONNULL_RETURN
	#define CREF_NONNULL(...)
	#define CREF_PURE
#endif

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/**
 * Opaque reference counter object instance. This object stores references in an array that gets automatically
 * extended when new references gets added.
 * This object holds an internal error bitfield that can be checked with cref_error(). Some functions, upon
 * failure, can trigger specific error bits and will exit early without side effects. If the error bitfield is
 * set to anything else than CREF_OK, any function that takes this object as an argument will return early
 * with no side effects and default return values. It is possible to repair the object to get rid of errors.
 * See cref_repair() for more details.

 */
typedef struct cref cref;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Error types.
 */
enum cref_err
{
	CREF_OK       = 0,
	CREF_INVALID  = 1,
	CREF_OVERFLOW = 1 << 1,
	CREF_MEMORY   = 1 << 2,
};

/************************************************************************************************************/
/* GLOBALS **************************************************************************************************/
/************************************************************************************************************/

/**
 * A macro that gives uninitialized reference counters a non-NULL value that is safe to use with the
 * reference counter's related functions. However, any function called with a handle set to this value will
 * return early and without any side effects.
 */
#define CREF_PLACEHOLDER &cref_placeholder_instance

/**
 * Global reference counter instance with the error state set to CBOOK_INVALID. This instance is only made
 * available to allow the static initialization of reference counter pointers with the macro CREF_PLACEHOLDER.
 */
extern cref cref_placeholder_instance;

/************************************************************************************************************/
/* CONSTRUCTORS / DESTRUCTORS *******************************************************************************/
/************************************************************************************************************/

/**
 * Creates a reference counter and deep copy the contents of another reference counter into it.
 *
 * @param ref : Reference counter to copy contents from
 *
 * return     : New reference counter instance
 * return_err : CREF_PLACEHOLDER
 */
cref *
cref_clone(cref *ref)
CREF_NONNULL_RETURN
CREF_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Creates an empty reference counter.
 *
 * return     : New reference counter instance
 * return_err : CREF_PLACEHOLDER
 */
cref *
cref_create(void)
CREF_NONNULL_RETURN;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Destroys the reference counter and frees memory.
 *
 * @param ref : Reference counter to interact with
 */
void
cref_destroy(cref *ref)
CREF_NONNULL(1);

/************************************************************************************************************/
/* PROCEDURES ***********************************************************************************************/
/************************************************************************************************************/

/**
 * Convenience for loop wrapper.
 */
#define CREF_FOR_EACH(REF, I) for(size_t I = 0; I < cref_length(REF); I++)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Convenience inverse for loop wrapper.
 */
#define CREF_FOR_EACH_REV(REF, I) for(size_t I = cref_length(REF) - 1; I < SIZE_MAX; I--)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Convenience generic wrapper to pull a reference.
 */
#define cref_pull(REF, VAL) \
	_Generic (VAL, \
		int     : cref_pull_index, \
		size_t  : cref_pull_index, \
		default : cref_pull_ptr    \
	)(REF, VAL)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Convenience generic wrapper to purge a reference.
 */
#define cref_purge(REF, VAL) \
	_Generic (VAL, \
		int     : cref_purge_index, \
		size_t  : cref_purge_index, \
		default : cref_purge_ptr    \
	)(REF, VAL)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Clears the contents of a given reference counter. Allocated memory is not freed, use cref_destroy() for
 * that.
 *
 * @param ref : Reference counter to interact with
 */
void
cref_clear(cref *ref)
CREF_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Preallocates slots for the reference array to avoid triggering multiple automatic reallocs when pushing new
 * references. This function has no effect if the requested number of slots is smaller than the previously
 * allocated amounts.
 *
 * @param ref : Reference counter to interact with
 *
 * @error CREF_OVERFLOW : The size of the resulting reference array will be > SIZE_MAX
 * @error CREF_INVALID  : Failed memory allocation
 */
void
cref_prealloc(cref *ref, size_t slots_number)
CREF_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Decrements the counter of a reference at the given index. If the counter reaches 0, the referece gets
 * removed from the reference arrau. This function has no effects if index is out of bounds.
 *
 * @param ref   : Reference counter to interact with
 * @param index : Index within the array
 */
void
cref_pull_index(cref *ref, size_t index)
CREF_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Searches for a reference with the matching pointer. If found, it's counter gets decremented. If the counter
 * then reached 0, the referece gets removed from the reference arrau.
 *
 * @param ref : Reference counter to interact with
 * @param ptr : Pointer
 */
void
cref_pull_ptr(cref *ref, const void *ptr)
CREF_NONNULL(1, 2);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Removes a reference at the given index regardless of its count. This function has no effects if index is
 * out of bounds.
 *
 * @param ref   : Reference counter to interact with
 * @param index : Index within the array
 */
void
cref_purge_index(cref *ref, size_t index)
CREF_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Searches for a reference with the matching pointer. If found, the reference gets removed regardless of its
 * count.
 *
 * @param ref : Reference counter to interact with
 * @param ptr : Pointer
 */
void
cref_purge_ptr(cref *ref, const void *ptr)
CREF_NONNULL(1, 2);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Searches for a reference with the matching pointer. If found, its counter gets incremented. If not, the
 * reference gets added at the end of the reference array with its count = 1. The array get automatically
 * extended as needed.
 *
 * @param ref : Reference counter to interact with
 *
 * @error CREF_OVERFLOW : The size of the resulting reference array will be > SIZE_MAX
 * @error CREF_INVALID  : Failed memory allocation
 */
void
cref_push(cref *ref, const void *ptr)
CREF_NONNULL(1, 2);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Clears errors and puts the reference counter back into an usable state. The only unrecoverable error is
 * CREF_INVALID.
 *
 * @param ref : Reference counter to interact with
 */
void
cref_repair(cref *ref)
CREF_NONNULL(1);

/************************************************************************************************************/
/* FUNCTIONS ************************************************************************************************/
/************************************************************************************************************/

/**
 * Gets the reference count at the given index. If index is out of bounds, the default return_err value is
 * returned.
 *
 * @param ref   : Reference counter to interact with
 * @param index : Index within the array
 *
 * @return     : Reference count
 * @return_err : 0
 */
unsigned int
cref_count(const cref *ref, size_t index)
CREF_NONNULL(1)
CREF_PURE;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Gets the error state.
 *
 * @param ref : Reference counter to interact with
 *
 * @return : Error bitfield
 */
enum cref_err
cref_error(const cref *ref)
CREF_NONNULL(1)
CREF_PURE;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Tries to find a reference with the matching pointer value. If found, the reference count is returned (>0),
 * and if the optional index parameter is not NULL, the array index of the found reference will be written
 * into it. If not found, return_err is returned.
 *
 * @param ref   : Reference counter to interact with
 * @param ptr   : Pointer to search
 * @param index : Optional parameter, index of the found reference
 *
 * @return     : Reference count
 * @return_err : 0
 */
unsigned int
cref_find(const cref *ref, const void *ptr, size_t *index)
CREF_NONNULL(1, 2);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Gets the total number of different tracked references.
 *
 * @param ref : Reference counter to interact with
 *
 * @return     : Number of different references
 * @return_err : 0
 */
size_t
cref_length(const cref *ref)
CREF_NONNULL(1)
CREF_PURE;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Gets the reference pointer at the given index. If index is out of bounds, the default return_err value is
 * returned.
 *
 * @param ref   : Reference counter to interact with
 * @param index : Index within the array
 *
 * @return     : Pointer
 * @return_err : NULL
 */
const void *
cref_ptr(const cref *ref, size_t index)
CREF_NONNULL(1)
CREF_PURE;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

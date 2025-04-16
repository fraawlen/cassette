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
/* TYPES ****************************************************************************************************/
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
/************************************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Opaque reference counter object implemented as a dynamic void pointer array. The counter
 * 	automatically grows when new pointers get pushed. When the same pointer gets pushed multiple
 * 	times, its reference count increases. A saved value only gets deleted when its reference count
 * 	reaches 0.
 *
 * 	Some methods may fail and set an internal error, which can be checked using cref_error().
 * 	If an error is set, all methods will exit early with default return values and no side
 * 	effects, leaving only the destruction function available.
 */
typedef struct cref cref;

/************************************************************************************************************/
/* CONSTRUCTORS / DESTRUCTORS *******************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Destroys a reference counter and frees all associated memory.
 * 	Calling this function on a NULL counter has no effect.
 *
 * [Parameters]
 *
 * 	ref - Reference counter to destroy.
 *
 * [Returns]
 *
 * 	To prevent dangling pointers while keeping the function a one-liner, this function
 * 	conveniently returns nullptr.
 */
[[nodiscard]] nullptr_t cref_destroy(cref *ref);

/**
 * [Description]
 *
 * 	Creates a reference counter and deep copies the contents of another reference counter into it.
 *
 * [Parameters]
 *
 * 	ref - Reference counter to copy.
 *
 * [Returns]
 *
 * 	On success, a pointer to a newly allocated instance. Returns nullptr on failure.
 * 	If the counter is NULL or in a critical error state, this function always returns nullptr.
 * 	The caller is responsible for freeing the returned instance using cref_destroy().
 */
[[nodiscard]] [[gnu::malloc(cref_destroy)]] cref *cref_clone(const cref *ref);

/**
 * [Description]
 *
 * 	Creates an empty reference counter.
 *
 * [Returns]
 *
 * 	On success, a pointer to a newly allocated instance. Returns nullptr on failure.
 * 	The caller is responsible for freeing the returned instance using cref_destroy().
 */
[[nodiscard]] [[gnu::malloc(cref_destroy)]] cref *cref_create(void);

/************************************************************************************************************/
/* IMPURE METHODS *******************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Convenience generic wrapper to pull a reference.
 */
#define cref_pull(REF, VAL) \
	_Generic (VAL, \
		int     : cref_pull_index, \
		size_t  : cref_pull_index, \
		default : cref_pull_ptr    \
	)(REF, VAL)

/**
 * [Description]
 *
 * 	Convenience generic wrapper to purge a reference.
 */
#define cref_purge(REF, VAL) \
	_Generic (VAL, \
		int     : cref_purge_index, \
		size_t  : cref_purge_index, \
		default : cref_purge_ptr    \
	)(REF, VAL)

/**
 * [Description]
 *
 * 	Clears the contents of the reference counter.
 * 	Allocated memory is not freed, use cref_destroy() for that.
 * 	Calling this function on a NULL reference counter has no effect.
 *
 * [Parameters]
 *
 * 	ref - Reference counter to modify.
 */
void cref_clear(cref *ref);

/**
 * [Description]
 *
 * 	Clears any warning error the reference counter may have. Does not clears criticial errors.
 * 	Calling this function on a NULL reference counter has no effect.
 *
 * [Parameters]
 *
 * 	ref - Reference counter to modify.
 */
void cref_clear_warnings(cref *ref);

/**
 * [Description]
 *
 * 	Preallocates a set number of slots to prevent multiple automatic reallocations when pushing
 * 	new values.
 *
 * 	This function has no effect if the requested slot number is smaller than the
 * 	current allocation or a NULL reference counter is given.
 *
 * [Parameters]
 *
 * 	ref          - Reference counter to modify.
 * 	slots_number - Minimum number of slots to preallocate.
 *
 * [Errors]
 *
 * 	CERR_OVERFLOW
 * 	CERR_MEMORY
 */
void cref_prealloc(cref *ref, size_t slots_number);

/**
 * [Description]
 *
 * 	Decrements the reference count of the value at an index.
 * 	If the reference count reaches 0, the value is removed.
 * 	If the value is removed, the index of other values may change.
 * 	This function has no effects if index is out of bounds or a NULL reference counter is given.
 *
 * [Parameters]
 *
 * 	ref   - Reference counter to modify.
 * 	index - Index within the array.
 */
void cref_pull_index(cref *ref, size_t index);

/**
 * [Description]
 *
 * 	Searches for a value match. If found, it's reference count is decremented.
 * 	If the reference count reaches 0, the value is removed.
 * 	If the value is removed, the index of other values may change.
 * 	This function has no effects if the value is not found or a NULL reference counter is given.
 *
 * [Parameters]
 *
 * 	ref - Reference counter to modify.
 * 	ptr - Pointer.
 */
void cref_pull_ptr(cref *ref, void *ptr);

/*
 * [Description]
 *
 * 	Removes the value at an index regardless of its reference count.
 * 	After the value is removed, the index of other values may change.
 * 	This function has no effects if index is out of bounds or a NULL reference counter is given.
 *
 * [Parameters]
 *
 * 	ref   - Reference counter to modify.
 * 	index - Index within the array.
 */
void cref_purge_index(cref *ref, size_t index);

/**
 * [Description]
 *
 * 	Searches for a value match. If found, it's removed regardless of its reference count.
 * 	After the value is removed, the index of other values may change.
 * 	This function has no effects if the value is not found or a NULL reference counter is given.
 *
 * [Parameters]
 *
 * 	ref - Reference counter to modify.
 * 	ptr - Pointer.
 */
void cref_purge_ptr(cref *ref, void *ptr);

/**
 * [Description]
 *
 * 	Seaches for a value match. If found, its reference counter get incremented. If not, value is
 * 	added at the end of the value array with a reference count of 1.
 * 	The reference counter amy automatically allocate additional slots to accomodate the new value.
 * 	Calling this function on a NULL reference counter or pointer value has no effect.
 *
 * [Parameters]
 *
 * 	ref - Reference counter to modify.
 * 	ptr - Pointer.
 *
 * [Errors]
 *
 * 	CERR_OVERFLOW
 * 	CERR_MEMORY
 */
void cref_push(cref *ref, void *ptr);

/************************************************************************************************************/
/* PURE METHODS *********************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Convenience for-loop wrapper.
 * 	Parameter I is declared internally, you should only provide the desired identifier.
 */
#define CREF_FOR_EACH(REF, I) for(size_t I = 0; I < cref_length(REF); I++)

/**
 * [Description]
 *
 * 	Convenience inverse for-loop wrapper.
 * 	Parameter I is declared internally, you should only provide the desired identifier.
 */
#define CREF_FOR_EACH_REV(REF, I) for(size_t I = cref_length(REF) - 1; I < SIZE_MAX; I--)

/**
 * [Description]
 *
 * 	Retrieves the reference count of the value at an index.
 *
 * [Parameters]
 *
 * 	ref   - Reference counter to search.
 * 	index - Index within the array.
 *
 * [Returns]
 *
 * 	The reference count.
 * 	If the reference counter is NULL, in a critical error state, or the index is out of bounds, 
 * 	this function always returns 0.
 */
[[gnu::pure]] unsigned int cref_count(const cref *ref, size_t index);

/**
 * [Description]
 *
 * 	Retrieves the reference counter's current error state.
 *
 * [Parameters]
 *
 * 	ref - Reference counter to inspect.
 *
 * [Returns]
 *
 * 	The current error code.
 * 	If the dictionary is NULL, this function always returns CERR_INVALID.
 */
[[gnu::pure]] enum cerr cref_error(const cref *ref);

/**
 * [Description]
 *
 * 	Searches for the reference count of a value.
 * 	If the optional index parameter is not NULL, and the value is found within the value array, 
 * 	the associated slot index will be written to it.
 *
 * [Parameters]
 *
 * 	ref   - Reference counter to search.
 * 	ptr   - Pointer to search.
 * 	index - Optional pointer to store the associated value index.
 *
 * [Returns]
 *
 * 	If a matching value is found, its reference count is returned. If not, 0 is returned instead.
 * 	If the reference counter is NULL, in a critical error state, or the pointer value is NULL,
 * 	this function always returns 0.
 */
unsigned int cref_find(const cref *ref, void *ptr, size_t *index);

/**
 * [Description]
 *
 * 	Retrieves the total number of different tracked values.
 *
 * [Parameters]
 *
 * 	ref - Reference counter to inspect.
 *
 * [Returns]
 *
 * 	Number of unique values stored.
 * 	If the reference counter is NULL or in a critical error state, this function always returns 0.
 */
[[gnu::pure]] size_t cref_length(const cref *ref);

/**
 * [Description]
 *
 * 	Retrieves the value at an index.
 *
 * [Parameters]
 *
 * 	ref   - Reference counter to inspect.
 * 	index - Index within the array.
 *
 * [Returns]
 *
 * 	Value at index.
 * 	If the reference counter is NULL, in a critical error state, or the index is out of bounds,
 * 	this function always returns nullptr.
 */
[[gnu::pure]] void *cref_ptr(const cref *ref, size_t index);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

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
 *
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
/************************************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
cref *
cref_clone(cref *ref)
CREF_NONNULL_RETURN
CREF_NONNULL(1);

/**
 *
 */
cref *
cref_create(void)
CREF_NONNULL_RETURN;

/**
 *
 */
void
cref_destroy(cref *ref)
CREF_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

#define cref_pull(REF, VAL) \
	_Generic (VAL, \
		int     : cref_pull_index, \
		size_t  : cref_pull_index, \
		default : cref_pull_ptr    \
	)(REF, VAL)

#define cref_purge(REF, VAL) \
	_Generic (VAL, \
		int     : cref_purge_index, \
		size_t  : cref_purge_index, \
		default : cref_purge_ptr    \
	)(REF, VAL)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
void
cref_clear(cref *ref)
CREF_NONNULL(1);

/**
 *
 */
void
cref_init_iterator(cref *ref)
CREF_NONNULL(1);

/**
 *
 */
bool
cref_iterate(cref *ref)
CREF_NONNULL(1);

/**
 *
 */
void
cref_lock_iterator(cref *ref)
CREF_NONNULL(1);

/**
 *
 */
void
cref_prealloc(cref *ref, size_t slots_number)
CREF_NONNULL(1);

/**
 *
 */
void
cref_pull_index(cref *ref, size_t index)
CREF_NONNULL(1);

/**
 *
 */
void
cref_pull_ptr(cref *ref, const void *ptr)
CREF_NONNULL(1, 2);

/**
 *
 */
void
cref_purge_index(cref *ref, size_t index)
CREF_NONNULL(1);

/**
 *
 */
void
cref_purge_ptr(cref *ref, const void *ptr)
CREF_NONNULL(1, 2);

/**
 *
 */
void
cref_push(cref *ref, const void *ptr)
CREF_NONNULL(1, 2);

/**
 *
 */
void
cref_repair(cref *ref)
CREF_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
const void *
cref_at_index(const cref *ref, size_t index)
CREF_NONNULL(1)
CREF_PURE;

/**
 *
 */
unsigned int
cref_at_index_count(const cref *ref, size_t index)
CREF_NONNULL(1)
CREF_PURE;

/**
 *
 */
enum cref_err
cref_error(const cref *ref)
CREF_NONNULL(1)
CREF_PURE;

/**
 *
 */
unsigned int
cref_find(const cref *ref, const void *ptr, size_t *index)
CREF_NONNULL(1, 2);

/**
 *
 */
const void *
cref_iteration(const cref *ref)
CREF_NONNULL(1)
CREF_PURE;

/**
 *
 */
unsigned int
cref_iteration_count(const cref *ref)
CREF_NONNULL(1)
CREF_PURE;

/**
 *
 */
size_t
cref_iterator_offset(const cref *ref)
CREF_NONNULL(1)
CREF_PURE;

/**
 *
 */
size_t
cref_length(const cref *ref)
CREF_NONNULL(1)
CREF_PURE;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/**
 * A macro that gives uninitialized reference trackers a non-NULL value that is safe to use with the
 * reference tracker's related functions. However, any function called with a handle set to this value will
 * return early and without any side effects.
 */
#define CREF_PLACEHOLDER &cref_placeholder_instance

/**
 * Global reference tracker instance with the error state set to CBOOK_INVALID. This instance is only made
 * available to allow the static initialization of reference tracker pointers with the macro CREF_PLACEHOLDER.
 */
extern cref cref_placeholder_instance;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

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

#ifndef DU_TRACKER_H
#define DU_TRACKER_H

#include <stdbool.h>
#include <stdlib.h>

#include "du-status.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define DU_TRACKER_EMPTY {.ptr = NULL, .n = 0, .n_alloc = 0, .status = DU_STATUS_SUCCESS}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Stack of arbitrary pointers. Used to store, retrieve and track any arbitrary element of data, which is
 * referenced inside the tracker by the generic void pointer array. n <= n_alloc.
 * If status is set to DU_STATUS_FAILURE all handler functions will have no effect with the exception of
 * du_tracker_reset().
 *
 * @param ptr     : array of stored pointers
 * @param n       : size of actively used array space
 * @param n_alloc : size of allocated array space
 * @param status  : error state
 */
typedef struct {
	const void **ptr;
	size_t n;
	size_t n_alloc;
	du_status_t status;
} du_tracker_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Preallocate memory to the tracker and set its variables appropriately. Allocated memory in the array is
 * initialised to 0. This function is optional because a tracker can allocate memory automatically when
 * needed. If n = 0, no memory is allocated and *tracker is set to DU_TRACKER_EMPTY.
 * In case of error, tracker->status will be set to DU_STATUS_FAILURE. It's set to DU_STATUS_SUCCESS
 * otherwhise.
 *
 * @param tracker : tracker to init
 * @param n_alloc : initial size of the pointer array to pre-allocate. 
 */
void du_tracker_init(du_tracker_t *tracker, size_t n_alloc);

/**
 * Zeroes the tracker and free allocated memory within the structure. The given structure itself is not freed,
 * and may require an explicit free operation. The pointers that were referenced in **ptr are not freed
 * either. The status of the structure is also reset to DU_STATUS_SUCCESS.
 *
 * @param tracker : tracker to reset
 */
void du_tracker_reset(du_tracker_t *tracker);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Removes a pointer from the tracker. The value pointed to by the removed pointer is not freed nor modified.
 * The tracker is not shrinked when a pointer is pulled from it, unless there is no remaining pointer in it
 * (n == 0), in which case du_tracker_reset() is called internally and memory is freed. If the given
 * pointer was not already part of tracker, nothing happens.
 *
 * @param tracker : tracker to remove items from
 * @param ptr     : pointer to pull from the tracker
 */
void du_tracker_pull(du_tracker_t *tracker, const void *ptr);

/**
 * Adds a pointer to the tracker. A given pointer can only be added once, if a duplicate pointer is given,
 * this function has no effect. New pointers are always added at the end of the array inside the struct. The
 * tracker's array is automatically expands as needed. In case of expansion failure, tracker->status will be set
 * to DU_STATUS_FAILURE.
 *
 * @param tracker : tracker to add items to
 * @param ptr     : pointer to push to tracker
 * @param index   : optional, if non NULL, du_tracker_push() will put in it, on success or if a duplicate is
 *                  given, the position of the pointer within the tracker. In case of failure, *index is
 *                  unmodified. Because this function internally uses du_tracker_find(), this parameter
 *                  follows the same rules, in other words, if provided, its pointed value should be
 *                  initialised.
 */
void du_tracker_push(du_tracker_t *tracker, const void *ptr, size_t *index);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Checks if given pointer is present in the tracker.
 *
 * @param tracker : tracker to search
 * @param ptr     : pointer to find
 * @param index   : optional, pointer whose value will be set to the position index of the found item. If the
 *                  pointer is not found this paramater is not modified. This parameter is optional and can be
 *                  set to NULL. This parameter is also used as the starting point of the array scan for speed
 *                  optimisation. Therefore, if provided, its pointed value should be initialised
 *
 * @return : true if item is present, false otherwhise.
 */
bool du_tracker_find(const du_tracker_t *tracker, const void *ptr, size_t *index);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DU_TRACKER_H */

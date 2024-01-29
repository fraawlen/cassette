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

#ifndef DU_INPUTS_H
#define DU_INPUTS_H

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#include "du-status.h"
#include "du-types.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define DU_INPUTS_EMPTY {.slots = NULL, .n = 0, .n_alloc = 0}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Input list slot.
 *
 * @param id     : input's identifier (touchid, mouse button, keycode. ...)
 * @param ref    : arbitrary reference to link the input to
 * @param coords : input coordinates
 */
typedef struct {
	uint32_t id;
	void *ref;
	du_coordinates_t coords;
} du_inputs_slot_t;

/**
 * Specialized structure to keep track of active end-user inputs, such as, but not limited to, screen touches,
 * button and key presses. Hence the coordinate associated to each tracked input. n <= n_alloc. Unlike
 * du_tracker_t, the array is not auto-extensible, instead, if the maximum amount of inputs is reached, the
 * following inputs get ignored.
 * If status is set to DU_STATUS_FAILURE all handler functions will have no effect with the exception of
 * du_inputs_reset().
 *
 * @param slots
 * @param n
 * @param n_alloc
 * @param status
 */
typedef struct {
	du_inputs_slot_t *slots;
	size_t n;
	size_t n_alloc;
	du_status_t status;
} du_inputs_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Pre-allocate memory to the input tracker and set its variables appropriately. Allocated memory in the
 * array is initialised to 0. If n = 0, no memory is allocated and *tracker is set to DU_TRACKER_EMPTY.
 * In case of error, tracker->status will be set to DU_STATUS_FAILURE. It's set to DU_STATUS_SUCCESS
 * otherwhise.
 *
 * @param inputs : input list to init
 * @param n_alloc : initial size of the pointer array to pre-allocate.
 */
void du_inputs_init(du_inputs_t *inputs, size_t n_alloc);

/**
 *
 * @param inputs : input list to reset
 */
void du_inputs_reset(du_inputs_t *inputs);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Removes all tracked values. Internal memory however is not freed, use du_inputs_reset() for that.
 *
 * @param inputs : input list to clear
 */
void du_inputs_clear(du_inputs_t *inputs);

/**
 * Removes a tracked input.
 *
 * @param inputs : input list to push to
 * @param id     : input id to match
 */
void du_inputs_pull(du_inputs_t *inputs, uint32_t id);

/**
 * Adds a new input to the end of the array. If the input's id is already present in the input list it's
 * data will just be updated. If the maximum amount of inputs is reached, this a function has no effect and
 * the input will not be tracked.
 *
 * @param inputs : input list to pull from
 * @param id     : input id to match
 * @param ref    : arbitrary pointer reference of the input
 * @param x      : coordinate x
 * @param y      : coordinate y
 */
void du_inputs_push(du_inputs_t *inputs, uint32_t id, void *ref, du_position_t x, du_position_t y);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Locates a given input id if it is present in the array.
 *
 * @param inputs : input list to search
 * @param id     : input id to match
 * @param index  : optional, pointer whose value will be set to the position index of the input with the
 *                 matching id. If the item is not found this paramater is not modified. This parameter is
 *                 optional and can be set to NULL
 */
bool du_inputs_find(const du_inputs_t *inputs, uint32_t id, size_t *index);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DU_INPUTS_H */

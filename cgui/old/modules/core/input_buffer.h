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

#ifndef DG_CORE_INPUT_BUFFER_H
#define DG_CORE_INPUT_BUFFER_H

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define DG_CORE_INPUT_BUFFER_MAX_TOUCHES 10
#define DG_CORE_INPUT_BUFFER_MAX_BUTTONS 12

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/*
 * Kind to indentify the type of inputs are given to an input buffer.
 */
typedef enum {
	DG_CORE_INPUT_BUFFER_REF,
	DG_CORE_INPUT_BUFFER_COORD,
} dg_core_input_buffer_kind_t;

/**
 * Slot to store an input and its information.
 * It relies on C11 anonymous unions and structs to set its kind dependent fields.
 * Only use kind dependents fields after checking the kind.
 *
 * @param id  : input identifier (like the button or key value for example)
 * @param ref : generic pointer referencing the input in one way or another
 * @param x   : x coordinate value
 * @param y   : y coordinate value
 */
typedef struct {
	uint32_t id;
	union {
		/* DG_CORE_INPUT_BUFFER_REF */
		void *ref;
		/* DG_CORE_INPUT_BUFFER_COORD */
		struct {
			int16_t x;
			int16_t y;
		};
	};
} dg_core_input_buffer_slot_t;

/**
 * Buffer type struct used to hold and track user input such as pointer buttons, keyboard keys and screen
 * touches. Tracked inputs are sorted by arrival with the first item being the first input. When an input is
 * removed all succeding inputs are shifted toward position 0. So as long as n_alloc > 0, inputs[0] is valid.
 * Inputs can either be stored as a reference, with a generic pointer, or as coordinates. n <= n_alloc.
 *
 * @param kind    : kind of the buffer, used to identify how to read values of an input
 * @param inputs  : input array
 * @param n       : amount of inputs currently being used
 * @param n_alloc : size of the allocated input array
 */
typedef struct {
	dg_core_input_buffer_kind_t kind; 
	dg_core_input_buffer_slot_t *inputs;
	size_t n;
	size_t n_alloc;
} dg_core_input_buffer_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Sets the size of the input buffer and allocates memory to the input array.
 *
 * @param buf     : buffer to interact with
 * @param n_alloc : maximum amount of trackable simultaneous inputs to allocate memory for
 * @param kind    : kind of the buffer, that will be used for all buffer_inputs
 *
 * @return : true on success, false otherwhise
 *
 * @error DG_CORE_ERRNO_MEMORY : out of memory for input array initialisation
 */
bool dg_core_input_buffer_init(dg_core_input_buffer_t *buf, size_t n_alloc, dg_core_input_buffer_kind_t kind);

/**
 * Frees memory and zeroes other variables.
 *
 * @param buf : buffer to interact with
 */
void dg_core_input_buffer_reset(dg_core_input_buffer_t *buf);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Locates the position of a given input (by its id) in the buffer)
 *
 * @param buf : buffer to interact with
 * @param id  : identifier of the input
 * @param pos : optional, if not set to NULL, the pointed value is set to the input's position in the buffer
 *              if it is present, otherwhise it is unmodified.
 *
 * @return : true is the input is present, false otherwise
 */
bool dg_core_input_buffer_find(dg_core_input_buffer_t *buf, uint32_t id, size_t *pos);

/**
 * Removes a value from the buffer. The values comming after it are pushed one step toward the beginning.
 *
 * @param buf : buffer to interact with
 * @param id  : identifier of the input
 *
 * @return : true on success, false otherwhise (because the id is not present within the buffer)
 */
bool dg_core_input_buffer_pull(dg_core_input_buffer_t *buf, uint32_t id);

/**
 * Add a value to the end of the buffer. If the value is already present in the buffer it will be moved to
 * the end. The registered inputs are of kind DG_CORE_INPUT_BUFFER_COORD. The buffer to use has to be
 * initialised to the matching kind before calling this function. 
 *
 * @param buf : buffer to interact with
 * @param id  : identifier of the input
 * @param x   : coordinate x
 * @param y   : coordinate y
 *
 * @return : true on success, false otherwhise (no more space left in the buffer)
 */
bool dg_core_input_buffer_push_coord(dg_core_input_buffer_t *buf, uint32_t id, int16_t x, int16_t y);

/**
 * Add a value to the end of the buffer. If the value is already present in the buffer it will be moved to
 * the end. The registered inputs are of kind DG_CORE_INPUT_BUFFER_REF. The buffer to use has to be
 * initialised to the matching kind beofre calling this function.
 *
 * @param id  : identifier of the input
 * @param ref : pointer reference of the input
 *
 * @return : true on success, false otherwhise (no more space left in the buffer)
 */
bool dg_core_input_buffer_push_ref(dg_core_input_buffer_t *buf, uint32_t id, void *ref);

/**
 * Removes all tracked values. Internal memory however is not freed, use dg_core_input_buffer_reset() for
 * that.
 *
 * @param buf : buffer to interact with
 */
void dg_core_input_buffer_clear(dg_core_input_buffer_t *buf);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DG_CORE_INPUT_BUFFER_H */

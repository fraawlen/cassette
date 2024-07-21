/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Graphics (CGUI) library.
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

#include "cerr.h"

#if __GNUC__ > 4
	#define CINPUTS_NONNULL_RETURN __attribute__((returns_nonnull))
	#define CINPUTS_NONNULL(...)   __attribute__((nonnull (__VA_ARGS__)))
	#define CINPUTS_PURE           __attribute__((pure))
#else
	#define CINPUTS_NONNULL_RETURN
	#define CINPUTS_NONNULL(...)
	#define CINPUTS_PURE
#endif

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

/**
 * Opaque input tracker object. An input tracker stores inputs such as screen touches, key or button presses
 * in the order they get added. The array that holds them is fixed size. If that array is full, new inputs get
 * ignored.
 *
 * Some methods, upon failure, will set an error bit in an internal error bitfield. The error can be checked
 * with cinputs_error(). If any error is set all inputs tracke methods will exit early with default return
 * values and no side-effects. It's possible to clear errors with cinputs_repair().
 */
typedef struct cinputs cinputs;

/************************************************************************************************************/
/* GLOBALS **************************************************************************************************/
/************************************************************************************************************/

/**
 * A macro that gives uninitialized books a non-NULL value that is safe to use with the input tracker's
 * related functions. However, any function called with a handle set to this value will return early and
 * without any side effects.
 */
#define CINPUTS_PLACEHOLDER &cinputs_placeholder_instance

/**
 * Global input tracker instance with the error state set to CERR_INVALID. This instance is made available to
 * allow the static initialization of input tracker pointers with the macro CINPUTS_PLACEHOLDER.
 */
extern cinputs cinputs_placeholder_instance;

/************************************************************************************************************/
/* CONSTRUCTORS / DESTRUCTORS *******************************************************************************/
/************************************************************************************************************/

/** 
 * Creates an input tracker and deep copy the contents of another input tracker into it.
 *
 * @param inputs : Input tracker to copy contents from
 *
 * @return     : New input tracker instance
 * @return_err : CINPUTS_PLACEHOLDER;
 */
cinputs *
cinputs_clone(const cinputs *inputs)
CINPUTS_NONNULL_RETURN
CINPUTS_NONNULL(1);

/** 
 * Creates an empty input tracker.
 *
 * @param max_inputs : Maximum number of inputs to track at a time. 0 is an illegal value.
 *
 * @return     : New input tracker instance
 * @return_err : CINPUTS_PLACEHOLDER;
 */
cinputs *
cinputs_create(size_t max_inputs)
CINPUTS_NONNULL_RETURN;

/** 
 * Destroys the input tracker and frees memory.
 *
 * @param inputs : Input tracker to interact with
 */
void
cinputs_destroy(cinputs *inputs)
CINPUTS_NONNULL(1);

/************************************************************************************************************/
/* IMPURE METHODS *******************************************************************************************/
/************************************************************************************************************/

/**
 * Convenience for-loop wrapper.
 */
#define CINPUTS_FOR_EACH(INPUTS, I) for(size_t I = 0; I < cinputs_load(INPUTS); I++)

/**
 * Convenience inverse for-loop wrapper.
 */
#define CINPUTS_FOR_EACH_REV(INPUTS, I) for(size_t I = cinputs_load(INPUTS) - 1; I < SIZE_MAX; I--)

/**
 * Clears the contents of a given input tracker. Allocated memory is not free, use cinputs_destroy() for that.
 *
 * @param inputs : Input tracker to interact with
 */
void
cinputs_clear(cinputs *inputs)
CINPUTS_NONNULL(1);

/**
 * If present, untracks an input with the matching id.
 *
 * @param inputs : Input tracker to interact with
 * @param id     : Identifier to match
 */
void
cinputs_pull_id(cinputs *inputs, unsigned int id)
CINPUTS_NONNULL(1);

/** 
 * Untracks an input at the given index. This function has no effects if index is out of bounds.
 *
 * @param inputs : Input tracker to interact with
 * @param index  : Index within the array
 */
void
cinputs_pull_index(cinputs *inputs, size_t index)
CINPUTS_NONNULL(1);

/**
 * Adds in input at the end of the input tracking array. If an input with a matching id already exists within
 * the array, it is pushed to the end of the array and its ptr, x and y details are updated. This function has
 * no effect if the array is full.
 *
 * @param inputs : Input tracker to interact with
 * @param x      : X coordinate
 * @param y      : Y coordinate
 * @param ptr    : Arbitrary pointer to something related to the input
 */
void
cinputs_push(cinputs *inputs, unsigned int id, int x, int y, void *ptr)
CINPUTS_NONNULL(1);

/**
 * Clears errors and puts the input tracker back into an usable state. The only unrecoverable error is
 * CREF_INVALID.
 *
 * @param inputs : Input tracker to interact with
 */
void
cinputs_repair(cinputs *inputs)
CINPUTS_NONNULL(1);

/**
 * Updates the size of input tracker. If the requested size is smaller than the current load, tailing inputs
 * will be pulled.
 *
 * @param inputs     : Input tracker to interact with
 * @param max_inputs : Maximum number of inputs to track at a time. 0 is an illegal value.
 *
 * @error CERR_OVERFLOW : The size of the resulting input tracking array will be > SIZE_MAX
 * @error CERR_INVALID  : Failed memory allocation
 */
void
cinputs_resize(cinputs *inputs, size_t max_inputs)
CINPUTS_NONNULL(1);

/**
 * Sets a new default pointer value to return when cinputs_get() cannot return a proper value.
 *
 * @param inputs : Input tracker to interact with
 * @param ptr    : Pointer 
 */
void
cinputs_set_default_ptr(cinputs *inputs, void *ptr)
CINPUTS_NONNULL(1);

/************************************************************************************************************/
/* PURE METHODS *********************************************************************************************/
/************************************************************************************************************/

/** 
 * Gets the error state.
 *
 * @param inputs : Input tracker to interact with
 * 
 * @return : Error value 
 */
enum cerr
cinputs_error(const cinputs *inputs)
CINPUTS_NONNULL(1)
CINPUTS_PURE;

/** 
 * Tries to find an input with the matching id. If found, true is returned, and if the optional index
 * parameter is not NULL, the array index of the found input will be written into it.
 *
 * @param inputs : Input tracker to interact with
 * @param id     : Identifier to match
 * @param index  : Optional parameter, index of the found input
 *
 * @return     : Id match
 * @return_err : false
 */
bool
cinputs_find(const cinputs *inputs, unsigned int id, size_t *index)
CINPUTS_NONNULL(1);

/** 
 * Gets the input's id at the given index. If index is out of bounds, the default return_err value is
 * returned.
 *
 * @param inputs : Input tracker to interact with
 * @param index : Index within the array
 *
 * @return     : Input
 * @return_err : 0
 */
unsigned int
cinputs_id(const cinputs *inputs, size_t index)
CINPUTS_NONNULL(1)
CINPUTS_PURE;

/** 
 * Gets the total number of different tracked inputs.
 *
 * @param inputs : Input tracker to interact with
 *
 * @return     : Number of tracked inputs
 * @return_err : 0
 */
size_t
cinputs_load(const cinputs *inputs)
CINPUTS_NONNULL(1)
CINPUTS_PURE;

/** 
 * Gets the input's associated pointer at the given index. If index is out of bounds, the default return_err
 * value is returned.
 *
 * @param inputs : Input tracker to interact with
 * @param index : Index within the array
 *
 * @return     : Pointer
 * @return_err : Pointer value set with cinputs_set_default_ptr(). NULL can still be returned if the default
 *               pointer value was not set or if CREF_PLACHOLDER is passed as the ref parameter.
 */
void *
cinputs_ptr(const cinputs *inputs, size_t index)
CINPUTS_NONNULL(1)
CINPUTS_PURE;

/** 
 * Gets the input's X coordinate at the given index. If index is out of bounds, the default return_err value
 * is returned.
 *
 * @param inputs : Input tracker to interact with
 * @param index : Index within the array
 *
 * @return     : Input
 * @return_err : 0
 */
int16_t
cinputs_x(const cinputs *inputs, size_t index)
CINPUTS_NONNULL(1)
CINPUTS_PURE;

/** 
 * Gets the input's Y coordinate at the given index. If index is out of bounds, the default return_err value
 * is returned.
 *
 * @param inputs : Input tracker to interact with
 * @param index : Index within the array
 *
 * @return     : Input
 * @return_err : 0
 */
int16_t
cinputs_y(const cinputs *inputs, size_t index)
CINPUTS_NONNULL(1)
CINPUTS_PURE;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

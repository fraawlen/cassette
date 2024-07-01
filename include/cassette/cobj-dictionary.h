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
	#define CDICT_NONNULL_RETURN __attribute__((returns_nonnull))
	#define CDICT_NONNULL(...)   __attribute__((nonnull (__VA_ARGS__)))
	#define CDICT_PURE           __attribute__((pure))
#else
	#define CDICT_NONNULL_RETURN
	#define CDICT_NONNULL(...)
	#define CDICT_PURE
#endif

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/**
 * Opaque dictionary object instance. It uses internally a hashtable with the FNV1-A hash function and uses
 * linear probing for collision resolution.
 * This object holds an internal error bitfield that can be checked with cdict_error(). Some functions, upon
 * failure, can trigger specific error bits and will exit early without side effects. If the error bitfield is
 * set to anything else than CDICT_OK, any function that takes this object as an argument will return early
 * with no side effects and default return values. It is possible to repair the object to get rid of errors.
 * See cdict_repair() for more details.
 */
typedef struct cdict cdict;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Error types.
 */
enum cdict_err
{
	CDICT_OK        = 0,
	CDICT_INVALID   = 1,
	CDICT_MEMORY    = 1 << 1,
	CDICT_OVERFLOW  = 1 << 2,
	CDICT_BAD_INPUT = 1 << 3,
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/**
 * Create a dictionary instance and deep copy the contents of another dictionary instance into it.
 *
 * @param dict : Dictionary to copy contents from
 *
 * @return     : New dictionary instance
 * @return_err : CDICT_PLACEHOLDER
 */
cdict *
cdict_clone(const cdict *dict)
CDICT_NONNULL_RETURN
CDICT_NONNULL(1);

/**
 * Creates an empty dictionary instance.
 *
 * @return     : New dictionary instance
 * @return_err : CDICT_PLACEHOLDER
 */
cdict *
cdict_create(void)
CDICT_NONNULL_RETURN;

/**
 * Destroys the given dictionary and frees memory.
 *
 * @param dict : Dictionary to interact with
 */
void
cdict_destroy(cdict *dict)
CDICT_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Clears all active slots. Allocated memory is not freed, use cdict_destroy() for that.
 *
 * @param dict : Dictionary to interact with
 */
void
cdict_clear(cdict *dict)
CDICT_NONNULL(1);

/**
 * Clears all active slots of a specific group. Allocated memory is not freed, use cdict_destroy() for that.
 *
 * @param dict  : Dictionary to interact with
 * @param group : Group to match
 */
void
cdict_clear_group(cdict *dict, unsigned int group)
CDICT_NONNULL(1);

/**
 * Deletes the slot that matches the given key and group. This function has no effect if there are no matching
 * slots. Allocated memory is not freed, use cdict_destroy() for that.
 *
 * @param dict  : Dictionary to interact with
 * @param key   : Key to match
 * @param group : Group to match
 */
void
cdict_erase(cdict *dict, const char *key, unsigned int group)
CDICT_NONNULL(1, 2);

/** 
 * Preallocates a set amount of slots to avoid triggering multiple automatic reallocs and rehashes when adding
 * data to the dictionary. To stay under the set maximum load factor (default = 0.6), the actual amount of
 * allocated hashtable slots is slot_number / max_load_factor. This function has no effect if the requested
 * number of slots is smaller than the previously allocated amount.
 *
 * @param dict         : Dictionary to interact with
 * @param slots_number : Number of slots
 *
 * @error CDICT_OVERFLOW : The size of the resulting dictionary will be > SIZE_MAX
 * @error CDICT_MEMORY   : Failed memory allocation
 */
void
cdict_prealloc(cdict *dict, size_t slots_number)
CDICT_NONNULL(1);

/**
 * Sets the maximum load factor. To stay under it, the dictionary may automatically extend its number of
 * allocated slots. Default value = 0.6. Values outside of the [0.0 1.0], 0.0 excluded, are illegal.
 *
 * @param dict        : Dictionary to interact with
 * @param laod_factor : Maximum load factor to set
 *
 * @error CDICT_OVERFLOW  : The size of the resulting dictionary will be > SIZE_MAX
 * @error CDICT_MEMORY    : Failed memory allocation
 * @error CDICT_BAD_INPUT : Illegal load_factor values were given
 */
void
cdict_set_max_load(cdict *dict, double load_factor)
CDICT_NONNULL(1);

/** 
 * Clears errors and puts the dictionary back into an usable state. The only unrecoverable error is
 * CDICT_INVALID.
 *
 * @param dict : Dictionary to interact with
 */
void
cdict_repair(cdict *dict)
CDICT_NONNULL(1);

/**
 * Activates a slot in the dictionary's hashtable. The given key, group, and values will be associated with
 * that slot. If a slot with a matching key and group already exists, this function will only overwrite its
 * associated value. The dictionary can automatically extend the total number of allocated slots to stay under
 * its maximum load factor (default = 0.6).
 *
 * @param dict  : Dictionary to interact with
 * @param key   : Key to match
 * @param group : Group to match
 * @param value : Value to associate with the slot
 *
 * @error CDICT_OVERFLOW : The size of the resulting dictionary will be > SIZE_MAX
 * @error CDICT_MEMORY   : Failed memory allocation
 */
void
cdict_write(cdict *dict, const char *key, unsigned int group, size_t value)
CDICT_NONNULL(1, 2);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Gets the error state.
 *
 * @param dict : Dictionary to interact with
 *
 * @return : Error bitfield
 */
enum cdict_err
cdict_error(const cdict *dict)
CDICT_NONNULL(1)
CDICT_PURE;

/**
 * Tries to find a slot that matches the given key and group. If found, true is returned, and if the optional
 * value parameter is not NULL, the associated value of the found slot will be written into it.
 *
 * @param dict  : Dictionary to interact with
 * @param key   : Key to match
 * @param group : Group to match
 * @param value : Optional parameter, value associated to the found slot
 *
 * @return     : Slot match
 * @return_err : false
 */
bool
cdict_find(const cdict *dict, const char *key, unsigned int group, size_t *value)
CDICT_NONNULL(1, 2);

/**
 * Gets the number of active slots.
 *
 * @param dict : Dictionary to interact with
 *
 * @return     : Number of slots
 * @return_err : 0
 */
size_t
cdict_load(const cdict *dict)
CDICT_NONNULL(1)
CDICT_PURE;

/**
 * Gets a ratio of the number of active slots by the number of allocated slots.
 *
 * @param dict : Dictionary to interact with
 *
 * @return     : 0.0 to 1.0 ratio
 * @return_err : 0.0
 */
double
cdict_load_factor(const cdict *dict)
CDICT_NONNULL(1)
CDICT_PURE;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/**
 * A macro that gives uninitialized dictionaries a non-NULL value that is safe to use with the dictionary's
 * related functions. However, any function called with a handle set to this value will return early and
 * without any side effects.
 */
#define CDICT_PLACEHOLDER &cdict_placeholder_instance

/**
 * Global dictionary instance with the error state set to CDICT_INVALID. This instance is only made available
 * to allow the static initialization of dictionary pointers with the macro CDICT_PLACEHOLDER.
 */
extern cdict cdict_placeholder_instance;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

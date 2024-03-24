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

#ifndef DU_DICTIONARY_H
#define DU_DICTIONARY_H

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

/**
 * Slot occupation states. DU_DICTIONARY_DELETED value stands for tombstones.
 */
typedef enum {
	DU_DICTIONARY_UNUSED   = 0,
	DU_DICTIONARY_DELETED  = 1,
	DU_DICTIONARY_OCCUPIED = 2,
} du_dictionary_usage_t;

/**
 * Slot in the dictionary.
 *
 * @param hash  : hash of the key used to get the slot
 * @param value : stored value
 * @param group : group the slot's value is part of, used together with the key for matching values
 * @param used  : set to true if the slot is used, false if it's free
 */
typedef struct {
	uint32_t hash;
	int64_t value;
	int group;
	du_dictionary_usage_t usage;
} du_dictionary_slot_t;

/**
 * Dictionary struct, with its slots held in an array. Key values are hashed with the 32-bit fnv1a
 * algorithm and collisions are handled using linear probing. The dictionary's size is doubled everytime its
 * reaches a load factor of max_load. However, to minimize resizes, it is recommended to initialise it with
 * the appropriate amount of slots. n <= n_alloc / max_load.
 * If status is not set to DU_STATUS_SUCCESS all handler functions will have no effect with the exception of
 * du_dictionary_reset() and du_dictionary_init().
 *
 * @param slots    : slot array
 * @param n        : number of occupied slots
 * @param n_alloc  : total number of allocated slots
 * @param max_load : maximum load factor to maintain, forbidden values : <= 0.0
 * @param status   : error state
 */
typedef struct {
	du_dictionary_slot_t *slots;
	uint32_t n;
	uint32_t n_alloc;
	du_ratio_t max_load;
	du_status_t status;
} du_dictionary_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Pre-allocate memory to the dictionary and set its variables appropriately. Allocated slots within the
 * dictionary are all initialised to 0. This function is optional because a dictionary can allocate memory
 * automatically as needed (specificaly, everytime it reaches a load factor of dict->max_load, to stay under
 * it). The actual amount of slots allocated is therefore n_alloc / max_load, with max_load <= 1.0 and
 * >= 0.0. If n_alloc = 0, no memory is allocated but the structure will still be considered to have been
 * initialised.
 * In case of error, dict->status will be set to DU_STATUS_FAILURE. It's set to DU_STATUS_SUCCESS otherwhise.
 *
 * @param dict     : dictionary to init
 * @param n_alloc  : initial desired size (actual allocated size also takes in account the load factor)
 * @param max_load : load factor to stay under, forbidden values : <= 0.0
 */
void du_dictionary_init(du_dictionary_t *dict, uint32_t n_alloc, du_ratio_t max_load);

/**
 * Allocated memory is freed and the structure will be put in an unitialised state with dict->status set to
 * DU_STATUS_NOT_INIT. The given structure itself is not freed, and may require an explicit free operation.
 *
 * @param dict : dictionary to reset
 */
void du_dictionary_reset(du_dictionary_t *dict);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Erases all saved values within the given dictionary. However, unlike du_dictionary_reset(), allocated
 * memory is not freed nor reallocated, thus maintaining the size of the dictionary.
 *
 * @param dict : dictionary to clear
 */
void du_dictionary_clear(du_dictionary_t *dict);

/**
 * Erases all saved values of a given group within the given dictionary.
 *
 * @param dict  : dictionary to search through
 * @param group : group to clear
 */
void du_dictionary_erase_group(du_dictionary_t *dict, int group);

/**
 * Finds the slot matching key + group and frees it. If the key + group combo does not exist this function
 * has no effect.
 *
 * @param dict  : dictionary to search through
 * @param key   : string key to use
 * @param group : group to match
 */
void du_dictionary_erase_value(du_dictionary_t *dict, const char *key, int group);

/**
 * Finds a suitable slot for a matching key + group and fills it with the given group, computed hash and value.
 * If the key + group combo already exists its value will be overwritten. A NULL key is allowed, but is
 * equivalent to a "" key.
 * The given dictionary will be expanded automatically to maintain a load factor < dict->max_load.
 * In case of error, dict->status will be set to DU_STATUS_FAILURE.
 *
 * @param dict  : dictionary to search through
 * @param key   : string key to use
 * @param group : group to match
 * @param value : value to set
 */
void du_dictionary_set_value(du_dictionary_t *dict, const char *key, int group, int64_t value);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Finds a suitable slot for a matching key + group. A NULL key is allowed, but is equivalent to a "" key.
 *
 * @param dict  : dictionary to search through
 * @param key   : string key to use
 * @param group : group to match
 * @param found : optional, pointed value is set to slot's value if there is a match
 *
 * @return : true if there is a matching slot, false otherwhise
 */
bool du_dictionary_find_value(const du_dictionary_t *dict, const char *key, int group, int64_t *value);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DU_DICTIONARY_H */

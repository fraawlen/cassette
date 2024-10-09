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

#ifndef DG_CORE_HASHTABLE_H
#define DG_CORE_HASHTABLE_H

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define DG_CORE_HASHMAP_EMPTY (dg_core_hashtable_t){.slots = NULL, .n = 0, .n_alloc = 0}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Slot in the hashtable.
 *
 * @param hash  : hash of the key used to get the slot
 * @param used  : set to true if the slot currently used, false if its free
 * @param group : group the slot's value is part of, used together with the key for matching values
 * @param val   : value of the slot
 */
typedef struct {
	uint32_t hash;
	bool used;
	int group;
	size_t val;
} dg_core_hashtable_slot_t;

/**
 * Full hashtable struct, with its slots held in an array. Key values are hashed with the 32-bit fnv1a
 * algorithm and collisions are handled using linear probing. The hashtable's size is doubled everytime its
 * reaches a load factor of 60%. However, to minize resizes, which involve copying and rehasing every slots,
 * it is recommended to initialise it with the appropriate amount of slots. n <= n_alloc.
 *
 * @param slots   : slot array
 * @param n       : number of occupied slots
 * @param n_alloc : total number of allocated slots
 */
typedef struct {
	dg_core_hashtable_slot_t *slots;
	uint32_t n;
	uint32_t n_alloc;
} dg_core_hashtable_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Preallocate memory to the hashtable and set its variables appropriately. Allocated memory in the hashtable is
 * initialised to 0. This function is recommended but optional to operate dg_core_hashtable_t structs because a
 * hashtable can allocate memory automatically when needed (specificaly, everytime it reaches a load factor of
 * 0.6 to stay under it). The actual amount of slots allocated is n * 1.67.
 * If n = 0, no memory is allocated and *hm is instead set to DG_CORE_HASHMAP_EMPTY.
 *
 * @param hm : hashtable to init
 * @param n  : initial desired size (actual allocated size also takes in account the load factor)
 *
 * @return true if init was successfull, false otherwhise (errno is set)
 *
 * @error DG_CORE_ERRNO_HASHMAP : memory allocation failed
 */
bool dg_core_hashtable_init(dg_core_hashtable_t *hm, uint32_t n_alloc);

/**
 * Resets a given hashtable and free memory.
 *
 * @param : hashtable to reset
 */
void dg_core_hashtable_reset(dg_core_hashtable_t *hm);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Finds a suitable slot for a matching key + group and returns its value.
 * A NULL key is allowed but it is equivalent to a "" key.
 *
 * @param hm    : hashtable to search through
 * @param key   : string key to use
 * @param group : group to match
 * @param found : optional, pointed value is set to true if a matching slot was found, false otherwhise.
 *
 * @return : value of a matching slot, defaults to 0 if none is found
 */
size_t dg_core_hashtable_get_value(dg_core_hashtable_t *hm, const char *key, int group, bool *found);

/**
 * Finds a suitable slot for a mathcing key + group and fills it with the given group, hash and value.
 * The given hashtable will be expanded automatically to maintaint a load factor < 0.6. If the key + group
 * combo already exists its value will be overwritten.
 * A NULL key is allowed but it is equivalent to a "" key.
 *
 * @param hm    : hashtable to search through
 * @param key   : string key to use
 * @param group : group to match
 * @param val   : value to set
 *
 * @return : true if a slot has been selected and set, false in case of failure (hashtable resizing failed -
 *           errno is set)
 *
 * @error DG_CORE_ERRNO_HASHMAP : failure to allocate memory for a new value to fit in
 */
bool dg_core_hashtable_set_value(dg_core_hashtable_t *hm, const char *key, int group, size_t val);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DG_CORE_HASHTABLE_H */

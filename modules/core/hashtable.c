/**
 * Copyright © 2024 Frawwlen <fraawlen@posteo.net>
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

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "errno.h"
#include "hashtable.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define _LOAD_FACTOR 0.6

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static bool     _expand (dg_core_hashtable_t *hm);
static uint32_t _hash   (const char *key);

static dg_core_hashtable_slot_t *_find_slot (dg_core_hashtable_t *hm, uint32_t hash, int group);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

size_t
dg_core_hashtable_get_value(dg_core_hashtable_t *hm, const char *key, int group, bool *found)
{
	assert(hm);

	dg_core_hashtable_slot_t *slot = _find_slot(hm, _hash(key), group);
	
	if (found) {
		*found = slot->used;
	}

	return slot->val;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
dg_core_hashtable_init(dg_core_hashtable_t *hm, uint32_t n_alloc)
{
	assert(hm);

	const size_t m = n_alloc / _LOAD_FACTOR;

	if (m == 0) {
		*hm = DG_CORE_HASHMAP_EMPTY;
		return true;
	}

	hm->slots = calloc(m, sizeof(dg_core_hashtable_slot_t));
	if (!hm->slots) {
		dg_core_errno_set(DG_CORE_ERRNO_HASHTABLE);
		return false;
	}

	hm->n = 0;
	hm->n_alloc = m;

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_hashtable_reset(dg_core_hashtable_t *hm)
{
	assert(hm);

	free(hm->slots);

	*hm = DG_CORE_HASHMAP_EMPTY;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
dg_core_hashtable_set_value(dg_core_hashtable_t *hm, const char *key, int group, size_t val)
{
	assert(hm);

	if (hm->n / _LOAD_FACTOR >= hm->n_alloc &&!_expand(hm)) {
		return false;
	}

	const uint32_t hash = _hash(key);
	dg_core_hashtable_slot_t *slot = _find_slot(hm, hash, group);	

	if (!slot->used) {
		slot->group = group;
		slot->hash = hash;
		slot->used = true;
		hm->n++;
	}

	slot->val = val;

	return true;
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static bool
_expand(dg_core_hashtable_t *hm)
{
	dg_core_hashtable_t hm_new;
	dg_core_hashtable_slot_t *slot;

	if (!dg_core_hashtable_init(&hm_new, hm->n_alloc > 0 ? hm->n_alloc * 2 : 1)) {
		dg_core_errno_set(DG_CORE_ERRNO_HASHTABLE);
		return false;
	}

	hm_new.n = hm->n;

	for (uint32_t i = 0; i < hm->n_alloc; i++) {
		slot = &hm->slots[i];
		if (slot->used) {
			*_find_slot(&hm_new, slot->hash, slot->group) = *slot;
		}
	}

	dg_core_hashtable_reset(hm);
	*hm = hm_new;

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static dg_core_hashtable_slot_t *
_find_slot(dg_core_hashtable_t *hm, uint32_t hash, int group)
{
	uint32_t i = hash % hm->n_alloc;
	dg_core_hashtable_slot_t *slot = &hm->slots[i];

	while (slot->used && (slot->group != group || slot->hash != hash)) {
		i = i >= hm->n_alloc - 1 ? 0 : i + 1;
		slot = &hm->slots[i];
	}

	return slot;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static uint32_t
_hash(const char *key)
{
	/* fnv1a algorithm */

	uint32_t h = 2166136261;

	if (key) {
		for (size_t i = 0; i < strlen(key); i++) {
			h ^= key[i];
			h *= 16777619;
		}
	}

	return h;
}

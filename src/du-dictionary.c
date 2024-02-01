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

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "du.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static bool                  _extend    (du_dictionary_t *dict);
static du_dictionary_slot_t *_find_slot (const du_dictionary_t *dict, uint32_t hash, int group);
static uint32_t              _hash      (const char *str);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
du_dictionary_erase_value(du_dictionary_t *dict, const char *key, int group)
{
	assert(dict);
	du_status_test(dict->status, return);

	_find_slot(dict, _hash(key), group)->used = false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
du_dictionary_find_value(const du_dictionary_t *dict, const char *key, int group, int64_t *value)
{
	assert(dict);
	du_status_test(dict->status, return false);

	du_dictionary_slot_t *slot = _find_slot(dict, _hash(key), group);
	
	if (value && slot->used) {
		*value = slot->value;
	}

	return slot->used;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_dictionary_init(du_dictionary_t *dict, uint32_t n_alloc, du_ratio_t max_load)
{
	assert(dict);

	const uint32_t m = n_alloc / du_ratio_bind(&max_load);

	if (m == 0) {
		*dict = (du_dictionary_t)DU_DICTIONARY_EMPTY;
		return;
	}

	dict->slots = calloc(m, sizeof(du_dictionary_slot_t));
	dict->n = 0;
	dict->n_alloc = m;
	dict->max_load = max_load;
	dict->status = dict->slots ? DU_STATUS_SUCCESS : DU_STATUS_FAILURE;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_dictionary_reset(du_dictionary_t *dict)
{
	assert(dict);

	free(dict->slots);	
	*dict = (du_dictionary_t)DU_DICTIONARY_EMPTY;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_dictionary_set_value(du_dictionary_t *dict, const char *key, int group, int64_t value)
{
	assert(dict);
	du_status_test(dict->status, return);

	if (dict->n / dict->max_load >= dict->n_alloc && !_extend(dict)) {
		return;
	}

	const uint32_t hash = _hash(key);
	du_dictionary_slot_t *slot = _find_slot(dict, hash, group);

	slot->value = value;
	if (!slot->used) {
		slot->group = group;
		slot->hash = hash;
		slot->used = true;
		dict->n++;
	}
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static bool
_extend(du_dictionary_t *dict)
{
	du_dictionary_t dict_new;
	du_dictionary_slot_t *slot;

	du_dictionary_init(&dict_new, dict->n_alloc > 0 ? dict->n_alloc * 2 : 1, dict->max_load);
	du_status_test(dict->status, return false);

	for (size_t i = 0; i < dict->n_alloc; i++) {
		slot = &dict->slots[i];
		if (slot->used) {
			*_find_slot(&dict_new, slot->hash, slot->group) = *slot;
		}
	}

	dict_new.n = dict->n;
	du_dictionary_reset(dict);
	*dict = dict_new;

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static du_dictionary_slot_t *
_find_slot(const du_dictionary_t *dict, uint32_t hash, int group)
{
	size_t i = hash % dict->n_alloc;
	du_dictionary_slot_t *slot = &dict->slots[i];

	while (slot->used && (slot->group != group || slot->hash != hash)) {
		i = i >= dict->n_alloc - 1 ? 0 : i + 1;
		slot = &dict->slots[i];
	}

	return slot;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static uint32_t 
_hash(const char *str)
{
	uint32_t h = 2166136261;

	if (str) {
		for (size_t i = 0; i < strlen(str); i++) {
			h ^= str[i];
			h *= 16777619;
		}
	}

	return h;
}

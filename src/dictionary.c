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

#include <derelict/du.h>

#include "safe.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

enum _state_t
{
	_UNUSED   = 0,
	_DELETED  = 1,
	_OCCUPIED = 2,
};

typedef enum _state_t _state_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct _slot_t
{
	uint64_t hash;
	int64_t value;
	unsigned int group;
	_state_t state;
};

typedef struct _slot_t _slot_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct _dictionary_t
{
	_slot_t *slots;
	size_t n;
	size_t n_alloc;
	double max_load;
	bool failed;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static _slot_t *_find_slot (const du_dictionary_t *dict, uint64_t hash, _state_t state_cut_off);
static uint64_t _hash      (const char *str, unsigned int group);
static bool     _resize    (du_dictionary_t *dict, size_t n, size_t a, size_t b);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static du_dictionary_t _err_dict = 
{
	.slots    = NULL,
	.n        = 0,
	.n_alloc  = 0,
	.max_load = 1.0,
	.failed   = true,
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
du_dictionary_clear(du_dictionary_t *dict)
{
	assert(dict);

	if (dict->failed)
	{
		return;
	}

	memset(dict->slots, 0, dict->n_alloc * sizeof(_slot_t));
	dict->n = 0;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_dictionary_clear_group(du_dictionary_t *dict, unsigned int group)
{
	assert(dict);

	if (dict->failed)
	{
		return;
	}

	if (dict->n == 0)
	{
		return;
	}

	for (uint32_t i = 0; i < dict->n_alloc; i++)
	{
		if (dict->slots[i].state == _OCCUPIED && dict->slots[i].group == group)
		{
			dict->slots[i].state = _DELETED;
			dict->n--;
		}
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

du_dictionary_t *
du_dictionary_create(size_t n_alloc, double max_load)
{
	du_dictionary_t *dict;

	assert(max_load > 0.0 && max_load <= 1.0);

	if (n_alloc > SIZE_MAX * max_load)
	{
		return &_err_dict;
	}

	if (!(dict = malloc(sizeof(du_dictionary_t))))
	{
		return &_err_dict;
	}

	dict->slots    = NULL;
	dict->n        = 0;
	dict->n_alloc  = 0;
	dict->max_load = max_load;
	dict->failed   = false;

	_resize(dict, n_alloc / max_load, 1, 0);

	return dict;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_dictionary_destroy(du_dictionary_t **dict)
{
	assert(dict && *dict);

	if (*dict == &_err_dict)
	{
		return;
	}

	free((*dict)->slots);
	free(*dict);

	*dict = &_err_dict;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_dictionary_erase(du_dictionary_t *dict, const char *key, unsigned int group)
{
	_slot_t *slot;

	assert(dict);

	if (dict->failed)
	{
		return;
	}

	if (dict->n == 0)
	{
		return;
	}

	if ((slot = _find_slot(dict, _hash(key, group), _UNUSED)) && slot->state == _OCCUPIED)
	{
		slot->state = _DELETED;
		dict->n--;

	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
du_dictionary_find(const du_dictionary_t *dict, const char *key, unsigned int group, size_t *value)
{
	_slot_t *slot;

	assert(dict);

	if (dict->failed)
	{
		return false;
	}

	if (dict->n == 0)
	{
		return false;
	}

	if (!(slot = _find_slot(dict, _hash(key, group), _UNUSED)) || slot->state != _OCCUPIED)
	{
		return false;
	}

	if (value)
	{
		*value = slot->value;
	}

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
du_dictionary_get_alloc_size(const du_dictionary_t *dict)
{
	assert(dict);

	if (dict->failed)
	{
		return 0;
	}

	return dict->n_alloc;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
du_dictionary_get_load(const du_dictionary_t *dict)
{
	assert(dict);

	if (dict->failed)
	{
		return 0;
	}
	
	return dict->n;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

double
du_dictionary_get_load_factor(const du_dictionary_t *dict)
{
	assert(dict);

	if (dict->failed)
	{
		return 0.0;
	}

	return (double)dict->n / dict->n_alloc;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
du_dictionary_has_failed(const du_dictionary_t *dict)
{
	assert(dict);

	return dict->failed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_dictionary_write(du_dictionary_t *dict, const char *key, unsigned int group, size_t value)
{
	_slot_t *slot;
	_slot_t *slot_2;

	uint64_t hash;

	assert(dict);

	if (dict->failed)
	{
		return;
	}

	if (dict->n >= dict->n_alloc * dict->max_load && !_resize(dict, dict->n_alloc, 2, 1)) {
		return;
	}

	if (!(slot = _find_slot(dict, (hash = _hash(key, group)), _DELETED)))
	{
		dict->failed = true;
		return;
	}

	switch (slot->state) {

		case _DELETED:
			if (slot->hash != hash
			    && (slot_2 = _find_slot(dict, hash, _UNUSED))
			    && slot_2->state == _OCCUPIED)
			{
				slot_2->state = _DELETED;
				dict->n--;
			}
			/* fallthrough */

		case _UNUSED:
			slot->hash  = hash;
			slot->group = group;
			slot->state = _OCCUPIED;
			dict->n++;
			/* fallthrough */

		case _OCCUPIED:
			slot->value = value;
			break;
	}
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/


static _slot_t *
_find_slot(const du_dictionary_t *dict, uint64_t hash, _state_t state_cut_off)
{
	_slot_t *slot;

	uint64_t i0;
	uint64_t i;

	i0   = hash % dict->n_alloc;
	i    = i0;
	slot = dict->slots + i;

	while (state_cut_off < slot->state && slot->hash != hash)
	{
		if (++i >= dict->n_alloc)
		{
			i = 0;
		}
		if (i == i0)
		{
			return NULL;
		}
		slot = &dict->slots[i];
	}

	return slot;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static uint64_t 
_hash(const char *str, unsigned int group)
{
	const uint64_t offset = 14695981039346656037ULL;
	const uint64_t prime  = 1099511628211ULL;

	uint64_t h;

	h = offset;

	for (size_t i = 0; i < sizeof(group); i++)
	{
		h = (h ^ (group & (0xFF << i))) * prime;
	}

	if (str)
	{
		for (size_t i = 0; str[i] != '\0'; i++)
		{
			h = (h ^ str[i]) * prime;
		}
	}

	return h;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
_resize(du_dictionary_t *dict, size_t n, size_t a, size_t b)
{
	_slot_t *tmp;
	_slot_t *tmp_2;

	size_t n_2;
	bool safe = true;

	/* test for overflow */

	safe &= du_safe_mult(&n,   n, a);
	safe &= du_safe_add (&n,   n, b);
	safe &= du_safe_mult(NULL, n, sizeof(_slot_t));

	if (!safe)
	{
		dict->failed = true;
		return false;
	}

	if (n <= dict->n_alloc)
	{
		return true;
	}

	/* create new slot array */

	if (!(tmp = calloc(n, sizeof(_slot_t))))
	{
		dict->failed = true;
		return false;
	}

	tmp_2 = dict->slots;
	n_2   = dict->n_alloc;

	dict->slots   = tmp;
	dict->n_alloc = n;

	/* move old values to new slots */

	for (size_t i = 0; i < n_2; i++)
	{
		if (tmp_2[i].state == _OCCUPIED)
		{
			*_find_slot(dict, tmp_2[i].hash, _UNUSED) = tmp_2[i];
		}
	}

	/* end */

	free(tmp_2);

	return true;
}

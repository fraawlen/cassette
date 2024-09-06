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

#include <cassette/cobj.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "safe.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define HASH_OFFSET 14695981039346656037ULL
#define HASH_PRIME  1099511628211ULL

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum state
{
	UNUSED  = 0,
	DELETED = 1,
	ACTIVE  = 2,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct slot
{
	uint64_t hash;
	size_t value;
	size_t group;
	enum state state;
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct cdict
{
	struct slot *slots;
	size_t n;
	size_t n_alloc;
	double max_load;
	enum cerr err;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static struct slot *find     (const cdict *, uint64_t, enum state) CDICT_NONNULL(1) CDICT_PURE;
static uint64_t     get_hash (const char *, size_t)                CDICT_NONNULL(1) CDICT_PURE;
static bool         grow     (cdict *, size_t)                     CDICT_NONNULL(1);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

cdict cdict_placeholder_instance = 
{
	.slots    = NULL,
	.n        = 0,
	.n_alloc  = 0,
	.max_load = 1.0,
	.err      = CERR_INVALID,
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cdict_clear(cdict *dict)
{
	if (dict->err)
	{
		return;
	}

	memset(dict->slots, 0, dict->n_alloc * sizeof(struct slot));
	dict->n = 0;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cdict_clear_group(cdict *dict, size_t group)
{
	if (dict->err)
	{
		return;
	}

	for (size_t i = 0; i < dict->n_alloc; i++)
	{
		if (dict->slots[i].state == ACTIVE && dict->slots[i].group == group)
		{
			dict->slots[i].state = DELETED;
			dict->n--;
		}
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cdict *
cdict_clone(const cdict *dict)
{
	cdict *dict_new;

	if (dict->err || !(dict_new = malloc(sizeof(cdict))))
	{
		return CDICT_PLACEHOLDER;
	}

	if (!(dict_new->slots = malloc(dict->n_alloc * sizeof(struct slot))))
	{
		free(dict_new);
		return CDICT_PLACEHOLDER;
	}

	memcpy(dict_new->slots, dict->slots, dict->n_alloc * sizeof(struct slot));

	dict_new->n        = dict->n;
	dict_new->n_alloc  = dict->n_alloc;
	dict_new->max_load = dict->max_load;
	dict_new->err      = CERR_NONE;

	return dict_new;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cdict *
cdict_create(void)
{
	cdict *dict;

	if (!(dict = malloc(sizeof(cdict))))
	{
		return CDICT_PLACEHOLDER;
	}

	if (!(dict->slots = calloc(1, sizeof(struct slot))))
	{
		free(dict);
		return CDICT_PLACEHOLDER;
	}

	dict->n        = 0;
	dict->n_alloc  = 1;
	dict->max_load = 0.6;
	dict->err      = CERR_NONE;

	return dict;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cdict_destroy(cdict *dict)
{
	if (dict == CDICT_PLACEHOLDER)
	{
		return;
	}

	free(dict->slots);
	free(dict);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cdict_erase(cdict *dict, const char *key, size_t group)
{
	struct slot *slot;

	if (dict->err)
	{
		return;
	}

	if ((slot = find(dict, get_hash(key, group), UNUSED)) && slot->state == ACTIVE)
	{
		slot->state = DELETED;
		dict->n--;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cerr
cdict_error(const cdict *dict)
{
	return dict->err;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cdict_find(const cdict *dict, const char *key, size_t group, size_t *value)
{
	struct slot *slot;

	if (dict->err || !(slot = find(dict, get_hash(key, group), UNUSED)) || slot->state != ACTIVE)
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
cdict_load(const cdict *dict)
{
	if (dict->err)
	{
		return 0;
	}

	return dict->n;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

double
cdict_load_factor(const cdict *dict)
{
	if (dict->err)
	{
		return 0.0;
	}

	return (double)dict->n / dict->n_alloc;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cdict_prealloc(cdict *dict, size_t slots_number)
{
	if (dict->err)
	{
		return;
	}

	if (slots_number > SIZE_MAX * dict->max_load)
	{
		dict->err = CERR_OVERFLOW;
		return;
	}

	grow(dict, slots_number / dict->max_load);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cdict_repair(cdict *dict)
{
	if (dict->err != CERR_INVALID)
	{
		dict->err = CERR_NONE;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cdict_set_max_load(cdict *dict, double load_factor)
{
	if (dict->err)
	{
		return;
	}

	if (load_factor <= 0.0 || load_factor > 1.0)
	{
		dict->err = CERR_PARAM;
		return;
	}

	if (dict->n > SIZE_MAX * load_factor)
	{
		dict->err = CERR_OVERFLOW;
		return;
	}

	if (grow(dict, dict->n / load_factor))
	{
		dict->max_load = load_factor;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cdict_write(cdict *dict, const char *key, size_t group, size_t value)
{
	struct slot *slot;
	struct slot *slot_2;
	uint64_t hash;

	if (dict->err)
	{
		return;
	}

	if (dict->n >= dict->n_alloc * dict->max_load)
	{
		if (!safe_mul(NULL, dict->n_alloc, 2))
		{
			dict->err = CERR_OVERFLOW;
			return;
		}
		if (!grow(dict, dict->n_alloc * 2))
		{
			return;
		}
	}

	hash = get_hash(key, group);
	slot = find(dict, hash, DELETED);

	switch (slot->state)
	{
		case DELETED:
			if ((slot_2 = find(dict, hash, UNUSED)) && slot_2->state == ACTIVE)
			{
				slot_2->state = DELETED;
				dict->n--;
			}
			/* fallthrough */

		case UNUSED:
			slot->hash  = hash;
			slot->group = group;
			slot->state = ACTIVE;
			dict->n++;
			/* fallthrough */

		case ACTIVE:
			slot->value = value;
			break;
	}
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static struct slot *
find(const cdict *dict, uint64_t hash, enum state state_cutoff)
{
	struct slot *slot;
	uint64_t i0;
	uint64_t i;

	i0   = hash % dict->n_alloc;
	i    = i0;
	slot = dict->slots + i;

	while (state_cutoff < slot->state && slot->hash != hash)
	{
		if (++i >= dict->n_alloc)
		{
			i = 0;
		}
		if (i == i0)
		{
			return NULL;
		}
		slot = dict->slots + i;
	}

	return slot;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static uint64_t
get_hash(const char *str, size_t group)
{
	uint64_t h = HASH_OFFSET;

	for (size_t i = 0; i < sizeof(group); i++)
	{
		h = (h ^ (group & (0xFF << i))) * HASH_PRIME;
	}

	for (size_t i = 0; str[i] != '\0'; i++)
	{
		h = (h ^ str[i]) * HASH_PRIME;
	}

	return h;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
grow(cdict *dict, size_t n)
{
	struct slot *tmp;
	struct slot *tmp_2;
	size_t n_2;

	if (n <= dict->n_alloc)
	{
		return true;
	}

	if (!safe_mul(NULL, n, sizeof(struct slot)))
	{
		dict->err = CERR_OVERFLOW;
		return false;
	}

	if (!(tmp = calloc(n, sizeof(struct slot))))
	{
		dict->err = CERR_MEMORY;
		return false;
	}

	tmp_2 = dict->slots;
	n_2   = dict->n_alloc;

	dict->n_alloc = n;
	dict->slots   = tmp;

	for (size_t i = 0; i < n_2; i++)
	{
		if (tmp_2[i].state == ACTIVE)
		{
			*find(dict, tmp_2[i].hash, UNUSED) = tmp_2[i];
		}
	}

	free(tmp_2);

	return true;
}

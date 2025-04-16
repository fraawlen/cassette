/**
 * Copyright © 2024-2025 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette library.
 *
 * This library is free software; you can redistribute it and/or modify it either under the terms of the GNU
 * Lesser General Public License as published by the Free Software Foundation; either version 3.0 of the
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
#include <math.h>
#include <stdckdint.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define GUARD(OBJ, ...) if (!OBJ || cerr_critical(OBJ->err)) { return __VA_OPT__(__VA_ARGS__); }

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

static struct slot *find     (const cdict *, uint64_t, enum state);
static uint64_t     get_hash (const char *, size_t);
static bool         grow     (cdict *, size_t);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cdict_clear(cdict *dict)
{
	GUARD(dict);

	memset(dict->slots, 0, dict->n_alloc * sizeof(struct slot));
	dict->n = 0;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cdict_clear_group(cdict *dict, size_t group)
{
	GUARD(dict);

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

void
cdict_clear_warnings(cdict *dict)
{
	GUARD(dict);

	cerr_clear_warnings(&dict->err);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cdict *
cdict_clone(const cdict *dict)
{
	GUARD(dict, nullptr);

	cdict *dict_new;

	if (!(dict_new = malloc(sizeof(cdict))))
	{
		return nullptr;
	}

	if (!(dict_new->slots = malloc(dict->n_alloc * sizeof(struct slot))))
	{
		free(dict_new);
		return nullptr;
	}

	memcpy(dict_new->slots, dict->slots, dict->n_alloc * sizeof(struct slot));

	dict_new->n        = dict->n;
	dict_new->n_alloc  = dict->n_alloc;
	dict_new->max_load = dict->max_load;
	dict_new->err      = dict->err;

	return dict_new;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cdict *
cdict_create(void)
{
	cdict *dict;

	if (!(dict = malloc(sizeof(cdict))))
	{
		return nullptr;
	}

	if (!(dict->slots = calloc(1, sizeof(struct slot))))
	{
		free(dict);
		return nullptr;
	}

	dict->n        = 0;
	dict->n_alloc  = 1;
	dict->max_load = 0.6;
	dict->err      = CERR_NONE;

	return dict;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

nullptr_t
cdict_destroy(cdict *dict)
{
	if (dict)
	{
		free(dict->slots);
		free(dict);
	}

	return nullptr;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cdict_erase(cdict *dict, const char *key, size_t group)
{
	GUARD(dict);

	struct slot *slot = find(dict, get_hash(key, group), UNUSED);

	if (slot && slot->state == ACTIVE)
	{
		slot->state = DELETED;
		dict->n--;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cerr
cdict_error(const cdict *dict)
{
	return dict ? dict->err : CERR_INVALID;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cdict_find(const cdict *dict, const char *key, size_t group, size_t *value)
{
	GUARD(dict, false);

	struct slot *slot = find(dict, get_hash(key, group), UNUSED);

	if (!slot || slot->state != ACTIVE)
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
	GUARD(dict, 0);

	return dict->n;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

double
cdict_load_factor(const cdict *dict)
{
	GUARD(dict, 0.0);

	return (double)dict->n / dict->n_alloc;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cdict_prealloc(cdict *dict, size_t slots_number)
{
	GUARD(dict);

	if (slots_number > (SIZE_MAX - 1) * dict->max_load)
	{
		cerr_set(&dict->err, CERR_OVERFLOW);
		return;
	}

	grow(dict, slots_number / dict->max_load + 1);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cdict_set_max_load(cdict *dict, double load_factor)
{
	GUARD(dict);

	if (isnan(load_factor) || isinf(load_factor) || load_factor <= 0.0 || load_factor > 1.0)
	{
		cerr_set(&dict->err, CERR_PARAM);
		return;
	}

	if (dict->n > (SIZE_MAX - 1) * load_factor)
	{
		cerr_set(&dict->err, CERR_OVERFLOW);
		return;
	}

	grow(dict, dict->n / (dict->max_load = load_factor) + 1);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cdict_write(cdict *dict, const char *key, size_t group, size_t value)
{
	GUARD(dict);

	/* increase dict size if needed */

	size_t n;

	if (dict->n >= dict->n_alloc * dict->max_load)
	{
		if (ckd_mul(&n, dict->n_alloc, 2))
		{
			cerr_set(&dict->err, CERR_OVERFLOW);
			return;
		}
		if (!grow(dict, n))
		{
			return;
		}
	}

	/* write new entry */

	uint64_t     hash = get_hash(key, group);
	struct slot *slot = find(dict, hash, DELETED);
	struct slot *slot_2;

	if (!slot)
	{
		return;
	}

	switch (slot->state)
	{
		case DELETED:
			if ((slot_2 = find(dict, hash, UNUSED)) && slot_2->state == ACTIVE)
			{
				slot_2->state = DELETED;
				dict->n--;
			}
			[[fallthrough]];

		case UNUSED:
			slot->hash  = hash;
			slot->group = group;
			slot->state = ACTIVE;
			dict->n++;
			[[fallthrough]];

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
	uint64_t i0       = hash % dict->n_alloc;
	uint64_t i        = i0;
	struct slot *slot = dict->slots + i;

	while (state_cutoff < slot->state && slot->hash != hash)
	{
		if (++i >= dict->n_alloc)
		{
			i = 0;
		}
		if (i == i0)
		{
			return nullptr;
		}
		slot = dict->slots + i;
	}

	return slot;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static uint64_t
get_hash(const char *str, size_t group)
{
	constexpr uint64_t offset = 14695981039346656037ULL;
	constexpr uint64_t prime  = 1099511628211ULL;
	          uint64_t h      = offset;

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

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wanalyzer-imprecise-fp-arithmetic"

static bool
grow(cdict *dict, size_t n)
{
	struct slot *tmp;
	struct slot *tmp_2;
	size_t dummy;
	size_t n_2;

	if (n <= dict->n_alloc)
	{
		return true;
	}

	if (ckd_mul(&dummy, n, sizeof(struct slot)))
	{
		cerr_set(&dict->err, CERR_OVERFLOW);
		return false;
	}

	if (!(tmp = calloc(n, sizeof(struct slot))))
	{
		cerr_set(&dict->err, CERR_MEMORY);
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
			if ((tmp = find(dict, tmp_2[i].hash, UNUSED)))
			{
				*tmp = tmp_2[i];
			}
		}
	}

	free(tmp_2);

	return true;
}

#pragma GCC diagnostic pop

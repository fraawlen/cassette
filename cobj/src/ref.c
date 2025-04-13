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
#include <limits.h>
#include <stdbool.h>
#include <stdckdint.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define GUARD(OBJ, ...)       if (!OBJ || cerr_critical(OBJ->err)) { return __VA_OPT__(__VA_ARGS__); }
#define GUARD_ID(OBJ, I, ...) if (I >= OBJ->n) { return __VA_OPT__(__VA_ARGS__); }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct slot
{
	void *ptr;
	unsigned int n_ref;
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct cref
{
	struct slot *slots;
	size_t n;
	size_t n_alloc;
	enum cerr err;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static bool grow (cref *, size_t);
static void pull (cref *, size_t);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cref_clear(cref *ref)
{
	GUARD(ref);

	ref->n = 0;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cref_clear_warnings(cref *ref)
{
	GUARD(ref);

	cerr_clear_warnings(&ref->err);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cref *
cref_clone(const cref *ref)
{
	GUARD(ref, nullptr);

	cref *ref_new;

	if (!(ref_new = calloc(1, sizeof(cref))))
	{
		return nullptr;
	}

	if (!(ref_new->slots = malloc(ref->n_alloc * sizeof(struct slot))))
	{
		free(ref_new);
		return nullptr;
	}

	memcpy(ref_new->slots, ref->slots, ref->n * sizeof(struct slot));

	ref_new->n       = ref->n;
	ref_new->n_alloc = ref->n_alloc;
	ref_new->err     = ref->err;

	return ref_new;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

unsigned int
cref_count(const cref *ref, size_t index)
{
	GUARD(ref, 0);
	GUARD_ID(ref, index, 0);

	return ref->slots[index].n_ref;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cref *
cref_create(void)
{
	cref *ref;

	if (!(ref = malloc(sizeof(cref))))
	{
		return nullptr;
	}

	if (!(ref->slots = calloc(1, sizeof(struct slot))))
	{
		free(ref);
		return nullptr;
	}

	ref->n       = 0;
	ref->n_alloc = 1;
	ref->err     = CERR_NONE;

	return ref;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

nullptr_t
cref_destroy(cref *ref)
{
	GUARD(ref, nullptr);

	free(ref->slots);
	free(ref);

	return nullptr;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cerr
cref_error(const cref *ref)
{
	return ref ? ref->err : CERR_INVALID;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

unsigned int
cref_find(const cref *ref, void *ptr, size_t *index)
{
	GUARD(ref, 0);

	for (size_t i = 0; i < ref->n; i++)
	{
		if (ref->slots[i].ptr == ptr)
		{
			if (index)
			{
				*index = i;
			}
			return ref->slots[i].n_ref;
		}
	}

	return 0;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
cref_length(const cref *ref)
{
	GUARD(ref, 0);

	return ref->n;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cref_prealloc(cref *ref, size_t slots_number)
{
	GUARD(ref);

	grow(ref, slots_number);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void *
cref_ptr(const cref *ref, size_t index)
{
	GUARD(ref, nullptr);
	GUARD_ID(ref, index, nullptr);

	return ref->slots[index].ptr;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cref_pull_index(cref *ref, size_t index)
{
	GUARD(ref);
	GUARD_ID(ref, index);

	if (--ref->slots[index].n_ref == 0)   
	{
		pull(ref, index);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cref_pull_ptr(cref *ref, void *ptr)
{
	GUARD(ref);

	size_t i = 0;

	if (cref_find(ref, ptr, &i) > 0)
	{
		cref_pull_index(ref, i);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cref_purge_index(cref *ref, size_t index)
{
	GUARD(ref);
	GUARD_ID(ref, index);

	pull(ref, index);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cref_purge_ptr(cref *ref, void *ptr)
{
	GUARD(ref);

	size_t i = 0;

	if (cref_find(ref, ptr, &i) > 0)
	{
		cref_purge_index(ref, i);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cref_push(cref *ref, void *ptr)
{
	GUARD(ref);

	if (!ptr)
	{
		return;
	}

	/* if found, increment ref counter */
	
	size_t i = 0;

	if (cref_find(ref, ptr, &i) > 0)
	{
		if (ref->slots[i].n_ref == UINT_MAX)
		{
			cerr_set(&ref->err, CERR_OVERFLOW);
			return;
		}
		ref->slots[i].n_ref++;
		return;
	}

	/* if not, add new ref */

	size_t n;

	if (ref->n >= ref->n_alloc)
	{
		if (ckd_mul(&n, ref->n_alloc, 2))
		{
			cerr_set(&ref->err, CERR_OVERFLOW);
			return;
		}
		if (!grow(ref, n))
		{
			return;
		}
	}

	ref->slots[ref->n].ptr   = ptr;
	ref->slots[ref->n].n_ref = 1;
	ref->n++;
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static bool
grow(cref *ref, size_t n)
{
	return n > ref->n_alloc && CUTIL_REALLOC(ref->slots, ref->n_alloc, n, sizeof(struct slot), ref->err);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
pull(cref *ref, size_t i)
{
	memmove(ref->slots + i, ref->slots + i + 1, (--ref->n - i) * sizeof(struct slot));
}

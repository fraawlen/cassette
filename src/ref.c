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

#include <cassette/cref.h>
#include <limits.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "safe.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

struct _slot
{
	const void *ptr;
	unsigned int n_ref;
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct cref
{
	struct _slot *slots;
	size_t n;
	size_t n_alloc;
	enum cref_err err;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static bool _grow (cref *ref, size_t n) CREF_NONNULL(1);
static void _pull (cref *ref, size_t i) CREF_NONNULL(1);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

cref cref_placeholder_instance = 
{
	.slots   = NULL,
	.n       = 0,
	.n_alloc = 0,
	.err     = CREF_INVALID,
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cref_clear(cref *ref)
{
	if (ref->err)
	{
		return;
	}

	ref->n = 0;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cref *
cref_clone(cref *ref)
{
	cref *ref_new;

	if (ref->err || !(ref_new = calloc(1, sizeof(cref))))
	{
		return CREF_PLACEHOLDER;
	}

	if (!_grow(ref_new, ref->n_alloc))
	{
		free(ref_new);
		return CREF_PLACEHOLDER;
	}

	memcpy(ref_new->slots, ref->slots, ref->n * sizeof(struct _slot));

	ref_new->n   = ref->n;
	ref_new->err = CREF_OK;

	return ref_new;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

unsigned int
cref_count(const cref *ref, size_t index)
{
	if (ref->err || index >= ref->n)
	{
		return 0;
	}

	return ref->slots[index].n_ref;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cref *
cref_create(void)
{
	cref *ref;

	if (!(ref = calloc(1, sizeof(cref))))
	{
		return CREF_PLACEHOLDER;
	}

	if (!_grow(ref, 1))
	{
		free(ref);
		return CREF_PLACEHOLDER;
	}

	ref->n   = 0;
	ref->err = CREF_OK;

	return ref;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cref_destroy(cref *ref)
{
	if (ref == CREF_PLACEHOLDER)
	{
		return;
	}

	free(ref->slots);
	free(ref);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cref_err
cref_error(const cref *ref)
{
	return ref->err;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

unsigned int
cref_find(const cref *ref, const void *ptr, size_t *index)
{
	if (ref->err)
	{
		return 0;
	}

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
	if (ref->err)
	{
		return 0;
	}

	return ref->n;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cref_prealloc(cref *ref, size_t slots_number)
{
	if (ref->err)
	{
		return;
	}

	_grow(ref, slots_number);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const void *
cref_ptr(const cref *ref, size_t index)
{
	if (ref->err || index >= ref->n)
	{
		return NULL;
	}
	
	return ref->slots[index].ptr;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cref_pull_index(cref *ref, size_t index)
{
	if (ref->err || index >= ref->n || --ref->slots[index].n_ref > 0)   
	{
		return;
	}

	_pull(ref, index);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cref_pull_ptr(cref *ref, const void *ptr)
{
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
	if (ref->err || index >= ref->n)   
	{
		return;
	}

	_pull(ref, index);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cref_purge_ptr(cref *ref, const void *ptr)
{
	size_t i = 0;

	if (cref_find(ref, ptr, &i) > 0)
	{
		cref_purge_index(ref, i);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cref_push(cref *ref, const void *ptr)
{
	size_t i = 0;

	if (ref->err)
	{
		return;
	}

	/* if found, increment ref counter */

	if (cref_find(ref, ptr, &i) > 0)
	{
		if (ref->slots[i].n_ref == UINT_MAX)
		{
			ref->err |= CREF_OVERFLOW;
			return;
		}
		ref->slots[i].n_ref++;
		return;
	}

	/* if not, add new ref */

	if (ref->n >= ref->n_alloc)
	{
		if (!safe_mul(NULL, ref->n_alloc, 2))
		{
			ref->err |= CREF_OVERFLOW;
			return;
		}
		if (!_grow(ref, ref->n_alloc * 2))
		{
			return;
		}
	}

	ref->slots[ref->n].ptr   = ptr;
	ref->slots[ref->n].n_ref = 1;
	ref->n++;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cref_repair(cref *ref)
{
	ref->err &= CREF_INVALID;
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static bool
_grow(cref *ref, size_t n)
{
	struct _slot *tmp;

	if (n <= ref->n_alloc)
	{
		return true;
	}

	if (!safe_mul(NULL, n, sizeof(struct _slot)))
	{
		ref->err |= CREF_OVERFLOW;
		return false;
	}

	if (!(tmp = realloc(ref->slots, n * sizeof(struct _slot))))
	{
		ref->err |= CREF_MEMORY;
		return false;
	}

	ref->n_alloc = n;
	ref->slots   = tmp;

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_pull(cref *ref, size_t i)
{
	memmove(ref->slots + i, ref->slots + i + 1, (--ref->n - i) * sizeof(struct _slot));
}

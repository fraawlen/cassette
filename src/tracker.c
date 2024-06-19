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

#include <assert.h>
#include <cassette/cobj.h>
#include <limits.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#include "safe.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

struct _slot_t
{
	const void *ptr;
	unsigned long n_ref;
};

typedef struct _slot_t _slot_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct _tracker_t
{
	_slot_t *slots;
	size_t n;
	size_t n_alloc;
	size_t iterator;
	bool failed;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static bool _resize(cobj_tracker_t *tracker, size_t n, size_t a, size_t b);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static cobj_tracker_t _err_tracker = 
{
	.slots    = NULL,
	.n        = 0,
	.n_alloc  = 0,
	.iterator = SIZE_MAX,
	.failed   = false,
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cobj_tracker_clear(cobj_tracker_t *tracker)
{
	assert(tracker);

	if (tracker->failed)
	{
		return;
	}

	tracker->n = 0;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cobj_tracker_t *
cobj_tracker_create(size_t n_alloc)
{
	cobj_tracker_t *tracker;

	if (!(tracker = malloc(sizeof(cobj_tracker_t))))
	{
		return &_err_tracker;
	}

	tracker->slots    = NULL;
	tracker->n        = 0;
	tracker->n_alloc  = 0;
	tracker->iterator = SIZE_MAX;
	tracker->failed   = false;

	_resize(tracker, n_alloc, 1, 0);

	return tracker;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cobj_tracker_destroy(cobj_tracker_t **tracker)
{
	assert(tracker && *tracker);

	if (*tracker == &_err_tracker)
	{
		return;
	}

	free((*tracker)->slots);
	free(*tracker);

	*tracker = &_err_tracker;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

unsigned long
cobj_tracker_find(const cobj_tracker_t *tracker, const void *ptr, size_t *index)
{
	size_t i0;
	size_t i;

	assert(tracker);

	if (tracker->failed)
	{
		return 0;
	}

	if (tracker->n == 0 || !ptr)
	{
		return 0;
	}

	i0 = index && *index < tracker->n ? *index : tracker->n - 1;

	/* first scan, from i0 to 0 */

	i = i0;
	do
	{
		if (tracker->slots[i].ptr == ptr)
		{
			goto found;
		}
	}
	while (i-- > 0);

	/* second scan, from i0 to n */

	i = i0;
	while (++i < tracker->n)
	{
		if (tracker->slots[i].ptr == ptr)
		{
			goto found;
		}
	}

	/* end */

	return 0;

found:

	if (index)
	{
		*index = i;
	}

	return tracker->slots[i].n_ref;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
cobj_tracker_get_alloc_size(const cobj_tracker_t *tracker)
{
	assert(tracker);

	if (tracker->failed)
	{
		return 0;
	}

	return tracker->n_alloc;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const void *
cobj_tracker_get_index(const cobj_tracker_t *tracker, size_t index)
{
	assert(tracker);

	if (tracker->failed)
	{
		return NULL;
	}

	if (index >= tracker->n)
	{
		return NULL;
	}

	return tracker->slots[index].ptr;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

unsigned long
cobj_tracker_get_index_n_ref(const cobj_tracker_t *tracker, size_t index)
{
	assert(tracker);

	if (tracker->failed)
	{
		return 0;
	}

	if (index >= tracker->n)
	{
		return 0;
	}

	return tracker->slots[index].n_ref;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const void *
cobj_tracker_get_iteration(const cobj_tracker_t *tracker)
{
	assert(tracker);

	if (tracker->failed)
	{
		return NULL;
	}

	if (tracker->iterator == 0 || tracker->iterator > tracker->n)
	{
		return NULL;
	}

	return tracker->slots[tracker->iterator - 1].ptr;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

unsigned long
cobj_tracker_get_iteration_n_ref(const cobj_tracker_t *tracker)
{
	assert(tracker);

	if (tracker->failed)
	{
		return 0;
	}

	if (tracker->iterator == 0 || tracker->iterator > tracker->n)
	{
		return 0;
	}

	return tracker->slots[tracker->iterator - 1].n_ref;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
cobj_tracker_get_iterator_offset(const cobj_tracker_t *tracker)
{
	assert(tracker);

	if (tracker->failed)
	{
		return 0;
	}

	if (tracker->iterator > tracker->n)
	{
		return 0;
	}

	return tracker->iterator;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cobj_tracker_t *
cobj_tracker_get_placeholder(void)
{
	return &_err_tracker;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
cobj_tracker_get_size(const cobj_tracker_t *tracker)
{
	assert(tracker);

	if (tracker->failed)
	{
		return 0;
	}

	return tracker->n;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cobj_tracker_has_failed(const cobj_tracker_t *tracker)
{
	assert(tracker);

	return tracker->failed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cobj_tracker_increment_iterator(cobj_tracker_t *tracker)
{
	assert(tracker);

	if (tracker->failed)
	{
		return false;
	}

	if (tracker->iterator >= tracker->n)
	{
		return false;
	}

	tracker->iterator++;

	return true;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cobj_tracker_lock_iterator(cobj_tracker_t *tracker)
{
	assert(tracker);

	if (tracker->failed)
	{
		return;
	}

	tracker->iterator = SIZE_MAX;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cobj_tracker_pull_index(cobj_tracker_t *tracker, size_t index)
{
	assert(tracker);

	if (tracker->failed)
	{
		return;
	}

	if (index >= tracker->n)
	{
		return;
	}

	if (tracker->slots[index].n_ref-- > 1)
	{
		return;
	}

	if (index < tracker->iterator)
	{	
		tracker->iterator--;
	}

	for (tracker->n--; index < tracker->n; index++)
	{
		tracker->slots[index] = tracker->slots[index + 1];
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cobj_tracker_pull_pointer(cobj_tracker_t *tracker, const void *ptr, size_t index)
{
	assert(tracker);

	if (cobj_tracker_find(tracker, ptr, &index) > 0)
	{
		cobj_tracker_pull_index(tracker, index);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cobj_tracker_push(cobj_tracker_t *tracker, const void *ptr, size_t *index)
{
	size_t i = 0;
	
	assert(tracker);

	if (tracker->failed)
	{
		return;
	}

	if (!ptr)
	{
		return;
	}

	/* check if reference exist, increment the reference counter if it does */

	if (!index)
	{
		index = &i;
	}

	if (cobj_tracker_find(tracker, ptr, index) > 0)
	{
		if (tracker->slots[*index].n_ref < ULONG_MAX)
		{
			tracker->slots[*index].n_ref++;
		}
		return;
	}

	/* otherwhise add new element, grow array if needed */

	if (tracker->n >= tracker->n_alloc && !_resize(tracker, tracker->n, 2, 1))
	{
		return;
	}

	if (index)
	{
		*index = tracker->n;
	}

	tracker->slots[tracker->n].ptr   = ptr;
	tracker->slots[tracker->n].n_ref = 1;
	tracker->n++;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cobj_tracker_reset_iterator(cobj_tracker_t *tracker)
{
	assert(tracker);

	if (tracker->failed)
	{
		return;
	}

	tracker->iterator = 0;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cobj_tracker_trim(cobj_tracker_t *tracker)
{
	assert(tracker);

	if (tracker->failed)
	{
		return;
	}

	_resize(tracker, tracker->n, 1, 0);
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static bool
_resize(cobj_tracker_t *tracker, size_t n, size_t a, size_t b)
{
	_slot_t *tmp;
	
	bool safe = true;

	/* test for overflow */

	safe &= safe_mul(&n,   n, a);
	safe &= safe_add(&n,   n, b);
	safe &= safe_mul(NULL, n, sizeof(_slot_t));

	if (!safe)
	{
		tracker->failed = true;
		return false;
	}

	/* resize array */

	if (n == 0)
	{
		free(tracker->slots);
		tmp = NULL;
	}
	else
	{
		if (!(tmp = realloc(tracker->slots, n * sizeof(_slot_t))))
		{
			tracker->failed = true;
			return false;
		}
	}

	tracker->slots   = tmp;
	tracker->n_alloc = n;

	return true;
}

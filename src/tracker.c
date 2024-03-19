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
#include <limits.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#include <derelict/du.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

typedef struct _slot_t _slot_t;
struct _slot_t
{
	const void *ptr;
	unsigned long n_ref;
};

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

static bool _resize(du_tracker_t *tracker, size_t n, size_t a, size_t b);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static du_tracker_t _err_tracker = 
{
	.slots    = NULL,
	.n        = 0,
	.n_alloc  = 0,
	.iterator = 0,
	.failed   = false,
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
du_tracker_clear(du_tracker_t *tracker)
{
	assert(tracker);

	if (tracker->failed)
	{
		return;
	}

	tracker->n        = 0;
	tracker->iterator = 0;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

du_tracker_t *
du_tracker_create(size_t n_alloc)
{
	du_tracker_t *tracker = malloc(sizeof(du_tracker_t));

	if (!tracker)
	{
		return &_err_tracker;
	}

	tracker->slots    = NULL;
	tracker->n        = 0;
	tracker->n_alloc  = n_alloc;
	tracker->iterator = 0;
	tracker->failed   = false;

	_resize(tracker, n_alloc, 1, 0);

	return tracker;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_tracker_destroy(du_tracker_t **tracker)
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
du_tracker_find(const du_tracker_t *tracker, const void *ptr, size_t *index)
{
	assert(tracker);

	if (tracker->failed)
	{
		return 0;
	}

	if (tracker->n == 0 || !ptr)
	{
		return 0;
	}

	size_t i0 = index && *index < tracker->n ? *index : tracker->n - 1;
	size_t i;

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
du_tracker_get_alloc_size(const du_tracker_t *tracker)
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
du_tracker_get_index(const du_tracker_t *tracker, size_t index, unsigned long *n_ref)
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

	if (n_ref)
	{
		*n_ref = tracker->slots[index].n_ref;
	}

	return tracker->slots[index].ptr;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const void *
du_tracker_get_next(du_tracker_t *tracker, unsigned long *n_ref)
{
	assert(tracker);

	if (tracker->failed)
	{
		return NULL;
	}

	if (tracker->iterator >= tracker->n)
	{
		return NULL;
	}

	if (n_ref)
	{
		*n_ref = tracker->slots[tracker->iterator].n_ref;
	}

	return tracker->slots[tracker->iterator++].ptr;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
du_tracker_get_size(const du_tracker_t *tracker)
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
du_tracker_has_failed(const du_tracker_t *tracker)
{
	assert(tracker);

	return tracker->failed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_tracker_pull_index(du_tracker_t *tracker, size_t index)
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
du_tracker_pull_pointer(du_tracker_t *tracker, const void *ptr, size_t index)
{
	assert(tracker);

	if (du_tracker_find(tracker, ptr, &index) > 0)
	{
		du_tracker_pull_index(tracker, index);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_tracker_push(du_tracker_t *tracker, const void *ptr, size_t *index)
{
	assert(tracker);

	if (tracker->failed)
	{
		return;
	}

	if (!ptr)
	{
		return;
	}

	size_t i = 0;

	/* check if reference exist, increment the reference counter if it does */

	if (!index)
	{
		index = &i;
	}

	if (du_tracker_find(tracker, ptr, index) > 0)
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
du_tracker_reset_iterator(du_tracker_t *tracker)
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
du_tracker_trim(du_tracker_t *tracker)
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
_resize(du_tracker_t *tracker, size_t n, size_t a, size_t b)
{
	_slot_t *tmp;

	/* test for overflow */

	if (n > SIZE_MAX / a || n * a > SIZE_MAX - b || n * a + b > (SIZE_MAX - 1) / sizeof(_slot_t))
	{
		tracker->failed = true;
		return false;
	}

	n = n * a + b;

	/* resize array */

	if (n == 0)
	{
		free(tracker->slots);
		tmp = NULL;
	}
	else
	{
		tmp = realloc(tracker->slots, n * sizeof(_slot_t));
		if (!tmp)
		{
			tracker->failed = true;
			return false;
		}
	}

	tracker->slots   = tmp;
	tracker->n_alloc = n;

	return true;
}

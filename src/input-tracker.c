/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Graphics (CGUI) library.
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

#include <cassette/cgui.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

struct _input_tracker_t
{
	cgui_input_tracker_input_t *slots;
	size_t n;
	size_t n_alloc;
	size_t iterator;
	bool failed;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static cgui_input_tracker_t _err_inputs =
{
	.slots    = NULL,
	.n        = 0,
	.n_alloc  = 0,
	.iterator = SIZE_MAX,
	.failed   = true,
};

static const cgui_input_tracker_input_t _err_slot =
{
	.id  = 0,
	.x   = 0,
	.y   = 0,
	.ref = NULL,
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cgui_input_tracker_clear(cgui_input_tracker_t *inputs)
{
	assert(inputs);

	if (inputs->failed)
	{
		return;
	}

	inputs->n = 0;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgui_input_tracker_t *
cgui_input_tracker_create(size_t max_inputs)
{
	cgui_input_tracker_t *inputs;

	if (max_inputs > SIZE_MAX / sizeof(cgui_input_tracker_input_t))
	{
		return &_err_inputs;
	}

	if (!(inputs = malloc(sizeof(cgui_input_tracker_t))))
	{
		return &_err_inputs;
	}

	inputs->slots    = malloc(max_inputs * sizeof(cgui_input_tracker_input_t));
	inputs->n        = 0;
	inputs->n_alloc  = max_inputs;
	inputs->iterator = SIZE_MAX;
	inputs->failed   = !inputs->slots;

	return inputs;	
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_input_tracker_destroy(cgui_input_tracker_t **inputs)
{
	assert(inputs && *inputs);

	if (*inputs == &_err_inputs)
	{
		return;
	}

	free((*inputs)->slots);
	free(*inputs);

	*inputs = &_err_inputs;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgui_input_tracker_find(cgui_input_tracker_t *inputs, unsigned int id, size_t *index)
{
	assert(inputs);

	if (inputs->failed)
	{
		return false;
	}

	if (inputs->n == 0)
	{
		return false;
	}

	for (size_t i = 0; i < inputs->n; i++)
	{
		if (inputs->slots[i].id == id)
		{
			if (index)
			{
				*index = i;
			}
			return true;
		}
	}

	return false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
cgui_input_tracker_get_alloc_size(const cgui_input_tracker_t *inputs)
{
	assert(inputs);

	if (inputs->failed)
	{
		return 0;
	}

	return inputs->n_alloc;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgui_input_tracker_input_t
cgui_input_tracker_get_index(const cgui_input_tracker_t *inputs, size_t index)
{
	assert(inputs);

	if (inputs->failed)
	{
		return _err_slot;
	}

	if (index >= inputs->n)
	{
		return _err_slot;
	}

	return inputs->slots[index];
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgui_input_tracker_input_t
cgui_input_tracker_get_iteration(const cgui_input_tracker_t *inputs)
{
	assert(inputs);

	if (inputs->failed)
	{
		return _err_slot;
	}

	if (inputs->iterator == 0 || inputs->iterator > inputs->n)
	{
		return _err_slot;
	}

	return inputs->slots[inputs->iterator - 1];
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
cgui_input_tracker_get_iterator_offset(const cgui_input_tracker_t *inputs)
{
	assert(inputs);

	if (inputs->failed)
	{
		return 0;
	}

	if (inputs->iterator > inputs->n)
	{
		return 0;
	}

	return inputs->iterator;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
cgui_input_tracker_get_load(const cgui_input_tracker_t *inputs)
{
	assert(inputs);

	if (inputs->failed)
	{
		return 0;
	}

	return inputs->n;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgui_input_tracker_t *
cgui_input_tracker_get_placeholder(void)
{
	return &_err_inputs;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgui_input_tracker_has_failed(const cgui_input_tracker_t *inputs)
{
	assert(inputs);

	return inputs->failed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgui_input_tracker_increment_iterator(cgui_input_tracker_t *inputs)
{
	assert(inputs);

	if (inputs->failed)
	{
		return false;
	}

	if (inputs->iterator >= inputs->n)
	{
		return false;
	}

	inputs->iterator++;

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_input_tracker_lock_iterator(cgui_input_tracker_t *inputs)
{
	assert(inputs);

	if (inputs->failed)
	{
		return;
	}

	inputs->iterator = SIZE_MAX;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_input_tracker_pull_id(cgui_input_tracker_t *inputs, unsigned int id)
{
	size_t index;

	assert(inputs);

	if (cgui_input_tracker_find(inputs, id, &index))
	{
		cgui_input_tracker_pull_index(inputs, index);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_input_tracker_pull_index(cgui_input_tracker_t *inputs, unsigned int index)
{
	assert(inputs);

	if (inputs->failed)
	{
		return;
	}

	if (index >= inputs->n)
	{
		return;
	}

	if (index < inputs->iterator)
	{
		inputs->iterator--;
	}

	for (inputs->n--; index < inputs->n; index++)
	{
		inputs->slots[index] = inputs->slots[index + 1];
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_input_tracker_push(cgui_input_tracker_t *inputs, unsigned int id, unsigned int x, unsigned int y, void *ref)
{
	assert(inputs);

	if (inputs->failed)
	{
		return;
	}

	cgui_input_tracker_pull_id(inputs, id);

	if (inputs->n >= inputs->n_alloc)
	{
		return;
	}

	inputs->slots[inputs->n].id  = id;
	inputs->slots[inputs->n].x   = x;
	inputs->slots[inputs->n].y   = y;
	inputs->slots[inputs->n].ref = ref;
	inputs->n++;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_input_tracker_reset_iterator(cgui_input_tracker_t *inputs)
{
	assert(inputs);

	if (inputs->failed)
	{
		return;
	}

	inputs->iterator = 0;
}

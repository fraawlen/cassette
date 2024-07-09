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

#include <cassette/cgui.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "safe.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

struct cgui_inputs
{
	struct cgui_input *slots;
	size_t n;
	size_t n_alloc;
	enum cgui_inputs_err err;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static bool _resize (cgui_inputs *inputs, size_t n) CGUI_NONNULL(1);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

cgui_inputs cgui_inputs_placeholder_instance =
{
	.slots   = NULL,
	.n       = 0,
	.n_alloc = 0,
	.err     = CGUI_INPUTS_INVALID,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const struct cgui_input _err_slot =
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
cgui_inputs_clear(cgui_inputs *inputs)
{
	if (inputs->err)
	{
		return;
	}

	inputs->n = 0;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgui_inputs *
cgui_inputs_clone(const cgui_inputs *inputs)
{
	cgui_inputs *inputs_new;

	if (inputs->err || !(inputs_new = calloc(1, sizeof(cgui_inputs))))
	{
		return CGUI_INPUTS_PLACEHOLDER;
	}

	if (!_resize(inputs_new, inputs->n_alloc))
	{
		free(inputs_new);
		return CGUI_INPUTS_PLACEHOLDER;
	}

	memcpy(inputs_new->slots, inputs->slots, inputs->n * sizeof(struct cgui_input));

	inputs_new->n   = inputs->n;
	inputs_new->err = CGUI_INPUTS_OK;

	return inputs_new;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgui_inputs *
cgui_inputs_create(size_t max_inputs)
{
	cgui_inputs *inputs;

	if (!(inputs = calloc(1, sizeof(cgui_inputs))))
	{
		return CGUI_INPUTS_PLACEHOLDER;
	}

	if (!_resize(inputs, max_inputs))
	{
		free(inputs);
		return CGUI_INPUTS_PLACEHOLDER;
	}

	inputs->n   = 0;
	inputs->err = CGUI_INPUTS_OK;

	return inputs;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_inputs_destroy(cgui_inputs *inputs)
{
	if (inputs == CGUI_INPUTS_PLACEHOLDER)
	{
		return;
	}

	free(inputs->slots);
	free(inputs);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cgui_inputs_err
cgui_inputs_error(const cgui_inputs *inputs)
{
	return inputs->err;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgui_inputs_find(const cgui_inputs *inputs, unsigned int id, size_t *index)
{
	if (inputs->err)
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

struct cgui_input
cgui_inputs_get(const cgui_inputs *inputs, size_t index)
{
	if (inputs->err || index >= inputs->n)
	{
		return _err_slot;
	}

	return inputs->slots[index];
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
cgui_inputs_load(const cgui_inputs *inputs)
{
	if (inputs->err)
	{
		return 0;
	}

	return inputs->n;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_inputs_pull_id(cgui_inputs *inputs, unsigned int id)
{
	size_t index;

	if (cgui_inputs_find(inputs, id, &index))
	{
		cgui_inputs_pull_index(inputs, index);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_inputs_pull_index(cgui_inputs *inputs, size_t index)
{
	if (inputs->err || index >= inputs->n)
	{
		return;
	}

	memmove(
		inputs->slots + index,
		inputs->slots + index + 1,
		(--inputs->n - index) * sizeof(struct cgui_input));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_inputs_push(cgui_inputs *inputs, unsigned int id, int x, int y, void *ref)
{
	if (inputs->err)
	{
		return;
	}

	cgui_inputs_pull_id(inputs, id);
	if (inputs->n < inputs->n_alloc)
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
cgui_inputs_repair(cgui_inputs *inputs)
{
	inputs->err &= CGUI_INPUTS_INVALID;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_inputs_resize(cgui_inputs *inputs, size_t max_inputs)
{
	if (inputs->err)
	{
		return;
	}

	_resize(inputs, max_inputs);
}

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static bool
_resize(cgui_inputs *inputs, size_t n)
{
	struct cgui_input *tmp;

	if (n == 0)
	{
		inputs->err |= CGUI_INPUTS_BAD_INPUT;
		return false;
	}

	if (!safe_mul(NULL, n, sizeof(struct cgui_input)))
	{
		inputs->err |= CGUI_INPUTS_OVERFLOW;
		return false;
	}

	if (!(tmp = realloc(inputs->slots, n * sizeof(struct cgui_input))))
	{
		inputs->err |= CGUI_INPUTS_MEMORY;
		return false;
	}

	inputs->n       = n < inputs->n ? n : inputs->n;
	inputs->n_alloc = n;
	inputs->slots   = tmp;
	
	return true;
}

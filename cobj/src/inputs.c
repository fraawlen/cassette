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
#include <stdbool.h>
#include <stdckdint.h>
#include <stddef.h>
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
	unsigned int id;
	int16_t x;
	int16_t y;
	void *ptr;
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct cinputs
{
	struct slot *slots;
	size_t n;
	size_t n_alloc;
	enum cerr err;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static bool resize (cinputs *, size_t);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cinputs_clear(cinputs *inputs)
{
	GUARD(inputs);

	inputs->n = 0;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cinputs_clear_warnings(cinputs *inputs)
{
	GUARD(inputs);

	cerr_clear_warnings(&inputs->err);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cinputs *
cinputs_clone(const cinputs *inputs)
{
	GUARD(inputs, nullptr);

	cinputs *inputs_new;

	if (!(inputs_new = calloc(1, sizeof(cinputs))))
	{
		return nullptr;
	}

	if (!resize(inputs_new, inputs->n_alloc))
	{
		free(inputs_new);
		return nullptr;
	}

	memcpy(inputs_new->slots, inputs->slots, inputs->n * sizeof(struct slot));

	inputs_new->n   = inputs->n;
	inputs_new->err = inputs->err;

	return inputs_new;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cinputs *
cinputs_create(size_t max_inputs)
{
	cinputs *inputs;

	if (!(inputs = calloc(1, sizeof(cinputs))))
	{
		return nullptr;
	}

	if (!resize(inputs, max_inputs))
	{
		free(inputs);
		return nullptr;
	}

	inputs->n   = 0;
	inputs->err = CERR_NONE;

	return inputs;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

nullptr_t
cinputs_destroy(cinputs *inputs)
{
	if (inputs)
	{
		free(inputs->slots);
		free(inputs);
	}

	return nullptr;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cerr
cinputs_error(const cinputs *inputs)
{
	return inputs ? inputs->err : CERR_INVALID;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cinputs_find(const cinputs *inputs, unsigned int id, size_t *index)
{
	GUARD(inputs, false);

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

unsigned int
cinputs_id(const cinputs *inputs, size_t index)
{
	GUARD(inputs, 0);
	GUARD_ID(inputs, index, 0);

	return inputs->slots[index].id;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
cinputs_load(const cinputs *inputs)
{
	GUARD(inputs, 0);

	return inputs->n;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void *
cinputs_ptr(const cinputs *inputs, size_t index)
{
	GUARD(inputs, nullptr);
	GUARD_ID(inputs, index, nullptr);

	return inputs->slots[index].ptr;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cinputs_pull_id(cinputs *inputs, unsigned int id)
{
	size_t index;

	if (cinputs_find(inputs, id, &index))
	{
		cinputs_pull_index(inputs, index);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cinputs_pull_index(cinputs *inputs, size_t index)
{
	GUARD(inputs);
	GUARD_ID(inputs, index);

	memmove(
		inputs->slots + index,
		inputs->slots + index + 1,
		(--inputs->n - index) * sizeof(struct slot));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cinputs_push(cinputs *inputs, unsigned int id, int x, int y, void *ptr)
{
	GUARD(inputs);

	cinputs_pull_id(inputs, id);
	if (inputs->n >= inputs->n_alloc)
	{
		return;
	}

	inputs->slots[inputs->n].id  = id;
	inputs->slots[inputs->n].x   = x;
	inputs->slots[inputs->n].y   = y;
	inputs->slots[inputs->n].ptr = ptr;
	inputs->n++;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cinputs_resize(cinputs *inputs, size_t max_inputs)
{
	GUARD(inputs);

	resize(inputs, max_inputs);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

int16_t
cinputs_x(const cinputs *inputs, size_t index)
{
	GUARD(inputs, 0);
	GUARD_ID(inputs, index, 0);

	return inputs->slots[index].x;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

int16_t
cinputs_y(const cinputs *inputs, size_t index)
{
	GUARD(inputs, 0);
	GUARD_ID(inputs, index, 0);

	return inputs->slots[index].y;
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static bool
resize(cinputs *inputs, size_t n)
{
	if (!CUTIL_REALLOC(inputs->slots, inputs->n_alloc, n, sizeof(struct slot), inputs->err))
	{
		return false;
	}

	inputs->n = n < inputs->n ? n : inputs->n;
	
	return true;
}

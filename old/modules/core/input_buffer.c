/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Derelict Graphics (DG) GUI library.
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
#include <stdint.h>
#include <stdlib.h>

#include "errno.h"
#include "input_buffer.h"

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
dg_core_input_buffer_clear(dg_core_input_buffer_t *buf)
{
	assert(buf);

	buf->n = 0;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
dg_core_input_buffer_find(dg_core_input_buffer_t *buf, uint32_t id, size_t *pos)
{
	assert(buf);

	for (size_t i = 0; i < buf->n; i++) {
		if (buf->inputs[i].id == id) {
			if (pos) {
				*pos = i;
			}
			return true;
		}
	}

	return false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
dg_core_input_buffer_init(dg_core_input_buffer_t *buf, size_t n_alloc, dg_core_input_buffer_kind_t kind)
{
	assert(buf && n_alloc > 0);

	buf->inputs = malloc(n_alloc * sizeof(dg_core_input_buffer_slot_t));
	if (!buf->inputs) {
		dg_core_errno_set(DG_CORE_ERRNO_MEMORY);	
		return false;
	}

	buf->kind = kind;
	buf->n_alloc = n_alloc;
	buf->n = 0;

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
dg_core_input_buffer_pull(dg_core_input_buffer_t *buf, uint32_t id)
{
	assert(buf);

	size_t i = 0;

	if (!dg_core_input_buffer_find(buf, id, &i)) {
		return false;
	}

	buf->n--;
	for (; i < buf->n; i++) {
		buf->inputs[i] = buf->inputs[i + 1];
	}

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
dg_core_input_buffer_push_coord(dg_core_input_buffer_t *buf, uint32_t id, int16_t x, int16_t y)
{
	assert(buf && buf->kind == DG_CORE_INPUT_BUFFER_COORD);

	dg_core_input_buffer_pull(buf, id);

	if (buf->n >= buf->n_alloc) {
		return false;
	}

	buf->inputs[buf->n].id = id;
	buf->inputs[buf->n].x  = x;
	buf->inputs[buf->n].y  = y;
	buf->n++;

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
dg_core_input_buffer_push_ref(dg_core_input_buffer_t *buf, uint32_t id, void *ref)
{
	assert(buf && buf->kind == DG_CORE_INPUT_BUFFER_REF);

	dg_core_input_buffer_pull(buf, id);

	if (buf->n >= buf->n_alloc) {
		return false;
	}

	buf->inputs[buf->n].id  = id;
	buf->inputs[buf->n].ref = ref;
	buf->n++;

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_input_buffer_reset(dg_core_input_buffer_t *buf)
{
	assert(buf);

	buf->n = 0;
	buf->n_alloc = 0;
	free(buf->inputs);
}

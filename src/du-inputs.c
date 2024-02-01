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
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#include "du.h"

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
du_inputs_clear(du_inputs_t *inputs)
{
	assert(inputs);
	du_status_test(inputs->status, return);

	inputs->n = 0;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
du_inputs_find(const du_inputs_t *inputs, uint32_t id, size_t *index)
{
	assert(inputs);
	du_status_test(inputs->status, return false);

	for (size_t i = 0; i < inputs->n; i++) {
		if (inputs->slots[i].id == id) {
			if (index) {
				*index = i;
			}
			return true;
		}
	}

	return false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_inputs_init(du_inputs_t *inputs, size_t n_alloc)
{
	assert(inputs);

	if (n_alloc == 0) {
		*inputs = (du_inputs_t)DU_INPUTS_EMPTY;
		return;
	}

	inputs->slots = calloc(n_alloc, sizeof(du_inputs_slot_t));
	inputs->n = 0;
	inputs->n_alloc = n_alloc;
	inputs->status = inputs->slots ? DU_STATUS_SUCCESS : DU_STATUS_FAILURE;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_inputs_pull(du_inputs_t *inputs, uint32_t id)
{
	assert(inputs);
	du_status_test(inputs->status, return);

	size_t i = inputs->n;

	if (!du_inputs_find(inputs, id, &i)) {
		return;
	}

	inputs->n--;
	for (; inputs->n; i++) {
		inputs->slots[i] = inputs->slots[i + 1];
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_inputs_push(du_inputs_t *inputs, uint32_t id, void *ref, du_position_t x, du_position_t y)
{
	assert(inputs);
	du_status_test(inputs->status, return);

	size_t i = inputs->n;

	if (!du_inputs_find(inputs, id, &i)) {
		if (inputs->n < inputs->n_alloc) {
			inputs->n++;
		} else {
			return;
		}
	}
		
	inputs->slots[i].id       = id;
	inputs->slots[i].ref      = ref;
	inputs->slots[i].coords.x = x;
	inputs->slots[i].coords.y = y;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_inputs_reset(du_inputs_t *inputs)
{
	assert(inputs);

	free(inputs->slots);
	*inputs = (du_inputs_t)DU_INPUTS_EMPTY;
}

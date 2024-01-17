/**
 * Copyright © 2024 Frawwlen <fraawlen@posteo.net>
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
#include "stack.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static bool _extend(dg_core_stack_t *stk);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

bool
dg_core_stack_find(dg_core_stack_t *stk, const void *ptr, size_t *pos)
{
	assert(stk);

	const size_t i0 = pos && *pos < stk->n ? *pos : stk->n - 1;
	size_t i;

	/* first scan from i0 to 0 */

	for (i = i0; i < SIZE_MAX; i--) {
		if (stk->ptr[i] == ptr) {
			goto found;
		}
	}

	/* second scan, from i0 + 1 to max */

	for (i = i0 + 1; i < stk->n; i++) {
		if (stk->ptr[i] == ptr) {
			goto found;
		}
	}

	/*****/

	return false;

found:

	if (pos) {
		*pos = i;
	}
	
	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
dg_core_stack_init(dg_core_stack_t *stk, size_t n_alloc)
{
	assert(stk);

	if (n_alloc == 0) {
		*stk = DG_CORE_STACK_EMPTY;
		return true;
	}

	stk->ptr = malloc(n_alloc * sizeof(void*));
	if (!stk->ptr) {
		dg_core_errno_set(DG_CORE_ERRNO_STACK);
		return false;
	}

	stk->n_alloc = n_alloc;
	stk->n = 0;

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_stack_pull(dg_core_stack_t *stk, const void *ptr)
{
	assert(stk);
	
	size_t i = 0;

	if (!dg_core_stack_find(stk, ptr, &i)) {
		return;
	}

	stk->n--;

	if (stk->n > 0) {
		for (; i < stk->n; i++) {
			stk->ptr[i] = stk->ptr[i + 1];
		}
	} else {
		dg_core_stack_reset(stk);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
dg_core_stack_push(dg_core_stack_t *stk, const void *ptr, size_t *pos)
{
	assert(stk);

	if (dg_core_stack_find(stk, ptr, NULL)) {
		return true;
	}

	if (stk->n >= stk->n_alloc && !_extend(stk)) {
		return false;
	}

	if (pos) {
		*pos = stk->n;
	}

	stk->ptr[stk->n] = ptr;
	stk->n++;

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_stack_reset(dg_core_stack_t *stk)
{
	assert(stk);

	free(stk->ptr);

	*stk = DG_CORE_STACK_EMPTY;
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static bool
_extend(dg_core_stack_t *stk)
{
	const size_t n_alloc = stk->n_alloc > 0 ? stk->n_alloc * 2 : 1;

	void *tmp = realloc(stk->ptr, n_alloc * sizeof(void*));
	if (!tmp) {
		dg_core_errno_set(DG_CORE_ERRNO_STACK);
		return false;
	}

	stk->ptr = tmp;
	stk->n_alloc = n_alloc;

	return true;
}

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

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

bool
cutil_realloc(void **ptr, size_t *n_store, size_t n_new, size_t size, enum cerr *err)
{
	void *tmp;
	size_t r;

	if (!ptr)
	{
		return false;
	}

	if (n_new == 0 || size == 0)
	{
		cerr_set(err, CERR_PARAM);
		return false;
	}

	if (ckd_mul(&r, n_new, size))
	{
		cerr_set(err, CERR_OVERFLOW);
		return false;
	}

	if (!(tmp = realloc(*ptr, r)))
	{
		cerr_set(err, CERR_MEMORY);
		return false;
	}

	if (n_store)
	{
		*n_store = n_new;
	}

	*ptr = tmp;

	return true;
}

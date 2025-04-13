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

#pragma once

#include <stdbool.h>
#include <stdlib.h>

#include "cerr.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/* IMPURE METHODS *******************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Convenience macro to not have to cast ptr and get param addresses.
 */
#define CUTIL_REALLOC(PTR, N_STORE, N_NEW, SIZE, ERR) \
	cutil_realloc((void**)&PTR, &N_STORE, N_NEW, SIZE, &ERR)

/**
 * [Description]
 *
 * 	Cassette's realloc wrapper for arrays.
 * 	If the function fails, a Cassette error is set.
 * 	The realloc size (n_new * size) should not be 0.
 * 	On success, the size of the new memory area is written into n_store if it's not NULL.
 *
 * [Parameters]
 *
 * 	ptr     - Pointer to memory to realloc.
 * 	n_store - Optional parameter to store the total size of the new memory area.
 * 	n_new   - Number of elements.
 * 	size    - Size of each element.
 * 	err     - Optional parameter to store an error in case of failure.
 *
 * [Returns]
 *
 * 	True when the realloc is successful, false otherwhise.
 * 	This function can fail when: ptr is NULL, n_new * size overflows, the realloc fails.
 * 	In case of failure, ptr and n_store are not modified.
 */
bool cutil_realloc(void **ptr, size_t *n_store, size_t n_new, size_t size, enum cerr *err);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

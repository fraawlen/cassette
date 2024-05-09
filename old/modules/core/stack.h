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

#ifndef DG_CORE_STACK_H
#define DG_CORE_STACK_H

#include <stdbool.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define DG_CORE_STACK_EMPTY (dg_core_stack_t){.ptr = NULL, .n = 0, .n_alloc = 0}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Stack of arbitrary pointers. Used to store, retrieve and track any arbitrary element of data, which is
 * referenced inside the stack by the generic void pointer array. n <= n_alloc.
 *
 * @param ptr     : array of stored pointers
 * @param n       : size of actively used array space
 * @param n_alloc : size of allocated array space
 */
typedef struct {
	const void **ptr;
	size_t n;
	size_t n_alloc;
} dg_core_stack_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Preallocate memory to the stack and set its variables appropriately. Allocated memory in the array is not
 * initialised. This function is recommended but optional to operate dg_core_stack_t structs because a stack
 * can allocate memory automatically when needed.
 * If n = 0, no memory is allocated and *stk is instead set to DG_CORE_STACK_EMPTY.
 *
 * @param stk     : stack to init
 * @param n_alloc : initial allocated size of the pointer array. 
 *
 * @error DG_CORE_ERRNO_STACK : failure to alloc memory to the pointer array
 */
bool dg_core_stack_init(dg_core_stack_t *stk, size_t n_alloc);

/**
 * Zeroes the stack and free allocated memory within the structure. The given structure itself is not freed,
 * and may require an explicit free operation. The pointers that were referenced in **ptr are not freed
 * either.
 *
 * @param dg_core_stack : stack to reset
 */
void dg_core_stack_reset(dg_core_stack_t *stk);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Checks if given item is present in the dg_core_stack.
 *
 * @param stk : stack to search
 * @param ptr : pointer to find
 * @param pos : optional, pointer whose value will be set to the position index of the found item in case of
 *              success. If the item is not found this paramater is not modified. This parameter is optional
 *              and can be set to NULL. This parameter is also used as the starting point of the array scan
 *              for speed optimisation. Therefore, if provided, its pointed value should be initialised
 *
 * @return : true if item is present, false otherwhise.
 */
bool dg_core_stack_find(dg_core_stack_t *stk, const void *ptr, size_t *pos);

/**
 * Removes a pointer from the stack. The value pointed to by the removed pointer is not freed nor modified. 
 * The stack is not shrinked when a pointer is pulled from it, unless there is no remaining pointer in it
 * (n_alloc == 0), in which case dg_core_stack_reset() is called internally and memory is freed. If the given
 * pointer was not part of stack, nothing happens.
 *
 * @param stk : stack to remove items from
 * @param ptr : pointer to pull from the stack
 */
void dg_core_stack_pull(dg_core_stack_t *stk, const void *ptr);

/**
 * Adds a pointer to the stack. A given pointer can only be added once, duplicates pointers are not added.
 * The stack automatically expands as needed to accomodate new pointers added. New pointers are added at the
 * end of the array inside the struct.
 *
 * @param stk : stack to add items to
 * @param ptr : pointer to push to stack
 * @param pos : optional, if non NULL, dg_core_stack_push() will put in it, on success, the position of the
 *              pointer within the stack, in case of failure, *pos is unmodified.
 *
 * @return : true if the pointer was successfully added, false in case of failure (and errno is also set). If
 *           the pointer is a duplicate, true is returned anyway
 *
 * @error DG_CORE_ERRNO_STACK : failure to realloc memory to the pointer array
 */
bool dg_core_stack_push(dg_core_stack_t *stk, const void *ptr, size_t *pos);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DG_CORE_STACK_H */

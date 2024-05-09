/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Objects (COBJ) library.
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

#ifndef COBJ_TRACKER_H
#define COBJ_TRACKER_H

#include <stdbool.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

typedef struct _tracker_t cobj_tracker_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cobj_tracker_t *cobj_tracker_create(size_t n_alloc);

void cobj_tracker_destroy(cobj_tracker_t **tracker);

cobj_tracker_t *cobj_tracker_get_placeholder(void);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void cobj_tracker_clear(cobj_tracker_t *tracker);

bool cobj_tracker_increment_iterator(cobj_tracker_t *tracker);

void cobj_tracker_lock_iterator(cobj_tracker_t *tracker);

void cobj_tracker_pull_index(cobj_tracker_t *tracker, size_t index);

void cobj_tracker_pull_pointer(cobj_tracker_t *tracker, const void *ptr, size_t index);

void cobj_tracker_push(cobj_tracker_t *tracker, const void *ptr, size_t *index);

void cobj_tracker_reset_iterator(cobj_tracker_t *tracker);

void cobj_tracker_trim(cobj_tracker_t *tracker);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

unsigned long cobj_tracker_find(const cobj_tracker_t *tracker, const void *ptr, size_t *index);

size_t cobj_tracker_get_alloc_size(const cobj_tracker_t *tracker);

const void *cobj_tracker_get_index(const cobj_tracker_t *tracker, size_t index);

unsigned long cobj_tracker_get_index_n_ref(const cobj_tracker_t *tracker, size_t index);

const void *cobj_tracker_get_iteration(const cobj_tracker_t *tracker);

size_t cobj_tracker_get_iterator_offset(const cobj_tracker_t *tracker);

unsigned long cobj_tracker_get_iteration_n_ref(const cobj_tracker_t *tracker);

size_t cobj_tracker_get_size(const cobj_tracker_t *tracker);

bool cobj_tracker_has_failed(const cobj_tracker_t *tracker);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* COBJ_TRACKER_H */

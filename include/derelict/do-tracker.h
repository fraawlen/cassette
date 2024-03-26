/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Derelict Objects (DO) library.
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

#ifndef DO_TRACKER_H
#define DO_TRACKER_H

#include <stdbool.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

typedef struct _tracker_t do_tracker_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

do_tracker_t *do_tracker_create(size_t n_alloc);

void do_tracker_destroy(do_tracker_t **tracker);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void do_tracker_clear(do_tracker_t *tracker);

bool do_tracker_increment_iterator(do_tracker_t *tracker);

void do_tracker_lock_iterator(do_tracker_t *tracker);

void do_tracker_pull_index(do_tracker_t *tracker, size_t index);

void do_tracker_pull_pointer(do_tracker_t *tracker, const void *ptr, size_t index);

void do_tracker_push(do_tracker_t *tracker, const void *ptr, size_t *index);

void do_tracker_reset_iterator(do_tracker_t *tracker);

void do_tracker_trim(do_tracker_t *tracker);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

unsigned long do_tracker_find(const do_tracker_t *tracker, const void *ptr, size_t *index);

size_t do_tracker_get_alloc_size(const do_tracker_t *tracker);

const void *do_tracker_get_index(const do_tracker_t *tracker, size_t index);

unsigned long do_tracker_get_index_n_ref(const do_tracker_t *tracker, size_t index);

const void *do_tracker_get_iteration(const do_tracker_t *tracker);

unsigned long do_tracker_get_iteration_n_ref(const do_tracker_t *tracker);

size_t do_tracker_get_size(const do_tracker_t *tracker);

bool do_tracker_has_failed(const do_tracker_t *tracker);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DO_TRACKER_H */

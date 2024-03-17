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

#ifndef DU_TRACKER_H
#define DU_TRACKER_H

#include <stdbool.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

typedef struct _tracker_t du_tracker_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

du_tracker_t *du_tracker_create(size_t n_alloc);

void du_tracker_destroy(du_tracker_t **tracker);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/


void du_tracker_clear(du_tracker_t *tracker);

void du_tracker_pull_index(du_tracker_t *tracker, size_t index);

void du_tracker_pull_pointer(du_tracker_t *tracker, const void *ptr, size_t index);

void du_tracker_push(du_tracker_t *tracker, const void *ptr, size_t *index);

void du_tracker_reset_iterator(du_tracker_t *tracker);

void du_tracker_trim(du_tracker_t *tracker);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

unsigned long du_tracker_find(const du_tracker_t *tracker, const void *ptr, size_t *index);

size_t du_tracker_get_alloc_size(const du_tracker_t *tracker);

const void *du_tracker_get_index(const du_tracker_t *tracker, size_t index, unsigned long *n_ref);

const void *du_tracker_get_next(du_tracker_t *tracker, unsigned long *n_ref);

size_t du_tracker_get_size(const du_tracker_t *tracker);

bool du_tracker_has_failed(const du_tracker_t *tracker);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DU_TRACKER_H */

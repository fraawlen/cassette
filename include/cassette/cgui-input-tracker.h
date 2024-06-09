/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Graphics (CGUI) library.
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

#pragma once

#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

struct cgui_input_tracker_input_t
{
	unsigned int id;
	const void *ref;
	int x;
	int y;
};

typedef struct cgui_input_tracker_input_t cgui_input_tracker_input_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

typedef struct _input_tracker_t cgui_input_tracker_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgui_input_tracker_t *cgui_input_tracker_create(size_t max_inputs);

cgui_input_tracker_t *cgui_input_tracker_get_placeholder(void);

void cgui_input_tracker_destroy(cgui_input_tracker_t **inputs);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void cgui_input_tracker_clear(cgui_input_tracker_t *inputs);

bool cgui_input_tracker_increment_iterator(cgui_input_tracker_t *inputs);

void cgui_input_tracker_lock_iterator(cgui_input_tracker_t *inputs);

void cgui_input_tracker_pull_id(cgui_input_tracker_t *inputs, unsigned int id);

void cgui_input_tracker_pull_index(cgui_input_tracker_t *inputs, unsigned int index);

void cgui_input_tracker_push(cgui_input_tracker_t *inputs, unsigned int id, int x, int y, void *ref);

void cgui_input_tracker_reset_iterator(cgui_input_tracker_t *inputs);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool cgui_input_tracker_find(const cgui_input_tracker_t *inputs, unsigned int id, size_t *index);

size_t cgui_input_tracker_get_alloc_size(const cgui_input_tracker_t *inputs);

cgui_input_tracker_input_t cgui_input_tracker_get_index(const cgui_input_tracker_t *inputs, size_t index);

cgui_input_tracker_input_t cgui_input_tracker_get_iteration(const cgui_input_tracker_t *inputs);

size_t cgui_input_tracker_get_iterator_offset(const cgui_input_tracker_t *inputs);

size_t cgui_input_tracker_get_load(const cgui_input_tracker_t *inputs);

bool cgui_input_tracker_has_failed(const cgui_input_tracker_t *inputs);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

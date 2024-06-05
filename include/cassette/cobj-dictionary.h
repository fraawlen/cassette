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

#ifndef COBJ_DICTIONARY_H
#define COBJ_DICTIONARY_H

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

typedef struct _dictionary_t cobj_dictionary_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cobj_dictionary_t *cobj_dictionary_create(size_t n_alloc, double max_load);

cobj_dictionary_t *cobj_dictionary_get_placeholder(void);

void cobj_dictionary_destroy(cobj_dictionary_t **dict);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void cobj_dictionary_clear(cobj_dictionary_t *dict);

void cobj_dictionary_clear_group(cobj_dictionary_t *dict, unsigned int group);

void cobj_dictionary_erase(cobj_dictionary_t *dict, const char *key, unsigned int group);

void cobj_dictionary_write(cobj_dictionary_t *dict, const char *key, unsigned int group, size_t value);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool cobj_dictionary_find(const cobj_dictionary_t *dict, const char *key, unsigned int group, size_t *value);

size_t cobj_dictionary_get_alloc_size(const cobj_dictionary_t *dict);

size_t cobj_dictionary_get_load(const cobj_dictionary_t *dict);

double cobj_dictionary_get_load_factor(const cobj_dictionary_t *dict);

bool cobj_dictionary_has_failed(const cobj_dictionary_t *dict);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* COBJ_DICTIONARY_H */


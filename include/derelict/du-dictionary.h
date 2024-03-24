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

#ifndef DU_DICTIONARY_H
#define DU_DICTIONARY_H

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

typedef struct _dictionary_t du_dictionary_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

du_dictionary_t *du_dictionary_create(size_t n_alloc, double max_load);

void du_dictionary_destroy(du_dictionary_t **dict);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void du_dictionary_clear(du_dictionary_t *dict);

void du_dictionary_clear_group(du_dictionary_t *dict, unsigned int group);

void du_dictionary_erase(du_dictionary_t *dict, const char *key, unsigned int group);

void du_dictionary_write(du_dictionary_t *dict, const char *key, unsigned int group, size_t value);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool du_dictionary_find(const du_dictionary_t *dict, const char *key, unsigned int group, size_t *value);

size_t du_dictionary_get_alloc_size(const du_dictionary_t *dict);

size_t du_dictionary_get_load(const du_dictionary_t *dict);

double du_dictionary_get_load_factor(const du_dictionary_t *dict);

bool du_dictionary_has_failed(const du_dictionary_t *dict);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DU_DICTIONARY_H */


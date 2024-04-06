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

#ifndef DO_DICTIONARY_H
#define DO_DICTIONARY_H

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

typedef struct _dictionary_t do_dictionary_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

do_dictionary_t *do_dictionary_create(size_t n_alloc, double max_load);

void do_dictionary_destroy(do_dictionary_t **dict);

do_dictionary_t *do_dictionary_get_placeholder(void);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void do_dictionary_clear(do_dictionary_t *dict);

void do_dictionary_clear_group(do_dictionary_t *dict, unsigned int group);

void do_dictionary_erase(do_dictionary_t *dict, const char *key, unsigned int group);

void do_dictionary_write(do_dictionary_t *dict, const char *key, unsigned int group, size_t value);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool do_dictionary_find(const do_dictionary_t *dict, const char *key, unsigned int group, size_t *value);

size_t do_dictionary_get_alloc_size(const do_dictionary_t *dict);

size_t do_dictionary_get_load(const do_dictionary_t *dict);

double do_dictionary_get_load_factor(const do_dictionary_t *dict);

bool do_dictionary_has_failed(const do_dictionary_t *dict);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DO_DICTIONARY_H */


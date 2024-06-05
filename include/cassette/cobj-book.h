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

#ifndef COBJ_BOOK_H
#define COBJ_BOOK_H

#include <stdbool.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

typedef struct _book_t cobj_book_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cobj_book_group_mode_t
{
	COBJ_BOOK_OLD_GROUP = false,
	COBJ_BOOK_NEW_GROUP = true,
};

typedef enum cobj_book_group_mode_t cobj_book_group_mode_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cobj_book_t *cobj_book_create(size_t n_alloc, size_t word_n);

cobj_book_t *cobj_book_get_placeholder(void);

void cobj_book_destroy(cobj_book_t **book);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void cobj_book_clear(cobj_book_t *book);

void cobj_book_erase_last_group(cobj_book_t *book);

void cobj_book_erase_last_word(cobj_book_t *book);

bool cobj_book_increment_iterator(cobj_book_t *book);

void cobj_book_lock_iterator(cobj_book_t *book);

void cobj_book_reset_iterator(cobj_book_t *book, size_t group_index);

void cobj_book_rewrite_word(cobj_book_t *book, const char *str, size_t group_index, size_t word_index);

void cobj_book_trim(cobj_book_t *book);

void cobj_book_write_new_word(cobj_book_t *book, const char *str, cobj_book_group_mode_t group_mode);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

char *cobj_book_prepare_new_word(cobj_book_t *book, cobj_book_group_mode_t group_mode);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t cobj_book_get_alloc_words(const cobj_book_t *book);

size_t cobj_book_get_group_size(const cobj_book_t *book, size_t group_index);

const char *cobj_book_get_iteration(const cobj_book_t *book);

size_t cobj_book_get_iterator_group(const cobj_book_t *book);

size_t cobj_book_get_iterator_offset(const cobj_book_t *book);

size_t cobj_book_get_number_groups(const cobj_book_t *book);

size_t cobj_book_get_number_words(const cobj_book_t *book);

const char *cobj_book_get_word(const cobj_book_t *book, size_t group_index, size_t word_index);

size_t cobj_book_get_word_max_size(const cobj_book_t *book);

bool cobj_book_has_failed(const cobj_book_t *book);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* COBJ_BOOK_H */


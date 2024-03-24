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

#ifndef DU_BOOK_H
#define DU_BOOK_H

#include <stdbool.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

typedef struct _book_t du_book_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum du_book_group_mode_t
{
	DU_BOOK_OLD_GROUP = false,
	DU_BOOK_NEW_GROUP = true,
};

typedef enum du_book_group_mode_t du_book_group_mode_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

du_book_t *du_book_create(size_t n_alloc, size_t word_n);

void du_book_destroy(du_book_t **book);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void du_book_clear(du_book_t *book);

void du_book_erase_last_group(du_book_t *book);

void du_book_erase_last_word(du_book_t *book);

bool du_book_increment_iterator(du_book_t *book);

void du_book_reset_iterator(du_book_t *book, size_t group_index);

void du_book_rewrite_word(du_book_t *book, const char *str, size_t group_index, size_t word_index);

void du_book_trim(du_book_t *book);

void du_book_write_new_word(du_book_t *book, const char *str, du_book_group_mode_t group_mode);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

char *du_book_prepare_new_word(du_book_t *book, du_book_group_mode_t group_mode);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t du_book_get_alloc_words(const du_book_t *book);

size_t du_book_get_group_size(const du_book_t *book, size_t group_index);

const char *du_book_get_iteration(const du_book_t *book);

size_t du_book_get_number_groups(const du_book_t *book);

size_t du_book_get_number_words(const du_book_t *book);

const char *du_book_get_word(const du_book_t *book, size_t group_index, size_t word_index);

size_t du_book_get_word_max_size(const du_book_t *book);

bool du_book_has_failed(const du_book_t *book);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DU_BOOK_H */


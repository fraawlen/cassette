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

#ifndef DO_BOOK_H
#define DO_BOOK_H

#include <stdbool.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

typedef struct _book_t do_book_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum do_book_group_mode_t
{
	DO_BOOK_OLD_GROUP = false,
	DO_BOOK_NEW_GROUP = true,
};

typedef enum do_book_group_mode_t do_book_group_mode_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

do_book_t *do_book_create(size_t n_alloc, size_t word_n);

void do_book_destroy(do_book_t **book);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void do_book_clear(do_book_t *book);

void do_book_erase_last_group(do_book_t *book);

void do_book_erase_last_word(do_book_t *book);

bool do_book_increment_iterator(do_book_t *book);

void do_book_reset_iterator(do_book_t *book, size_t group_index);

void do_book_rewrite_word(do_book_t *book, const char *str, size_t group_index, size_t word_index);

void do_book_trim(do_book_t *book);

void do_book_write_new_word(do_book_t *book, const char *str, do_book_group_mode_t group_mode);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

char *do_book_prepare_new_word(do_book_t *book, do_book_group_mode_t group_mode);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t do_book_get_alloc_words(const do_book_t *book);

size_t do_book_get_group_size(const do_book_t *book, size_t group_index);

const char *do_book_get_iteration(const do_book_t *book);

size_t do_book_get_number_groups(const do_book_t *book);

size_t do_book_get_number_words(const do_book_t *book);

const char *do_book_get_word(const do_book_t *book, size_t group_index, size_t word_index);

size_t do_book_get_word_max_size(const do_book_t *book);

bool do_book_has_failed(const do_book_t *book);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DO_BOOK_H */


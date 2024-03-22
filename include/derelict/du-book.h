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

du_book_t *du_book_create(size_t n_alloc, size_t word_n);

void du_book_reset(du_book_t **book);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void du_book_clear(du_book_t *book);

void du_book_erase_last_group(du_book_t *book);

void du_book_erase_last_word(du_book_t *book);

void du_book_write_new_word(du_book_t *book, bool new_group, const char *str);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

char *du_book_get_group(const du_book_t *book, size_t index);

size_t du_book_get_group_length(const du_book_t *book, size_t index);

char *du_book_get_last_group(const du_book_t *book);

char *du_book_get_last_word(const du_book_t *book);

char *du_book_get_next_word(const du_book_t *book, char **word);

char *du_book_get_new_word(du_book_t *book, bool new_group);

char *du_book_get_word(const du_book_t *book, size_t index);

char *du_book_get_word_in_group(const du_book_t *book, size_t index_group, size_t index_word);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DU_BOOK_H */


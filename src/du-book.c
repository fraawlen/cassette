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

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "du.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static bool _extend (du_book_t *book);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
du_book_clear(du_book_t *book)
{
	assert(book);
	du_status_test(book->status, return);

	book->n_words  = 0;
	book->n_groups = 0;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

char *
du_book_get_next_word(const du_book_t *book, char **s)
{
	assert(book && s);
	du_status_test(book->status, return NULL);

	if (!*s) {
		return *s = book->words;
	}	

	if (*s < book->words || *s >= book->words + ((book->n_words - 1) * book->word_n)) {
		return NULL;
	}

	return *s = &book->words[((*s - book->words) / book->word_n + 1) * book->word_n];
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

char *
du_book_get_word(const du_book_t *book, size_t index)
{
	assert(book);
	du_status_test(book->status, return NULL);

	if (index >= book->n_words) {
		return NULL;
	}

	return &book->words[index * book->word_n];
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_book_init(du_book_t *book, size_t n_alloc, size_t word_n)
{
	assert(book && word_n > 0);

	book->n_words = 0;
	book->n_groups = 0;
	book->n_alloc = n_alloc;
	book->word_n = word_n;
	book->words  = n_alloc > 0 ? malloc(n_alloc * word_n) : NULL;
	book->groups = n_alloc > 0 ? malloc(n_alloc * sizeof(size_t)) : NULL;
	book->status = n_alloc == 0 || (book->groups && book->words) ? DU_STATUS_SUCCESS : DU_STATUS_FAILURE;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_book_reset(du_book_t *book)
{
	assert(book);

	free(book->words);
	free(book->groups);
	book->status = DU_STATUS_NOT_INIT;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

char *
du_book_start_next_word(du_book_t *book, bool new_group)
{
	assert(book);
	du_status_test(book->status, return NULL);

	if (book->n_words >= book->n_alloc && !_extend(book)) {
		return NULL;
	}

	if (new_group || book->n_words == 0) {
		book->groups[book->n_groups] = book->n_words;
		book->n_groups++;
	}

	return &book->words[(book->n_words++) * book->word_n];
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_book_write_next_word(du_book_t *book, bool new_group, const char *str)
{
	assert(book);
	du_status_test(book->status, return);

	char *word = du_book_start_next_word(book, new_group);
	if (word) {
		strncpy(word, str, book->word_n - 1);
		word[book->word_n - 1] = '\0';
	}
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static bool
_extend(du_book_t *book)
{
	const size_t n_alloc = book->n_alloc > 0 ? book->n_alloc * 2 : 1;

	void *tmp1 = realloc(book->words, n_alloc * book->word_n);
	du_status_assert(book->status, tmp1, return false);
	book->words = tmp1;

	void *tmp2 = realloc(book->groups, n_alloc * sizeof(size_t));
	du_status_assert(book->status, tmp2, return false);
	book->groups = tmp2;

	book->n_alloc = n_alloc;

	return true;
}

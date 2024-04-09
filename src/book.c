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

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <derelict/do.h>

#include "safe.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

struct _book_t
{
	char *words;
	size_t *groups;
	size_t word_n;
	size_t n_groups;
	size_t n_words;
	size_t n_alloc;
	size_t iterator_word;
	size_t iterator_group;
	bool failed;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static size_t _get_group_size (const do_book_t *book, size_t index);
static bool   _resize         (do_book_t *book, size_t n, size_t a, size_t b);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static do_book_t _err_book =
{
	.words          = NULL,
	.groups         = NULL,
	.word_n         = 0,
	.n_groups       = 0,
	.n_words        = 0,
	.n_alloc        = 0,
	.iterator_word  = SIZE_MAX,
	.iterator_group = SIZE_MAX,
	.failed         = true,
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
do_book_clear(do_book_t *book)
{
	assert(book);

	if (book->failed)
	{
		return;
	}

	book->n_words  = 0;
	book->n_groups = 0;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

do_book_t *
do_book_create(size_t n_alloc, size_t word_n)
{
	assert(word_n > 0);

	do_book_t *book;

	if (!(book = malloc(sizeof(do_book_t))))
	{
		return &_err_book;
	}

	book->words          = NULL;
	book->groups         = NULL;
	book->word_n         = word_n;
	book->n_groups       = 0;
	book->n_words        = 0;
	book->n_alloc        = 0;
	book->iterator_word  = SIZE_MAX;
	book->iterator_group = SIZE_MAX;
	book->failed         = false;

	_resize(book, n_alloc, 1, 0);

	return book;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
do_book_destroy(do_book_t **book)
{
	assert(book && *book);

	if (*book == &_err_book)
	{
		return;
	}

	free((*book)->words);
	free((*book)->groups);
	free(*book);

	*book = &_err_book;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
do_book_erase_last_group(do_book_t *book)
{
	assert(book);

	if (book->failed)
	{
		return;
	}

	if (book->n_groups == 0)
	{
		return;
	}

	book->n_words = book->groups[--book->n_groups];
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
do_book_erase_last_word(do_book_t *book)
{
	assert(book);

	if (book->failed)
	{
		return;
	}

	if (book->n_words == 0)
	{
		return;
	}

	if (book->groups[book->n_groups - 1] == --book->n_words)
	{
		book->n_groups--;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
do_book_get_alloc_words(const do_book_t *book)
{
	assert(book);

	if (book->failed)
	{
		return 0;
	}

	return book->n_alloc;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
do_book_get_group_size(const do_book_t *book, size_t group_index)
{
	assert(book);

	if (book->failed)
	{
		return 0;
	}

	return _get_group_size(book, group_index);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const char *
do_book_get_iteration(const do_book_t *book)
{
	assert(book);

	if (book->failed)
	{
		return "";
	}

	if (book->iterator_word == 0 || book->iterator_word > _get_group_size(book, book->iterator_group))
	{
		return "";
	}

	return book->words + (book->groups[book->iterator_group] + book->iterator_word - 1) * book->word_n;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
do_book_get_iterator_group(const do_book_t *book)
{
	assert(book);

	if (book->failed)
	{
		return SIZE_MAX;
	}

	if (book->iterator_group >= book->n_groups)
	{
		return SIZE_MAX;
	}

	return book->iterator_group;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
do_book_get_iterator_offset(const do_book_t *book)
{
	assert(book);

	if (book->failed)
	{
		return 0;
	}

	if (book->iterator_word > _get_group_size(book, book->iterator_group))
	{
		return 0;
	}

	return book->iterator_word;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
do_book_get_number_groups(const do_book_t *book)
{
	assert(book);

	if (book->failed)
	{
		return 0;
	}

	return book->n_groups;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
do_book_get_number_words(const do_book_t *book)
{
	assert(book);

	if (book->failed)
	{
		return 0;
	}

	return book->n_words;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

do_book_t *
do_book_get_placeholder(void)
{
	return &_err_book;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const char *
do_book_get_word(const do_book_t *book, size_t group_index, size_t word_index)
{
	assert(book);

	if (book->failed)
	{
		return "";
	}

	if (word_index >= _get_group_size(book, group_index))
	{
		return "";
	}

	return book->words + (book->groups[group_index] + word_index) * book->word_n;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
do_book_get_word_max_size(const do_book_t *book)
{
	assert(book);

	if (book->failed)
	{
		return 0;
	}

	return book->word_n;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
do_book_has_failed(const do_book_t *book)
{
	assert(book);

	return book->failed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
do_book_increment_iterator(do_book_t *book)
{
	assert(book);

	if (book->failed)
	{
		return false;
	}

	if (book->iterator_word >= _get_group_size(book, book->iterator_group))
	{
		return false;
	}

	book->iterator_word++;
	
	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
do_book_lock_iterator(do_book_t *book)
{
	assert(book);

	if (book->failed)
	{
		return;
	}

	book->iterator_group = SIZE_MAX;
	book->iterator_word  = SIZE_MAX;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

char *
do_book_prepare_new_word(do_book_t *book, do_book_group_mode_t group_mode)
{
	assert(book);

	if (book->failed)
	{
		return NULL;
	}

	if (book->n_words >= book->n_alloc && !_resize(book, book->n_alloc, 2, 1))
	{
		return NULL;
	}

	if (book->n_words == 0 || group_mode == DO_BOOK_NEW_GROUP)
	{
		book->groups[book->n_groups++] = book->n_words;
	}

	return book->words + book->n_words++ * book->word_n;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
do_book_reset_iterator(do_book_t *book, size_t group_index)
{
	assert(book);

	if (book->failed)
	{
		return;
	}

	if (group_index >= book->n_groups)
	{
		return;
	}

	book->iterator_word  = 0;
	book->iterator_group = group_index;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
do_book_rewrite_word(do_book_t *book, const char *str, size_t group_index, size_t word_index)
{
	char *word;

	assert(book);

	if (book->failed)
	{
		return;
	}

	if (word_index >= _get_group_size(book, group_index))
	{
		return;
	}

	word = book->words + (book->groups[group_index] + word_index) * book->word_n;

	snprintf(word, book->word_n, "%s", str);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
do_book_trim(do_book_t *book)
{
	assert(book);

	if (book->failed)
	{
		return;
	}

	_resize(book, book->n_words, 1, 0);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
do_book_write_new_word(do_book_t *book, const char *str, do_book_group_mode_t group_mode)
{
	char *word;

	assert(book);

	if (!str)
	{
		return;
	}
	
	if ((word = do_book_prepare_new_word(book, group_mode)))
	{
		snprintf(word, book->word_n, "%s", str);
	}
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static size_t
_get_group_size(const do_book_t *book, size_t index)
{
	if (book->n_groups == 0 || index >= book->n_groups)
	{
		return 0;
	}
	else if (index == book->n_groups - 1)
	{
		return book->n_words - book->groups[index];
	}
	else
	{
		return book->groups[index + 1] - book->groups[index];
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
_resize(do_book_t *book, size_t n, size_t a, size_t b)
{
	bool    safe = true;
	char   *tmp_1;
	size_t *tmp_2;	

	/* test for overflow */

	safe &= do_safe_mul(&n,   n, a);
	safe &= do_safe_add(&n,   n, b);
	safe &= do_safe_mul(NULL, n, book->word_n);
	safe &= do_safe_mul(NULL, n, sizeof(size_t));

	if (!safe)
	{
		book->failed = true;
		return false;
	}

	/* resize array */

	if (n == 0)
	{
		free(book->words);
		free(book->groups);
		book->words  = NULL;
		book->groups = NULL;
	}
	else
	{	
		if (!(tmp_1 = realloc(book->words, n * book->word_n)))
		{
			book->failed = true;
			return false;
		}
		book->words = tmp_1;
	
		if (!(tmp_2 = realloc(book->groups, n * sizeof(size_t))))
		{
			book->failed = true;
			return false;
		}	
		book->groups = tmp_2;
	}

	book->n_alloc = n;

	return true;
}

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

#include <derelict/du.h>

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
	bool failed;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static bool _resize(du_book_t *book, size_t n, size_t a, size_t b);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static du_book_t _err_book =
{
	.words    = NULL,
	.groups   = NULL,
	.word_n   = 0,
	.n_groups = 0,
	.n_words  = 0,
	.n_alloc  = 0,
	.failed   = true,
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

du_book_t *
du_book_create(size_t n_alloc, size_t word_n)
{
	assert(word_n > 0);

	du_book_t *book;

	if (!(book = malloc(sizeof(du_book_t))))
	{
		return &_err_book;
	}

	book->words    = NULL;
	book->groups   = NULL;
	book->word_n   = word_n;
	book->n_groups = 0;
	book->n_words  = 0;
	book->n_alloc  = 0;
	book->failed   = false;

	_resize(book, n_alloc, 1, 0);

	return book;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_book_reset(du_book_t **book)
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

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static bool
_resize(du_book_t *book, size_t n, size_t a, size_t b)
{
	bool    safe = true;
	char   *tmp_1;
	size_t *tmp_2;	

	/* test for overflow */

	safe &= du_safe_mul(&n,   n, a);
	safe &= du_safe_add(&n,   n, b);
	safe &= du_safe_mul(NULL, n, book->word_n);
	safe &= du_safe_mul(NULL, n, sizeof(size_t));

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

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

#include <cassette/cobj.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "safe.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

struct cbook
{
	char *chars;
	size_t *words;
	size_t *groups;
	size_t n_chars;
	size_t n_words;
	size_t n_groups;
	size_t n_alloc_chars;
	size_t n_alloc_words;
	size_t n_alloc_groups;
	size_t it_word;
	size_t it_group;
	enum cbook_err err;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static size_t _group_size (const cbook *book, size_t i)                       CBOOK_PURE CBOOK_NONNULL(1);
static bool   _grow       (cbook *book, size_t n_chars, size_t n_words, size_t n_groups) CBOOK_NONNULL(1);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

cbook cbook_placeholder_instance =
{
	.chars          = NULL,
	.words          = NULL,
	.groups         = NULL,
	.n_chars        = 0,
	.n_words        = 0,
	.n_groups       = 0,
	.n_alloc_chars  = 0,
	.n_alloc_words  = 0,
	.n_alloc_groups = 0,
	.it_word        = SIZE_MAX,
	.it_group       = SIZE_MAX,
	.err            = CBOOK_INVALID,
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

size_t
cbook_byte_length(const cbook *book)
{
	if (book->err)
	{
		return 0;
	}

	return book->n_chars;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cbook_clear(cbook *book)
{
	if (book->err)
	{
		return;
	}

	book->n_groups = 0;
	book->n_words  = 0;
	book->n_chars  = 0;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cbook *
cbook_clone(const cbook *book)
{
	cbook *book_new;

	if (book->err || !(book_new = malloc(sizeof(cbook))))
	{
		goto fail_book;
	}

	if (!(book_new->chars = malloc(book->n_alloc_chars)))
	{
		goto fail_chars;
	}

	if (!(book_new->words = malloc(book->n_alloc_words * sizeof(size_t))))
	{
		goto fail_words;
	}

	if (!(book_new->groups = malloc(book->n_alloc_groups * sizeof(size_t))))
	{
		goto fail_groups;
	}

	memcpy(book_new->chars,  book->chars,  book->n_chars);
	memcpy(book_new->words,  book->words,  book->n_words  * sizeof(size_t));
	memcpy(book_new->groups, book->groups, book->n_groups * sizeof(size_t));

	book_new->n_chars        = book->n_chars;
	book_new->n_words        = book->n_words;
	book_new->n_groups       = book->n_groups;
	book_new->n_alloc_chars  = book->n_alloc_chars;
	book_new->n_alloc_words  = book->n_alloc_words;
	book_new->n_alloc_groups = book->n_alloc_groups;
	book_new->it_word        = book->it_word;
	book_new->it_group       = book->it_group;
	book_new->err            = CBOOK_OK;

	return book_new;

	/* err */

fail_groups:
	free(book_new->words);
fail_words:
	free(book_new->chars);
fail_chars:
	free(book_new);
fail_book:
	return CBOOK_PLACEHOLDER;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cbook *
cbook_create(void)
{
	cbook *book;

	if (!(book = malloc(sizeof(cbook))))
	{
		goto fail_book;
	}

	if (!(book->chars = malloc(1)))
	{
		goto fail_chars;
	}

	if (!(book->words = malloc(sizeof(size_t))))
	{
		goto fail_words;
	}

	if (!(book->groups = malloc(sizeof(size_t))))
	{
		goto fail_groups;
	}

	book->chars[0]       = '\0';
	book->words[0]       = 0;
	book->groups[0]      = 0;
	book->n_chars        = 0;
	book->n_words        = 0;
	book->n_groups       = 0;
	book->n_alloc_chars  = 1;
	book->n_alloc_words  = 1;
	book->n_alloc_groups = 1;
	book->it_word        = SIZE_MAX;
	book->it_group       = SIZE_MAX;
	book->err            = CBOOK_OK;

	return book;

	/* err */

fail_groups:
	free(book->words);
fail_words:
	free(book->chars);
fail_chars:
	free(book);
fail_book:
	return CBOOK_PLACEHOLDER;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cbook_destroy(cbook *book)
{
	if (book == CBOOK_PLACEHOLDER)
	{
		return;
	}

	free(book->groups);
	free(book->words);
	free(book->chars);
	free(book);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cbook_err
cbook_error(const cbook *book)
{
	return book->err;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
cbook_group_length(const cbook *book, size_t group_index)
{
	if (book->err)
	{
		return 0;
	}

	return _group_size(book, group_index);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
cbook_groups_number(const cbook *book)
{
	if (book->err)
	{
		return 0;
	}

	return book->n_groups;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cbook_init_iterator(cbook *book, size_t group_index)
{
	if (book->err)
	{
		return;
	}

	if (group_index >= book->n_groups)
	{
		book->it_word  = SIZE_MAX;
		book->it_group = SIZE_MAX;
	}
	else
	{
		book->it_word  = 0;
		book->it_group = group_index;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cbook_iterate(cbook *book)
{
	if (book->err || book->it_word >= _group_size(book, book->it_group))
	{
		return false;
	}

	book->it_word++;

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const char *
cbook_iteration(const cbook *book)
{
	if (book->err || book->it_word == 0 || book->it_word > _group_size(book, book->it_group))
	{
		return "";
	}

	return book->chars + book->words[book->groups[book->it_group] + book->it_word - 1];
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
cbook_iterator_group(const cbook *book)
{
	if (book->err || book->it_group >= book->n_groups)
	{
		return SIZE_MAX;
	}

	return book->it_group;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
cbook_iterator_offset(const cbook *book)
{
	if (book->err || book->it_group >= book->n_groups)
	{
		return 0;
	}

	if (book->it_word > _group_size(book, book->it_group))
	{
		return _group_size(book, book->it_group);
	}

	return book->it_word;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cbook_lock_iterator(cbook *book)
{
	if (book->err)
	{
		return;
	}

	book->it_word  = SIZE_MAX;
	book->it_group = SIZE_MAX;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cbook_pop_group(cbook *book)
{
	if (book->err || book->n_groups == 0)
	{
		return;
	}

	book->n_chars = book->words[book->groups[--book->n_groups]];
	book->n_words = book->groups[book->n_groups];
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cbook_pop_word(cbook *book)
{
	if (book->err || book->n_words == 0)
	{
		return;
	}

	if (book->groups[book->n_groups - 1] == book->n_words - 1)
	{
		book->n_groups--;
	}

	book->n_chars = book->words[--book->n_words];
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cbook_prealloc(cbook *book, size_t bytes_number, size_t words_number, size_t groups_number)
{
	if (book->err)
	{
		return;
	}

	_grow(book, bytes_number, words_number, groups_number);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

char *
cbook_prepare_word(cbook *book, size_t length, enum cbook_group group_mode)
{
	size_t nc;
	size_t nw;
	size_t ng;

	if (book->err)
	{
		return NULL;
	}

	nc = book->n_alloc_chars;
	nw = book->n_alloc_words  * (book->n_words  >= book->n_alloc_words  ? 2 : 1);
	ng = book->n_alloc_groups * (book->n_groups >= book->n_alloc_groups ? 2 : 1);

	while (length > nc - book->n_chars)
	{
		if (!safe_mul(&nc, nc, 2))
		{
			book->err |= CBOOK_OVERFLOW;
			return NULL;
		}
	}

	if (!_grow(book, nc, nw, ng))
	{
		return NULL;
	}

	if (group_mode == CBOOK_NEW || book->n_groups == 0)
	{
		book->groups[book->n_groups++] = book->n_words;
	}

	book->words[book->n_words] = book->n_chars;
	book->n_chars += length;

	return book->chars + book->words[book->n_words++];
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cbook_repair(cbook *book)
{
	book->err &= CBOOK_INVALID;
}	

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const char *
cbook_word(const cbook *book, size_t word_index)
{
	if (book->err || word_index >= book->n_words)
	{
		return "";
	}

	return book->chars + book->words[word_index];
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const char *
cbook_word_in_group(const cbook *book, size_t group_index, size_t word_index)
{
	if (book->err
	 || _group_size(book, group_index) == 0
	 || _group_size(book, group_index) <= word_index)
	{
		return "";
	}

	return book->chars + book->words[book->groups[group_index] + word_index];
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
cbook_words_number(const cbook *book)
{
	if (book->err)
	{
		return 0;
	}

	return book->n_words;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cbook_write(cbook *book, const char *str, enum cbook_group group_mode)
{
	size_t n;
	char *buf;

	n = strlen(str) + 1;

	if ((buf = cbook_prepare_word(book, n, group_mode)))
	{
		memcpy(buf, str, n);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cbook_zero(cbook *book)
{
	if (book->err)
	{
		return;
	}

	memset(book->chars, '\0', book->n_alloc_chars);

	book->n_groups = 0;
	book->n_words  = 0;
	book->n_chars  = 0;
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static size_t
_group_size(const cbook *book, size_t i)
{
	if (i >= book->n_groups)
	{
		return 0;
	}
	else if (i == book->n_groups - 1)
	{
		return book->n_words - book->groups[i];
	}
	else
	{
		return book->groups[i + 1] - book->groups[i];
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
_grow(cbook *book, size_t n_chars, size_t n_words, size_t n_groups)
{
	void *tmp;

	if (!safe_mul(NULL, n_words,  sizeof(size_t))
	 || !safe_mul(NULL, n_groups, sizeof(size_t)))
	{
		book->err |= CBOOK_OVERFLOW;
		return false;
	}

	/* char */

	if (n_chars > book->n_alloc_chars)
	{
		if (!(tmp = realloc(book->chars, n_chars)))
		{
			book->err |= CBOOK_MEMORY;
			return false;
		}
		book->n_alloc_chars = n_chars;
		book->chars = tmp;
	}

	/* word indexes */

	if (n_words > book->n_alloc_words)
	{
		if (!(tmp = realloc(book->words, n_words * sizeof(size_t))))
		{
			book->err |= CBOOK_MEMORY;
			return false;
		}
		book->n_alloc_words = n_words;
		book->words = tmp;
	}

	/* group indexes */

	if (n_groups > book->n_alloc_groups)
	{
		if (!(tmp = realloc(book->groups, n_groups * sizeof(size_t))))
		{
			book->err |= CBOOK_MEMORY;
			return false;
		}
		book->n_alloc_groups = n_groups;
		book->groups = tmp;
	}

	/* end */

	return true;
}

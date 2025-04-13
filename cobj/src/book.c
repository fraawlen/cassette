/**
 * Copyright © 2024-2025 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette library.
 *
 * This library is free software; you can redistribute it and/or modify it either under the terms of the GNU
 * Lesser General Public License as published by the Free Software Foundation; either version 3.0 of the
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
#include <stdckdint.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define GUARD(OBJ, ...) if (!OBJ || cerr_critical(OBJ->err)) { return __VA_OPT__(__VA_ARGS__); }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/


struct cbook
{
	char *bytes;
	size_t *words;
	size_t *groups;
	size_t n_bytes;
	size_t n_words;
	size_t n_groups;
	size_t n_alloc_bytes;
	size_t n_alloc_words;
	size_t n_alloc_groups;
	bool new_group;
	enum cerr err;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static size_t group_size (const cbook *, size_t);
static bool   grow       (cbook *, size_t, size_t, size_t);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cbook_clear(cbook *book)
{
	GUARD(book);

	book->n_groups  = 0;
	book->n_words   = 0;
	book->n_bytes   = 0;
	book->new_group = true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cbook_clear_warnings(cbook *book)
{
	GUARD(book);

	cerr_clear_warnings(&book->err);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cbook *
cbook_clone(const cbook *book)
{
	GUARD(book, nullptr);

	cbook *book_new;

	if (!(book_new = calloc(1, sizeof(cbook))))
	{
		return nullptr;
	}

	if (!grow(book_new, book->n_alloc_bytes, book->n_alloc_words, book->n_alloc_groups))
	{
		free(book_new->bytes);
		free(book_new->words);
		free(book_new->groups);
		free(book_new);
		return nullptr;
	}

	memcpy(book_new->bytes,  book->bytes,  book->n_bytes);
	memcpy(book_new->words,  book->words,  book->n_words  * sizeof(size_t));
	memcpy(book_new->groups, book->groups, book->n_groups * sizeof(size_t));

	book_new->n_bytes   = book->n_bytes;
	book_new->n_words   = book->n_words;
	book_new->n_groups  = book->n_groups;
	book_new->new_group = book->new_group;
	book_new->err       = book->err;

	return book_new;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cbook *
cbook_create(void)
{
	cbook *book;

	if (!(book = calloc(1, sizeof(cbook))))
	{
		return nullptr;
	}

	if (!grow(book, 1, 1, 1))
	{
		free(book->bytes);
		free(book->words);
		free(book->groups);
		free(book);
		return nullptr;
	}

	book->n_bytes   = 0;
	book->n_words   = 0;
	book->n_groups  = 0;
	book->new_group = true;
	book->err       = CERR_NONE;

	return book;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

nullptr_t
cbook_destroy(cbook *book)
{
	if (book)
	{
		free(book->groups);
		free(book->words);
		free(book->bytes);
		free(book);
	}

	return nullptr;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cerr
cbook_error(const cbook *book)
{
	return book ? book->err : CERR_INVALID;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
cbook_group_length(const cbook *book, size_t group_index)
{
	GUARD(book, 0);

	return group_size(book, group_index);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
cbook_groups_number(const cbook *book)
{
	GUARD(book, 0);

	return book->n_groups;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
cbook_length(const cbook *book)
{
	GUARD(book, 0);

	return book->n_bytes;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cbook_pop_group(cbook *book)
{
	GUARD(book);

	if (book->n_groups == 0)
	{
		return;
	}

	book->n_bytes = book->words[book->groups[--book->n_groups]];
	book->n_words = book->groups[book->n_groups];

	if (book->n_groups == 0)
	{
		book->new_group = true;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cbook_pop_word(cbook *book)
{
	GUARD(book);

	if (book->n_words == 0)
	{
		return;
	}

	if (book->groups[book->n_groups - 1] == book->n_words - 1)
	{
		if (--book->n_groups == 0)
		{
			book->new_group = true;
		}
	}

	book->n_bytes = book->words[--book->n_words];
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cbook_prealloc(cbook *book, size_t bytes_number, size_t words_number, size_t groups_number)
{
	GUARD(book);

	grow(book, bytes_number, words_number, groups_number);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cbook_prepare_new_group(cbook *book)
{
	GUARD(book);

	book->new_group = true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cbook_undo_new_group(cbook *book)
{
	GUARD(book);

	if (book->n_groups > 0)
	{
		book->new_group = false;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const char *
cbook_word(const cbook *book, size_t word_index)
{
	GUARD(book, "");

	if (word_index >= book->n_words)
	{
		return "";
	}

	return book->bytes + book->words[word_index];
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const char *
cbook_word_in_group(const cbook *book, size_t group_index, size_t word_local_index)
{
	GUARD(book, "");

	if (group_size(book, group_index) == 0
	 || group_size(book, group_index) <= word_local_index)
	{
		return "";
	}

	return book->bytes + book->words[book->groups[group_index] + word_local_index];
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
cbook_word_index(const cbook *book, size_t group_index, size_t word_local_index)
{
	GUARD(book, 0);

	if (group_size(book, group_index) == 0
	 || group_size(book, group_index) <= word_local_index)
	{
		return 0;
	}

	return book->groups[group_index] + word_local_index;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
cbook_words_number(const cbook *book)
{
	GUARD(book, 0);

	return book->n_words;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cbook_write(cbook *book, const char *str)
{
	GUARD(book);

	size_t ns = strlen(str) + 1;
	size_t nc = book->n_alloc_bytes;
	size_t nw = book->n_alloc_words  * (book->n_words  >= book->n_alloc_words  ? 2 : 1);
	size_t ng = book->n_alloc_groups * (book->n_groups >= book->n_alloc_groups ? 2 : 1);

	while (ns > nc - book->n_bytes)
	{
		if (ckd_mul(&nc, nc, 2))
		{
			cerr_set(&book->err, CERR_OVERFLOW);
			return;
		}
	}

	if (!grow(book, nc, nw, ng))
	{
		return;
	}

	if (book->new_group)
	{
		book->groups[book->n_groups++] = book->n_words;
		book->new_group = false;
	}

	memmove(book->bytes + book->n_bytes, str, ns);
	book->words[book->n_words++] = book->n_bytes;
	book->n_bytes += ns;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cbook_zero(cbook *book)
{
	GUARD(book);

	memset(book->bytes, '\0', book->n_alloc_bytes);

	book->n_groups = 0;
	book->n_words  = 0;
	book->n_bytes  = 0;
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static size_t
group_size(const cbook *book, size_t i)
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
grow(cbook *book, size_t n_bytes, size_t n_words, size_t n_groups)
{
	size_t n = 0;

	n_bytes  > book->n_alloc_bytes  ? n++ : (n_bytes  = book->n_alloc_bytes);
	n_words  > book->n_alloc_words  ? n++ : (n_words  = book->n_alloc_words);
	n_groups > book->n_alloc_groups ? n++ : (n_groups = book->n_alloc_groups);

	return
		   n > 0
		&& CUTIL_REALLOC(book->bytes,  book->n_alloc_bytes,  n_bytes,  1,              book->err)
		&& CUTIL_REALLOC(book->words,  book->n_alloc_words,  n_words,  sizeof(size_t), book->err)
		&& CUTIL_REALLOC(book->groups, book->n_alloc_groups, n_groups, sizeof(size_t), book->err);
}

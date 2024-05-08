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

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#include <cassette/cobj.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void _print_all_groups (cobj_book_t *book);
static void _print_group      (cobj_book_t *book, size_t group);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

int
main(void)
{
	cobj_book_t *book;

	/* init */

	book = cobj_book_create(0, 32);

	/* operations */

	cobj_book_write_new_word(book, "Hello",   COBJ_BOOK_OLD_GROUP);
	cobj_book_write_new_word(book, "world",   COBJ_BOOK_OLD_GROUP);
	cobj_book_write_new_word(book, "This",    COBJ_BOOK_NEW_GROUP);
	cobj_book_write_new_word(book, "is",      COBJ_BOOK_OLD_GROUP);
	cobj_book_write_new_word(book, "a",       COBJ_BOOK_OLD_GROUP);
	cobj_book_write_new_word(book, "new",     COBJ_BOOK_OLD_GROUP);
	cobj_book_write_new_word(book, "group",   COBJ_BOOK_OLD_GROUP);
	cobj_book_write_new_word(book, "Another", COBJ_BOOK_NEW_GROUP);
	cobj_book_write_new_word(book, "one",     COBJ_BOOK_OLD_GROUP);
	cobj_book_write_new_word(book, "Last",    COBJ_BOOK_NEW_GROUP);
	cobj_book_write_new_word(book, "group",   COBJ_BOOK_OLD_GROUP);
	cobj_book_write_new_word(book, "of",      COBJ_BOOK_OLD_GROUP);
	cobj_book_write_new_word(book, "the",     COBJ_BOOK_OLD_GROUP);
	cobj_book_write_new_word(book, "book",    COBJ_BOOK_OLD_GROUP);
	_print_all_groups(book);
	_print_group(book, 8);

	printf("book allocated word before trimming : %zu\n",   cobj_book_get_alloc_words(book));
	cobj_book_trim(book);
	printf("book allocated word after  trimming : %zu\n\n", cobj_book_get_alloc_words(book));

	cobj_book_rewrite_word(book, "HELLO",         0, 0);
	cobj_book_rewrite_word(book, "WORLD",         0, 1);
	cobj_book_rewrite_word(book, "OUT OF BOUNDS", 0, 2);
	_print_all_groups(book);

	printf("\n4rd word of group 1 is : %s\n", cobj_book_get_word(book, 1, 3));

	cobj_book_erase_last_word(book);
	_print_all_groups(book);

	cobj_book_erase_last_group(book);
	_print_all_groups(book);

	cobj_book_clear(book);
	_print_all_groups(book);

	/* end */

	if (cobj_book_has_failed(book))
	{
		printf("Book has failed during operation.\n");
	}

	cobj_book_destroy(&book);
	cobj_book_destroy(&book); /* api is safe against double destructions */

	return 0;
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static void
_print_all_groups(cobj_book_t *book)
{
	for (size_t i = 0; i < cobj_book_get_number_groups(book); i++)
	{
		_print_group(book, i);
	}

	printf("\n");
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_print_group(cobj_book_t *book, size_t group)
{
	printf(
		"group %zu/%zu containts (%zu/%zu/%zu) words):\n",
		group,
		cobj_book_get_number_groups(book),
		cobj_book_get_group_size(book, group),
		cobj_book_get_number_words(book),
		cobj_book_get_alloc_words(book));

	cobj_book_reset_iterator(book, group);

	while (cobj_book_increment_iterator(book))
	{
		/* safe from NULL values inside this loop */
		printf("\t%s", cobj_book_get_iteration(book));
	}

	printf("\n");
}

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

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#include <derelict/do.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void _print_all_groups (do_book_t *book);
static void _print_group      (do_book_t *book, size_t group);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

int
main(void)
{
	do_book_t *book;

	/* init */

	book = do_book_create(0, 32);

	/* operations */

	do_book_write_new_word(book, "Hello",   DO_BOOK_OLD_GROUP);
	do_book_write_new_word(book, "world",   DO_BOOK_OLD_GROUP);
	do_book_write_new_word(book, "This",    DO_BOOK_NEW_GROUP);
	do_book_write_new_word(book, "is",      DO_BOOK_OLD_GROUP);
	do_book_write_new_word(book, "a",       DO_BOOK_OLD_GROUP);
	do_book_write_new_word(book, "new",     DO_BOOK_OLD_GROUP);
	do_book_write_new_word(book, "group",   DO_BOOK_OLD_GROUP);
	do_book_write_new_word(book, "Another", DO_BOOK_NEW_GROUP);
	do_book_write_new_word(book, "one",     DO_BOOK_OLD_GROUP);
	do_book_write_new_word(book, "Last",    DO_BOOK_NEW_GROUP);
	do_book_write_new_word(book, "group",   DO_BOOK_OLD_GROUP);
	do_book_write_new_word(book, "of",      DO_BOOK_OLD_GROUP);
	do_book_write_new_word(book, "the",     DO_BOOK_OLD_GROUP);
	do_book_write_new_word(book, "book",    DO_BOOK_OLD_GROUP);
	_print_all_groups(book);
	_print_group(book, 8);

	printf("book allocated word before trimming : %zu\n",   do_book_get_alloc_words(book));
	do_book_trim(book);
	printf("book allocated word after  trimming : %zu\n\n", do_book_get_alloc_words(book));

	do_book_rewrite_word(book, "HELLO",         0, 0);
	do_book_rewrite_word(book, "WORLD",         0, 1);
	do_book_rewrite_word(book, "OUT OF BOUNDS", 0, 2);
	_print_all_groups(book);

	printf("\n4rd word of group 1 is : %s\n", do_book_get_word(book, 1, 3));

	do_book_erase_last_word(book);
	_print_all_groups(book);

	do_book_erase_last_group(book);
	_print_all_groups(book);

	do_book_clear(book);
	_print_all_groups(book);

	/* end */

	if (do_book_has_failed(book))
	{
		printf("Book has failed during operation.\n");
	}

	do_book_destroy(&book);
	do_book_destroy(&book); /* api is safe against double destructions */

	return 0;
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static void
_print_all_groups(do_book_t *book)
{
	for (size_t i = 0; i < do_book_get_number_groups(book); i++)
	{
		_print_group(book, i);
	}

	printf("\n");
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_print_group(do_book_t *book, size_t group)
{
	printf(
		"group %zu/%zu containts (%zu/%zu/%zu) words):\n",
		group,
		do_book_get_number_groups(book),
		do_book_get_group_size(book, group),
		do_book_get_number_words(book),
		do_book_get_alloc_words(book));

	do_book_reset_iterator(book, group);

	while (do_book_increment_iterator(book))
	{
		/* safe from NULL values inside this loop */
		printf("\t%s", do_book_get_iteration(book));
	}

	printf("\n");
}

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

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#include <derelict/du.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void _print_all_groups (du_book_t *book);
static void _print_group      (du_book_t *book, size_t group);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

int
main(void)
{
	du_book_t *book;

	/* init */

	book = du_book_create(0, 32);

	/* operations */

	du_book_write_new_word(book, "Hello",   DU_BOOK_OLD_GROUP);
	du_book_write_new_word(book, "world",   DU_BOOK_OLD_GROUP);
	du_book_write_new_word(book, "This",    DU_BOOK_NEW_GROUP);
	du_book_write_new_word(book, "is",      DU_BOOK_OLD_GROUP);
	du_book_write_new_word(book, "a",       DU_BOOK_OLD_GROUP);
	du_book_write_new_word(book, "new",     DU_BOOK_OLD_GROUP);
	du_book_write_new_word(book, "group",   DU_BOOK_OLD_GROUP);
	du_book_write_new_word(book, "Another", DU_BOOK_NEW_GROUP);
	du_book_write_new_word(book, "one",     DU_BOOK_OLD_GROUP);
	du_book_write_new_word(book, "Last",    DU_BOOK_NEW_GROUP);
	du_book_write_new_word(book, "group",   DU_BOOK_OLD_GROUP);
	du_book_write_new_word(book, "of",      DU_BOOK_OLD_GROUP);
	du_book_write_new_word(book, "the",     DU_BOOK_OLD_GROUP);
	du_book_write_new_word(book, "book",    DU_BOOK_OLD_GROUP);
	_print_all_groups(book);
	_print_group(book, 8);

	printf("book allocated word before trimming : %zu\n",   du_book_get_alloc_words(book));
	du_book_trim(book);
	printf("book allocated word after  trimming : %zu\n\n", du_book_get_alloc_words(book));

	du_book_rewrite_word(book, "HELLO",         0, 0);
	du_book_rewrite_word(book, "WORLD",         0, 1);
	du_book_rewrite_word(book, "OUT OF BOUNDS", 0, 2);
	_print_all_groups(book);

	printf("\n4rd word of group 1 is : %s\n", du_book_get_word(book, 1, 3));

	du_book_erase_last_word(book);
	_print_all_groups(book);

	du_book_erase_last_group(book);
	_print_all_groups(book);

	du_book_clear(book);
	_print_all_groups(book);

	/* end */

	if (du_book_has_failed(book))
	{
		printf("Book has failed during operation.\n");
	}

	du_book_destroy(&book);
	du_book_destroy(&book); /* api is safe against double destructions */

	return 0;

}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static void
_print_all_groups(du_book_t *book)
{
	for (size_t i = 0; i < du_book_get_number_groups(book); i++)
	{
		_print_group(book, i);
	}

	printf("\n");
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_print_group(du_book_t *book, size_t group)
{
	printf(
		"group %zu/%zu containts (%zu/%zu/%zu) words):\n",
		group,
		du_book_get_number_groups(book),
		du_book_get_group_size(book, group),
		du_book_get_number_words(book),
		du_book_get_alloc_words(book));

	du_book_reset_iterator(book, group);

	while (du_book_increment_iterator(book))
	{
		/* safe from NULL values inside this loop */
		printf("\t%s", du_book_get_iteration(book));
	}

	printf("\n");
}

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
#include <stdio.h>
#include <stdlib.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void print_stats (void);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static cbook *book = CBOOK_PLACEHOLDER;

/************************************************************************************************************/
/* MAIN *****************************************************************************************************/
/************************************************************************************************************/

int
main(void)
{
	/* Setup */

	book = cbook_create();

	/* Operations */

	cbook_write(book, "test1");
	cbook_write(book, "test2");
	cbook_write(book, "test3");

	cbook_prepare_new_group(book);
	cbook_write(book, "test4");
	cbook_write(book, "test5");
	cbook_write(book, "test6");
	cbook_write(book, "test7");

	cbook_prepare_new_group(book);
	cbook_write(book, "test8");

	cbook_prepare_new_group(book);
	cbook_undo_new_group(book);
	cbook_write(book, "test9");
	cbook_write(book, "test0");

	print_stats();

	for (size_t i = 0; i < cbook_groups_number(book); i++)
	{
		printf("[ GROUP %zu ]\n", i);
		CBOOK_FOR_EACH_REV(book, i, j)
		{
			printf("\t%s\n", cbook_word(book, j));
		}
	}

	printf("%zu\n", cbook_group_length(book, 0));
	printf("%zu\n", cbook_group_length(book, 1));
	printf("%zu\n", cbook_group_length(book, 2));
	printf("%zu\n", cbook_group_length(book, 3));
	printf("%zu\n", cbook_group_length(book, 4));

	printf("%s\n", cbook_word_in_group(book, 3, 0));

	/* End */

	if (cbook_error(book))
	{
		printf("Book errored during operation\n");	
	}

	cbook_destroy(book);

	return 0;
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
print_stats(void)
{
	printf(
		"%zu groups / %zu words / %zu chars\n",
		cbook_groups_number(book),
		cbook_words_number(book),
		cbook_length(book));
}

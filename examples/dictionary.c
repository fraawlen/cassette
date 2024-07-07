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

static void _print_stats (void);
static void _print_value (const char *key, unsigned int group);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static cbook *_book = CBOOK_PLACEHOLDER;
static cdict *_dict = CDICT_PLACEHOLDER;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

int
main(void)
{
	/* Setup */

	_book = cbook_create();
	_dict = cdict_create();

	/* Book operations */

	printf(">> %zu\n", cbook_group_length(_book, 0));
	printf(">> %s\n", cbook_word_in_group(_book, 0, 0));

	cbook_write(_book, "test1", CBOOK_NEW);
	cbook_write(_book, "test2", CBOOK_OLD);
	cbook_write(_book, "test3", CBOOK_OLD);
	cbook_write(_book, "test4", CBOOK_NEW);
	cbook_write(_book, "test5", CBOOK_OLD);
	cbook_write(_book, "test6", CBOOK_OLD);
	cbook_write(_book, "test7", CBOOK_OLD);
	cbook_write(_book, "test8", CBOOK_NEW);
	cbook_write(_book, "test9", CBOOK_NEW);
	cbook_write(_book, "test0", CBOOK_OLD);

	for (size_t i = 0; i < cbook_groups_number(_book); i++)
	{
		printf("[ GROUP %zu ]\n", i);
		CBOOK_FOR_EACH_REV(_book, i, j)
		{
			printf("\t%s\n", cbook_word(_book, j));
		}
	}

	/* Dict operations */

	printf(">> %zu\n", cbook_group_length(_book, 0));
	printf(">> %zu\n", cbook_group_length(_book, 1));
	printf(">> %zu\n", cbook_group_length(_book, 2));
	printf(">> %zu\n", cbook_group_length(_book, 3));
	printf(">> %zu\n", cbook_group_length(_book, 4));

	printf(">> %s\n", cbook_word_in_group(_book, 3, 0));

	cdict_write(_dict, "test", 0, 12);
	cdict_write(_dict, "test", 1, 32);
	cdict_write(_dict, "test", 1, 44);
	cdict_write(_dict, "AAAA", 0, 99);
	cdict_write(_dict, "ASXC", 0, 56);

	cdict_erase(_dict, "AAAA", 0);
	cdict_clear_group(_dict, 0);

	_print_stats();

	_print_value("test", 0);
	_print_value("test", 1);
	_print_value("AAAA", 0);
	_print_value("ASXC", 0);

	/* End */

	if (cbook_error(_book))
	{
		printf("Book errored during operation\n");	
	}

	if (cdict_error(_dict))
	{
		printf("Dictionary errored during operation\n");	
	}

	cbook_destroy(_book);
	cdict_destroy(_dict);

	return 0;
}

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void
_print_stats(void)
{
	printf(
		"DICT\t%zu used slots (%f load factor)\n",
		cdict_load(_dict),
		cdict_load_factor(_dict));

	printf(
		"BOOK\t%zu groups / %zu words / %zu chars\n",
		cbook_groups_number(_book),
		cbook_words_number(_book),
		cbook_length(_book));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_print_value(const char *key, unsigned int group)
{
	size_t value = 0;

	if (cdict_find(_dict, key, group, &value))
	{
		printf("%s\t%u\t%zu\n", key, group, value);
	}
}

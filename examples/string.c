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

static void _print_str(cobj_string_t *str, const char *comment);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

int
main(void)
{
	cobj_string_t *str;

	const char *codepoint;
	size_t i = 0;

	/* init */

	str = cobj_string_create();

	/* operations */

	cobj_string_set_raw(str, "t↑↑t\ntest\ntest");
	_print_str(str, "set initial value");

	cobj_string_insert_raw(str, "insertion\n", 5);
	_print_str(str, "insertion at codepoint offset 5");

	cobj_string_cut(str, 0, 5);
	_print_str(str, "cut 5 codepoints at offset 0");

	cobj_string_wrap(str, 4);
	_print_str(str, "wrapped to 4 columns");

	cobj_string_append_raw(str, "\nok");
	_print_str(str, "append");

	cobj_string_slice(str, 5, 6);
	_print_str(str, "slice with offset 4 and length 6");

	cobj_string_prepend_raw(str, "    \t\t");
	_print_str(str, "appended whitespace");

	cobj_string_trim(str);
	_print_str(str, "trimmed whitespaces");

	cobj_string_pad(str, "→", 0, 10);
	_print_str(str, "padded beginning of string with dots to reach a length of 10");

	cobj_string_realloc(str);
	_print_str(str, "reallocted memory to get rid off excess unused space");

	/* manual iteration */

	codepoint = cobj_string_get_chars(str);
	while (*codepoint != '\0')
	{
		i++;
		codepoint = cobj_string_seek_next_codepoint(codepoint);
	}

	printf("\t-> counted %zu codepoints manually\n", i);

	/* end */

	if (cobj_string_has_failed(str))
	{
		printf("\t->string failed during operation\n");
	}

	cobj_string_destroy(&str);
	cobj_string_destroy(&str); /* api is safe against double destructions */

	return 0;
}

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void
_print_str(cobj_string_t *str, const char *comment)
{
	printf(
		"%s\n\t-> %zux%zu / %zu / %zu (%s)\n",
		cobj_string_get_chars(str),
		cobj_string_get_height(str),
		cobj_string_get_width(str),
		cobj_string_get_length(str),
		cobj_string_get_alloc_size(str),
		comment);
}

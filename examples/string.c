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

static void _print_str(du_string_t *str, const char *comment);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

int
main(void)
{
	/* init */

	du_string_t *str = du_string_create();

	/* operations */

	du_string_set_raw(str, "test\ntest");
	_print_str(str, "set initial value");

	du_string_attach_raw(str, "\nTEST", DU_STRING_TAIL);
	_print_str(str, "append \"\\nTEST\"");

	du_string_attach_raw(str, "hehehe", DU_STRING_LEAD);
	_print_str(str, "prepend \"hehehe\"");

	du_string_pad(str, "_", 22, DU_STRING_LEAD);
	_print_str(str, "left padded with \"_\"");

	du_string_pad(str, "_", 24, DU_STRING_TAIL);
	_print_str(str, "right padded with \"_\"");

	du_string_wrap(str, 5);
	_print_str(str, "wrapped string after 5th column");

	du_string_trim(str, 2, DU_STRING_LEAD);
	_print_str(str, "trimmed 2 codepoints at the beginning");

	du_string_trim(str, 5, DU_STRING_TAIL);
	_print_str(str, "trimmed 5 codepoints at the end");

	du_string_attach_raw(str, "   ", DU_STRING_LEAD);
	du_string_attach_raw(str, "   ", DU_STRING_TAIL);
	_print_str(str, "attached 3 spaces at the beginning and end of string");

	du_string_trim_whitespaces(str);
	_print_str(str, "trimmed leading and trailing whitespaces");

	du_string_limit(str, 9, DU_STRING_LEAD);
	_print_str(str, "limited the string's length to 9 codepoints from the left");

	du_string_t *str2 = du_string_create_slice(str, 6, 2, DU_STRING_LEAD);
	_print_str(str2, "created a slice (of size 6 and offset 2 from the left) of the original string");

	du_string_attach(str, str2, DU_STRING_LEAD);
	_print_str(str, "attached the slice to the beginning of the main string");

	du_string_insert_raw(str, "OYA", 4, DU_STRING_LEAD);
	_print_str(str, "inserted \"OYA\" after the 4th position from the left");

	du_string_insert_raw(str, "OYA", 2, DU_STRING_TAIL);
	_print_str(str, "inserted \"OYA\" after the 2th position from the right");

	du_string_cut(str, 5, 5, DU_STRING_LEAD);
	_print_str(str, "cut 5 codepoints offseted by 5 from the left");

	du_string_cut(str, 5, 5, DU_STRING_TAIL);
	_print_str(str, "cut 5 codepoints offseted by 5 from the right");

	du_string_clear(str);
	_print_str(str, "cleared string of all content");

	du_string_t *str3 = du_string_create_double(3405.234523, 2);
	_print_str(str3, "created string from double");

	/* end */

	if (du_string_has_failed(str))
	{
		printf("string failed during operation\n");
	}

	du_string_destroy(&str);
	du_string_destroy(&str2);
	du_string_destroy(&str3);
	du_string_destroy(&str3); /* safe against double destructions / free */

	return 0;
}

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void
_print_str(du_string_t *str, const char *comment)
{
	printf(
		"%s\n\t-> %lix%li / %li / %li (%s)\n",
		du_string_get_chars(str),
		du_string_get_height(str),
		du_string_get_width(str),
		du_string_get_length(str),
		du_string_get_alloc_size(str),
		comment);
}

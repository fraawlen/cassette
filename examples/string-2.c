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

static void _print_str(do_string_t *str, const char *comment);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

int
main(void)
{
	do_string_t *str_og;
	do_string_t *str_wrap;

	size_t offset_og;
	size_t offset_wrap;

	/* init */

	str_og   = do_string_create();
	str_wrap = do_string_create();

	do_string_set_raw(str_og, "This is a loooooooooooong line of text !");
	do_string_set(str_wrap, str_og);
	do_string_wrap(str_wrap, 10);

	_print_str(str_og,   "original string");
	_print_str(str_wrap, "wrapped string");

	/* get insersion offset for wrapped string */

	offset_wrap = do_string_convert_coords_to_offset(str_wrap, 2, 5);

	/* convert that offset to match the original string */

	offset_og = do_string_convert_wrapped_offset(str_og, str_wrap, offset_wrap);

	/* insert new text in the original string and update the wrapped one */

	do_string_insert_raw(str_og, "ER", offset_og);
	do_string_set(str_wrap, str_og);
	do_string_wrap(str_wrap, 10);

	_print_str(str_og,   "original string with insertion based on wrapped string coords");
	_print_str(str_wrap, "updated wrapped string");

	/* end */

	do_string_destroy(&str_og);
	do_string_destroy(&str_wrap);

	return 0;
}

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void
_print_str(do_string_t *str, const char *comment)
{
	printf(
		"%s\n\t-> %zux%zu / %zu / %zu (%s)\n\n",
		do_string_get_chars(str),
		do_string_get_height(str),
		do_string_get_width(str),
		do_string_get_length(str),
		do_string_get_alloc_size(str),
		comment);
}

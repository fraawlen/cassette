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

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void _print (const struct cline line);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

int
main(void)
{
	struct cline line = CLINE_DEFAULT;
	cstr *str = cstr_create();

	cline_limit(&line, 20, -20);
	cline_move(&line, 7);
	cline_resize(&line, -5);
	cline_grow(&line, -30);
	cline_offset(&line, -7);

	_print(line);

	cstr_set_tab_width(str, 4);
	cstr_append(str, "→est");
	cstr_pad(str, "Ͳ", 1, 10);
	cstr_insert(str, "\t", 0);
	cstr_insert(str, "\t", 0);
	cstr_wrap(str, 6);
	printf("%s (%zu)\n", cstr_chars(str), cstr_width(str));
	printf("%zu\n", cstr_coords_offset(str, 0, 1));
	printf("%zu\n", cstr_coords_offset(str, 0, 2));
	printf("%zu\n", cstr_coords_offset(str, 0, 3));
	printf("%zu\n", cstr_coords_offset(str, 0, 4));
	printf("%zu\n", cstr_coords_offset(str, 0, 5));
	printf("%zu\n\n", cstr_coords_offset(str, 0, 6));
	printf("%zu\n", cstr_coords_offset(str, 1, 1));
	printf("%zu\n", cstr_coords_offset(str, 1, 2));
	printf("%zu\n", cstr_coords_offset(str, 1, 3));
	printf("%zu\n", cstr_coords_offset(str, 1, 4));
	printf("%zu\n", cstr_coords_offset(str, 1, 5));
	printf("%zu\n", cstr_coords_offset(str, 1, 6));
	printf("%zu\n", cstr_coords_offset(str, 1, 7));
	printf("%zu\n\n", cstr_coords_offset(str, 1, 8));
	printf("%s\n", cstr_chars_at_coords(str, 0, 8));

	return 0;
}

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void
_print(const struct cline line)
{
	printf("%li <= %li + %li <= %li\n", line.min, line.origin, line.length, line.max);
}


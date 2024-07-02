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

	cline_limit(&line, 20, -20);
	cline_move(&line, 7);
	cline_resize(&line, -5);
	cline_grow(&line, -30);
	cline_offset(&line, -7);

	_print(line);

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


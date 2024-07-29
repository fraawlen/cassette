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

static void _print (const struct cseg seg);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

int
main(void)
{
	struct cseg seg = CSEG_I8;

	cseg_limit(&seg, 20, -20);
	cseg_move(&seg, 7);
	cseg_resize(&seg, -5);
	cseg_grow(&seg, -30);
	cseg_offset(&seg, -7);

	_print(seg);

	return 0;
}

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void
_print(const struct cseg seg)
{
	printf("%li <= %li + %li <= %li\n", seg.min, seg.origin, seg.length, seg.max);
}


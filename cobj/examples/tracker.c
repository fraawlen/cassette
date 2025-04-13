/**
 * Copyright © 2024-2025 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette library.
 *
 * This library is free software; you can redistribute it and/or modify it either under the terms of the GNU
 * Lesser General Public License as published by the Free Software Foundation; either version 3.0 of the
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
#include <stdlib.h>

#include <cassette/cobj.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

cref *refs = nullptr;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

int
main(void)
{
	unsigned int n;
	size_t i = 0;

	int a = 10;
	int b = 20;
	int c = 30;
	int d = 40;
	int e = 50;
	int f = 60;

	/* Setup */

	refs = cref_create();

	/* Operation */

	cref_push(refs, &a);
	cref_push(refs, &b);
	cref_push(refs, &b);
	cref_push(refs, &b);
	cref_push(refs, &c);
	cref_push(refs, &c);
	cref_push(refs, &d);
	cref_push(refs, &e);
	cref_push(refs, &f);

	if ((n = cref_find(refs, &b, &i)) > 0)
	{
		printf("Ref B was found at index %zu with %u counts\n", i, n);
	}

	cref_purge(refs, &b);
	cref_pull(refs, &c);
	cref_pull(refs, 0);

	CREF_FOR_EACH(refs, j)
	{
		printf(
			"%i / %u refs / %p\n",
			*(int*)cref_ptr(refs, j),
			cref_count(refs, j),
			cref_ptr(refs, j));
	}

	/* End */
	
	if (cref_error(refs))
	{
		printf("Reference counter errored during operation (%s)\n", cerr_name(cref_error(refs)));
	}

	refs = cref_destroy(refs);

	return 0;
}

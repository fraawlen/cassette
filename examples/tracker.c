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
#include <stdlib.h>

#include <cassette/cobj.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

cref *_refs = CREF_PLACEHOLDER;

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

	_refs = cref_create();

	/* Operation */

	cref_push(_refs, &a);
	cref_push(_refs, &b);
	cref_push(_refs, &b);
	cref_push(_refs, &b);
	cref_push(_refs, &c);
	cref_push(_refs, &c);
	cref_push(_refs, &d);
	cref_push(_refs, &e);
	cref_push(_refs, &f);

	if ((n = cref_find(_refs, &b, &i)) > 0)
	{
		printf("Ref B was found at index %zu with %u counts\n", i, n);
	}

	cref_purge(_refs, &b);
	cref_pull(_refs, &c);
	cref_pull(_refs, 0);

	CREF_FOR_EACH(_refs, j)
	{
		printf(
			"%i / %u refs / %p\n",
			*(int*)cref_ptr(_refs, j),
			cref_count(_refs, j),
			cref_ptr(_refs, j));
	}

	/* End */
	
	if (cref_error(_refs))
	{
		printf("Reference tracker has failed during operation.\n");
	}
	
	cref_destroy(_refs);

	return 0;
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/



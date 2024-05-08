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

static void _print_contents (cobj_tracker_t *tracker, const char *header);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

int
main(void)
{
	cobj_tracker_t *tracker;

	size_t i;

	/* Variables to track, can be anything, including arrays or structures */
	/* It is expected however to use a single data-type in a given tracker */
	
	int a = 1;
	int b = 20;
	int c = 3;
	int d = 65;
	int e = 234;

	/* Init */

	tracker = cobj_tracker_create(0);

	cobj_tracker_push(tracker, &a, NULL);
	cobj_tracker_push(tracker, &b, NULL);
	cobj_tracker_push(tracker, &c, NULL);
	cobj_tracker_push(tracker, &d, NULL);
	cobj_tracker_push(tracker, &e, NULL);
	cobj_tracker_push(tracker, &e, NULL); /* duplicate, increments reference counter */

	_print_contents(tracker, "tracker initialised with");

	/* Remove some components */

	cobj_tracker_pull_index(tracker, 0);
	cobj_tracker_pull_pointer(tracker, &e, 0);
	cobj_tracker_pull_pointer(tracker, &d, 0);
	cobj_tracker_pull_pointer(tracker, &d, 0); /* no effect because the pointer has already been removed */

	_print_contents(tracker, "a few components have been removed");

	/* Print info & trim allocated memory */

	printf(
		"size / allocated slots : %zu / %zu\n",
		cobj_tracker_get_size(tracker), 
		cobj_tracker_get_alloc_size(tracker));

	cobj_tracker_trim(tracker);

	printf(
		"size / allocated slots : %zu / %zu (after trimming)\n\n",
		cobj_tracker_get_size(tracker), 
		cobj_tracker_get_alloc_size(tracker));

	/* Re-add previously removed element */
	
	cobj_tracker_push(tracker, &d, NULL);
	
	_print_contents(tracker, "previously removed component has been re-added");

	/*
	 * Check if a pointer is tracked
	 * the parameter i, while optional, should be initialised if given because it acts both as the
	 * container to store an index if the element was found but also as a starting point for the internal
	 * array scan. Usefull if you need to often check if a specific element is present.
	 */

	i = 0;

	if (cobj_tracker_find(tracker, &a, &i))
	{
		printf("component with value %i was found at index %zu\n\n", a, i);
	}
	else
	{
		printf("component with value %i was not found\n\n", a);
	}

	/* Untrack all values */

	cobj_tracker_clear(tracker);

	_print_contents(tracker, "tracker has been cleared");

	/* end */
	
	if (cobj_tracker_has_failed(tracker))
	{
		printf("Tracker has failed during operation.\n");
	}
	
	cobj_tracker_destroy(&tracker);
	cobj_tracker_destroy(&tracker); /* api is safe against double destructions */

	return 0;
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static void
_print_contents(cobj_tracker_t *tracker, const char *header)
{
	printf("%s :\n", header);

	/* Check the number of tracked pointers. */

	if (cobj_tracker_get_size(tracker) == 0)
	{
		printf("\tempty");
	}

	/* Get each pointer sequencially, cast them to int and print them. */
	/* Always reset the iterator beforehand.                           */

	cobj_tracker_reset_iterator(tracker);

	while(cobj_tracker_increment_iterator(tracker))
	{
		/* safe from NULL values inside this loop */
		printf(
			"\t%i(%lu)",
			*(int*)cobj_tracker_get_iteration(tracker),
			cobj_tracker_get_iteration_n_ref(tracker));
	}

	printf("\n\n");
}

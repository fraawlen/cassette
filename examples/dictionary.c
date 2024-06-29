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

static cdict *_dict = CDICT_PLACEHOLDER;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

int
main(void)
{
	/* Setup */

	_dict = cdict_create();

	/* Operations */

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

	if (cdict_error(_dict))
	{
		printf("Dictionary errored during operation\n");	
	}

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
		"%zu used slots (%f load factor) / %zu allocated slots\n",
		cdict_load(_dict),
		cdict_load_factor(_dict),
		cdict_allocated_slots(_dict));	
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

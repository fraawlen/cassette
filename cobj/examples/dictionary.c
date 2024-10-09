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

static void print_stats (void);
static void print_value (const char *key, unsigned int group);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static cdict *dict = CDICT_PLACEHOLDER;

/************************************************************************************************************/
/* MAIN *****************************************************************************************************/
/************************************************************************************************************/

int
main(void)
{
	/* Setup */

	dict = cdict_create();

	/* Operations */

	cdict_write(dict, "test", 0, 12);
	cdict_write(dict, "test", 1, 32);
	cdict_write(dict, "test", 1, 44);
	cdict_write(dict, "AAAA", 0, 99);
	cdict_write(dict, "ASXC", 0, 56);

	cdict_erase(dict, "AAAA", 0);
	cdict_clear_group(dict, 0);

	print_stats();

	print_value("test", 0);
	print_value("test", 1);
	print_value("AAAA", 0);
	print_value("ASXC", 0);

	/* End */

	if (cdict_error(dict))
	{
		printf("Dictionary errored during operation\n");	
	}

	cdict_destroy(dict);

	return 0;
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
print_stats(void)
{
	printf(
		"%zu used slots (%f load factor)\n",
		cdict_load(dict),
		cdict_load_factor(dict));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
print_value(const char *key, unsigned int group)
{
	size_t value = 0;

	if (cdict_find(dict, key, group, &value))
	{
		printf("%s\t%u\t%zu\n", key, group, value);
	}
}

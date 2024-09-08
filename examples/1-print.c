/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Configuration (CCFG) library.
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

#include <cassette/ccfg.h>
#include <stdio.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void print_resources (const char *, const char *);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static ccfg *cfg = CCFG_PLACEHOLDER;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const char *data =
	"LET ratio 0.5\n"
	"\n"
	"example a TIME\n"
	"example b 45.5\n"
	"example c 50 60 70\n"
	"example d TRUE FALSE\n"
	"example e new_value\n"
	"example f ($$ example_param)\n"
	"example g (CITRPL #000000 #ffff8000 ($ ratio))\n"
	"example h ($ ratio)";

/************************************************************************************************************/
/* MAIN *****************************************************************************************************/
/************************************************************************************************************/

/**
 * In this 1st example, a simple resource look-up is done after loading the configuration. If found, the 
 * values associated with the resource are printed to stdout.
 */

int
main(void)
{
	/* Setup */

	cfg = ccfg_create();

	ccfg_push_param(cfg, "example_param", "value_from_executable");

	/* Operations */

	ccfg_load_internal(cfg, data);

	printf("namespace\tprop\tsize\traw_values\n");
	printf("---------\t----\t----\t----------\n");

	print_resources("example-1", "a");
	print_resources("example-1", "b");
	print_resources("example-1", "c");
	print_resources("example-1", "d");
	print_resources("example-1", "e");
	print_resources("example-1", "f");
	print_resources("example-1", "g");
	print_resources("example-1", "h");
	print_resources("example-1", "i"); /* expected to not be found */

	/* End */

	if (ccfg_error(cfg))
	{
		printf("Configuration parser failed during operation.\n");
	}

	return 0;
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static void
print_resources(const char *namespace, const char *property)
{
	ccfg_fetch(cfg, namespace, property);

	printf("%s\t%s\t%zu", namespace, property, ccfg_resource_length(cfg));
	while (ccfg_iterate(cfg))
	{
		printf("\t%s", ccfg_resource(cfg));
	}

	printf("\n");
}

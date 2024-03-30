/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Derelict Resources (DR) library.
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

#include <limits.h>
#include <pwd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <derelict/do.h>
#include <derelict/dr.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void _build_source_filename (do_string_t *str);
static void _callback              (dr_config_t *cfg);
static void _print_resource        (dr_config_t *cfg, const char *namespace, const char *property);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

int
main(void)
{
	dr_config_t *cfg;
	do_string_t *str;

	/* init */

	cfg = dr_config_create(0);
	str = do_string_create();

	_build_source_filename(str);

	dr_config_push_source(cfg, do_string_get_chars(str));
	dr_config_push_callback_load(cfg, _callback);

	/* operations */

	dr_config_load(cfg);

	_print_resource(cfg, "node-0", "coords");
	_print_resource(cfg, "node-1", "coords");
	_print_resource(cfg, "node-2", "coords");

	/* end */

	if (dr_config_has_failed(cfg))
	{
		printf("Configuration has failed during operation.\n");
	}

	do_string_destroy(&str);
	dr_config_destroy(&cfg);

	return 0;
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static void
_build_source_filename(do_string_t *str)
{
	char  home[PATH_MAX];
	char *env;

	env = getenv("HOME");
	
	strncpy(home, env ? env : getpwuid(getuid())->pw_dir, PATH_MAX - 1);
	home[PATH_MAX - 1] = '\0';

	do_string_set_raw(str, home);
	do_string_append_raw(str, "/A");
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_callback(dr_config_t *cfg)
{
	if (!dr_config_has_failed(cfg))
	{
		printf("\nconfiguration loaded successfully\n");
	}
	else
	{
		printf("\nconfiguration failed to load\n");
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_print_resource(dr_config_t *cfg, const char *namespace, const char *property)
{
	size_t n;

	dr_resource_fetch(cfg, namespace, property);

	if ((n = dr_resource_get_size(cfg)) > 0)
	{
		printf("\nresource %s::%s values (%zu) :\n", namespace, property, n);
		while (dr_resource_pick_next_value(cfg))
		{
			printf("\t%s\n", dr_resource_convert_to_string(cfg));
		}
	}
	else
	{
		printf("\nresource %s::%s was not found\n", namespace, property);
	}
}

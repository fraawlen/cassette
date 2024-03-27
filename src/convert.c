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

#include <assert.h>
#include <float.h>
#include <limits.h>
#include <stdbool.h>
#include <stdlib.h>

#include <derelict/do.h>
#include <derelict/dr.h>

#include "config.h"
#include "util.h"

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

bool
dr_convert_to_bool(const dr_config_t *cfg)
{
	assert(cfg);

	return strtod(dr_config_get_resource(cfg), NULL) != 0;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

do_color_t
dr_convert_to_color(const dr_config_t *cfg)
{
	const char *raw_value;

	assert(cfg);

	raw_value = dr_config_get_resource(cfg);

	if ((raw_value)[0] == '#')
	{
		return do_color_convert_hex_str(raw_value, NULL);
	}
	else
	{
		return do_color_convert_argb_uint(strtoul(raw_value, NULL, 0));
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

double
dr_convert_to_double(const dr_config_t *cfg)
{
	assert(cfg);

	return strtod(dr_config_get_resource(cfg), NULL);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

long
dr_convert_to_long(const dr_config_t *cfg)
{
	assert(cfg);

	return strtol(dr_config_get_resource(cfg), NULL, 0);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

double
dr_convert_to_range(const dr_config_t *cfg, double min, double max)
{
	assert(cfg);

	return dr_util_limit(strtod(dr_config_get_resource(cfg), NULL), min, max);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
dr_convert_to_reference(const dr_config_t *cfg, const do_dictionary_t *dict, size_t group, size_t fallback)
{
	size_t value;

	assert(cfg && dict);

	if (cfg->failed || do_dictionary_has_failed(dict))
	{
		return fallback;
	}

	if (!do_dictionary_find(dict, dr_config_get_resource(cfg), group, &value))
	{
		return fallback;
	}
	
	return value;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

unsigned long
dr_convert_to_ulong(const dr_config_t *cfg)
{
	assert(cfg);

	return strtoul(dr_config_get_resource(cfg), NULL, 0);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

double
dr_convert_to_udouble(const dr_config_t *cfg)
{
	assert(cfg);

	return dr_util_limit(strtod(dr_config_get_resource(cfg), NULL), 0, DBL_MAX);
}


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
dr_resource_convert_to_bool(const dr_config_t *cfg)
{
	assert(cfg);
	
	if (cfg->failed)
	{
		return false;
	}

	return strtod(do_book_get_iteration(cfg->sequences), NULL) != 0;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

do_color_t
dr_resource_convert_to_color(const dr_config_t *cfg)
{
	const char *raw_value;

	assert(cfg);

	if (cfg->failed)
	{
		return DO_COLOR_TRANSPARENT;
	}

	raw_value = do_book_get_iteration(cfg->sequences);

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
dr_resource_convert_to_double(const dr_config_t *cfg)
{
	assert(cfg);

	if (cfg->failed)
	{
		return 0.0;
	}

	return strtod(do_book_get_iteration(cfg->sequences), NULL);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

long
dr_resource_convert_to_long(const dr_config_t *cfg)
{
	assert(cfg);

	if (cfg->failed)
	{
		return 0;
	}

	return strtol(do_book_get_iteration(cfg->sequences), NULL, 0);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

double
dr_resource_convert_to_range(const dr_config_t *cfg, double min, double max)
{
	assert(cfg);

	if (cfg->failed)
	{
		return min;
	}

	return dr_util_limit(strtod(do_book_get_iteration(cfg->sequences), NULL), min, max);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

double
dr_resource_convert_to_ratio(const dr_config_t *cfg)
{
	assert(cfg);

	if (cfg->failed)
	{
		return 0.0;
	}

	return dr_util_limit(strtod(do_book_get_iteration(cfg->sequences), NULL), 0.0, 1.0);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
dr_resource_convert_to_reference(const dr_config_t *cfg, const do_dictionary_t *dict, size_t group, size_t fallback)
{
	size_t value;

	assert(cfg && dict);

	if (cfg->failed || do_dictionary_has_failed(dict))
	{
		return fallback;
	}

	if (!do_dictionary_find(dict, do_book_get_iteration(cfg->sequences), group, &value))
	{
		return fallback;
	}
	
	return value;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const char *
dr_resource_convert_to_string(const dr_config_t *cfg)
{
	assert(cfg);
	
	if (cfg->failed)
	{
		return "";
	}

	return do_book_get_iteration(cfg->sequences);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

double
dr_resource_convert_to_udouble(const dr_config_t *cfg)
{
	assert(cfg);

	if (cfg->failed)
	{
		return 0.0;
	}

	return dr_util_limit(strtod(do_book_get_iteration(cfg->sequences), NULL), 0, DBL_MAX);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

unsigned long
dr_resource_convert_to_ulong(const dr_config_t *cfg)
{
	assert(cfg);

	if (cfg->failed)
	{
		return 0;
	}

	return strtoul(do_book_get_iteration(cfg->sequences), NULL, 0);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
dr_resource_fetch(dr_config_t *cfg, const char *namespace, const char *property)
{
	size_t i_namespace;
	size_t i_prop;

	assert(cfg);

	if (cfg->failed)
	{
		return false;
	}

	do_book_lock_iterator(cfg->sequences);

	if (!namespace || namespace[0] == '\0')
	{
		return false;
	}

	if (!property || property[0] == '\0')
	{
		return false;
	}

	if (!do_dictionary_find(cfg->ref_sequences, namespace, 0, &i_namespace))
	{
		return false;
	}

	if (!do_dictionary_find(cfg->ref_sequences, property, i_namespace, &i_prop))
	{
		return false;
	}

	do_book_reset_iterator(cfg->sequences, i_prop);

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
dr_resource_get_size(const dr_config_t *cfg)
{
	assert(cfg);

	if (cfg->failed)
	{
		return 0;
	}

	return do_book_get_group_size(cfg->sequences, do_book_get_iterator_group(cfg->sequences));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/


bool
dr_resource_pick_next_value(dr_config_t *cfg)
{
	assert(cfg);
	
	if (cfg->failed)
	{
		return false;
	}

	return do_book_increment_iterator(cfg->sequences);
}

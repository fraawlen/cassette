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

#include <cassette/ccfg.h>
#include <cassette/cobj.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

#include "main.h"
#include "source.h"
#include "token.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define GUARD(OBJ, ...) if (!OBJ || cerr_critical(OBJ->err)) { return __VA_OPT__(__VA_ARGS__); }

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static const char *select_source (const ccfg *, size_t *);
static void        update_err    (ccfg *);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

bool
ccfg_can_open_sources(const ccfg *cfg, size_t *index)
{
	GUARD(cfg, false);

	return select_source(cfg, index)[0] != '\0';
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ccfg_clear_resources(ccfg *cfg)
{
	GUARD(cfg);

	cbook_clear(cfg->sequences);
	cdict_clear(cfg->keys_sequences);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ccfg_clear_params(ccfg *cfg)
{
	GUARD(cfg);

	cbook_clear(cfg->params);
	cdict_clear(cfg->keys_params);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ccfg_clear_sources(ccfg *cfg)
{
	GUARD(cfg);

	cbook_clear(cfg->sources);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ccfg_clear_warnings(ccfg *cfg)
{
	GUARD(cfg);

	cerr_clear_warnings(&cfg->err);
	cbook_clear_warnings(cfg->params);
	cbook_clear_warnings(cfg->sequences);
	cbook_clear_warnings(cfg->sources);
	cdict_clear_warnings(cfg->keys_params);
	cdict_clear_warnings(cfg->keys_sequences);
	cdict_clear_warnings(cfg->tokens);
	
	update_err(cfg);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wanalyzer-malloc-leak"
#pragma GCC diagnostic ignored "-Wanalyzer-mismatching-deallocation"
#pragma GCC diagnostic ignored "-Wanalyzer-use-of-uninitialized-value"
#pragma GCC diagnostic ignored "-Wmismatched-dealloc"

ccfg *
ccfg_clone(ccfg *cfg)
{
	GUARD(cfg, nullptr);

	ccfg *cfg_new;

	if (!(cfg_new = malloc(sizeof(ccfg))))
	{
		return nullptr;
	}

	cfg_new->params         = cbook_clone(cfg->params);
	cfg_new->sequences      = cbook_clone(cfg->sequences);
	cfg_new->sources        = cbook_clone(cfg->sources);
	cfg_new->keys_params    = cdict_clone(cfg->keys_params);
	cfg_new->keys_sequences = cdict_clone(cfg->keys_sequences);
	cfg_new->tokens         = cdict_clone(cfg->tokens);
	cfg_new->it_group       = cfg->it_group;
	cfg_new->it             = cfg->it;
	cfg_new->loads          = cfg->loads;
	cfg_new->restricted     = cfg->restricted;
	cfg_new->err            = cfg->err;
	
	update_err(cfg_new);

	return cerr_critical(cfg_new->err) ? ccfg_destroy(cfg_new) : cfg_new;
}

#pragma GCC diagnostic pop

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wanalyzer-malloc-leak"
#pragma GCC diagnostic ignored "-Wanalyzer-mismatching-deallocation"
#pragma GCC diagnostic ignored "-Wanalyzer-use-of-uninitialized-value"
#pragma GCC diagnostic ignored "-Wmismatched-dealloc"

ccfg *
ccfg_create(void)
{
	ccfg *cfg;

	if (!(cfg = malloc(sizeof(ccfg))))
	{
		return nullptr;
	}

	cfg->params         = cbook_create();
	cfg->sequences      = cbook_create();
	cfg->sources        = cbook_create();
	cfg->keys_params    = cdict_create();
	cfg->keys_sequences = cdict_create();
	cfg->tokens         = token_dict_create();
	cfg->it_group       = SIZE_MAX;
	cfg->it             = SIZE_MAX;
	cfg->loads          = 0;
	cfg->restricted     = false;
	cfg->err            = CERR_NONE;

	update_err(cfg);

	return cerr_critical(cfg->err) ? ccfg_destroy(cfg) : cfg;
}

#pragma GCC diagnostic pop

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wanalyzer-use-of-uninitialized-value"
#pragma GCC diagnostic ignored "-Wanalyzer-use-after-free"

nullptr_t
ccfg_destroy(ccfg *cfg)
{
	if (cfg)
	{
		cbook_destroy(cfg->params);
		cbook_destroy(cfg->sequences);
		cbook_destroy(cfg->sources);
		cdict_destroy(cfg->keys_params);
		cdict_destroy(cfg->keys_sequences);
		cdict_destroy(cfg->tokens);
		free(cfg);
	}

	return nullptr;
}

#pragma GCC diagnostic pop

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cerr
ccfg_error(const ccfg *cfg)
{
	return cfg ? cfg->err : CERR_INVALID;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ccfg_fetch(ccfg *cfg, const char *namespace, const char *property)
{
	GUARD(cfg);

	size_t i;

	cfg->it_group = SIZE_MAX;
	cfg->it       = SIZE_MAX;

	if (cdict_find(cfg->keys_sequences, namespace, 0, &i)
	 && cdict_find(cfg->keys_sequences, property,  i, &cfg->it_group))
	{
		cfg->it = 0;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
ccfg_iterate(ccfg *cfg)
{
	GUARD(cfg, false);

	if (cfg->it >= cbook_group_length(cfg->sequences, cfg->it_group))
	{
		return false;
	}

	cfg->it++;

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ccfg_load(ccfg *cfg)
{
	GUARD(cfg);

	ccfg_clear_resources(cfg);
	source_parse_root(cfg, select_source(cfg, nullptr), false);
	cfg->loads++;

	update_err(cfg);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ccfg_load_internal(ccfg *cfg, const char *buffer)
{
	GUARD(cfg);

	ccfg_clear_resources(cfg);
	source_parse_root(cfg, buffer ? buffer : "", true);
	cfg->loads++;

	update_err(cfg);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ccfg_push_param_double(ccfg *cfg, const char *name, double d)
{
	char tmp[64];

	snprintf(tmp, 64, "%f", d);

	ccfg_push_param_str(cfg, name, tmp);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ccfg_push_param_long(ccfg *cfg, const char *name, long long l)
{
	char tmp[64];

	snprintf(tmp, 64, "%lli", l);

	ccfg_push_param_str(cfg, name, tmp);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ccfg_push_param_str(ccfg *cfg, const char *name, const char *str)
{
	GUARD(cfg);

	cbook_write(cfg->params, str);
	cdict_write(cfg->keys_params, name, 0, cbook_words_number(cfg->params) - 1);

	update_err(cfg);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ccfg_push_source(ccfg *cfg, const char *filename)
{
	GUARD(cfg);

	cbook_write(cfg->sources, filename);

	update_err(cfg);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const char *
ccfg_resource(const ccfg *cfg)
{
	GUARD(cfg, "");

	return cbook_word_in_group(cfg->sequences, cfg->it_group, cfg->it - 1);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
ccfg_resource_length(const ccfg *cfg)
{
	GUARD(cfg, 0);

	return cbook_group_length(cfg->sequences, cfg->it_group);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ccfg_restore(ccfg *cfg, const ccfg_cursor cursor)
{
	if (!ccfg_valid_cursor(cfg, cursor))
	{
		return;
	}

	cfg->it_group = (size_t)cursor.data[2];
	cfg->it       = (size_t)cursor.data[3];
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ccfg_restrict(ccfg *cfg)
{
	GUARD(cfg);

	cfg->restricted = true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

ccfg_cursor
ccfg_snap(const ccfg *cfg)
{
	ccfg_cursor cursor;

	cursor.data[0] = 0;
	cursor.data[1] = SIZE_MAX;
	cursor.data[2] = SIZE_MAX;
	cursor.data[3] = SIZE_MAX;

	GUARD(cfg, cursor);

	cursor.data[0] = (uintptr_t)cfg;
	cursor.data[1] = cfg->loads;
	cursor.data[2] = cfg->it_group;
	cursor.data[3] = cfg->it;

	return cursor;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ccfg_unrestrict(ccfg *cfg)
{
	GUARD(cfg);

	cfg->restricted = false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
ccfg_valid_cursor(const ccfg *cfg, const ccfg_cursor cursor)
{
	GUARD(cfg, false);

	return (ccfg *)cursor.data[0] == cfg
	    && (size_t)cursor.data[1] == cfg->loads
	    && (size_t)cursor.data[2] < cbook_groups_number(cfg->sequences)
	    && (size_t)cursor.data[3] < cbook_group_length(cfg->sequences, (size_t)cursor.data[2]);
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static const char *
select_source(const ccfg *cfg, size_t *index)
{
	FILE *f;
	const char *str;

	for (size_t i = 0; i < cbook_words_number(cfg->sources); i++)
	{
		str = cbook_word(cfg->sources, i);
		if ((f = fopen(str, "r")))
		{
			if (index)
			{
				*index = i;
			}
			fclose(f);
			return str;
		}
	}

	return "";
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
update_err(ccfg *cfg)
{
	cerr_set(&cfg->err, cbook_error(cfg->params));
	cerr_set(&cfg->err, cbook_error(cfg->sequences));
	cerr_set(&cfg->err, cbook_error(cfg->sources));
	cerr_set(&cfg->err, cdict_error(cfg->keys_params));
	cerr_set(&cfg->err, cdict_error(cfg->keys_sequences));
	cerr_set(&cfg->err, cdict_error(cfg->tokens));
}

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
#include <cassette/cobj.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "main.h"
#include "source.h"
#include "token.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define SET_ERR(ERR) if (!cfg->err) { cfg->err = ERR; }

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static const char * select_source (const ccfg *, size_t *) CCFG_NONNULL_RETURN CCFG_NONNULL(1);
static enum cerr    update_err    (ccfg *)                                     CCFG_NONNULL(1);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

ccfg ccfg_placeholder_instance =
{
	.params         = CBOOK_PLACEHOLDER,
	.sequences      = CBOOK_PLACEHOLDER,
	.sources        = CBOOK_PLACEHOLDER,
	.keys_params    = CDICT_PLACEHOLDER,
	.keys_sequences = CDICT_PLACEHOLDER,
	.tokens         = CDICT_PLACEHOLDER,
	.it_group       = SIZE_MAX,
	.it             = SIZE_MAX,
	.restricted     = false,
	.err            = CERR_INVALID,
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

bool
ccfg_can_open_sources(const ccfg *cfg, size_t *index)
{
	if (cfg->err)
	{
		return false;
	}

	return select_source(cfg, index)[0] != '\0';
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ccfg_clear_resources(ccfg *cfg)
{
	if (cfg->err)
	{
		return;
	}

	cbook_clear(cfg->sequences);
	cdict_clear(cfg->keys_sequences);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ccfg_clear_params(ccfg *cfg)
{
	if (cfg->err)
	{
		return;
	}

	cbook_clear(cfg->params);
	cdict_clear(cfg->keys_params);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ccfg_clear_sources(ccfg *cfg)
{
	if (cfg->err)
	{
		return;
	}

	cbook_clear(cfg->sources);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

ccfg *
ccfg_clone(ccfg *cfg)
{
	ccfg *cfg_new;

	if (!(cfg_new = malloc(sizeof(ccfg))))
	{
		return CCFG_PLACEHOLDER;
	}

	cfg_new->params         = cbook_clone(cfg->params);
	cfg_new->sequences      = cbook_clone(cfg->sequences);
	cfg_new->sources        = cbook_clone(cfg->sources);
	cfg_new->keys_params    = cdict_clone(cfg->keys_params);
	cfg_new->keys_sequences = cdict_clone(cfg->keys_sequences);
	cfg_new->tokens         = cdict_clone(cfg->tokens);
	cfg_new->it_group       = cfg->it_group;
	cfg_new->it             = cfg->it;
	cfg_new->restricted     = cfg->restricted;
	cfg_new->err            = CERR_NONE;

	if (update_err(cfg_new))
	{
		ccfg_destroy(cfg_new);
		return CCFG_PLACEHOLDER;
	}

	return cfg_new;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

ccfg *
ccfg_create(void)
{
	ccfg *cfg;

	if (!(cfg = malloc(sizeof(ccfg))))
	{
		return CCFG_PLACEHOLDER;
	}

	cfg->params         = cbook_create();
	cfg->sequences      = cbook_create();
	cfg->sources        = cbook_create();
	cfg->keys_params    = cdict_create();
	cfg->keys_sequences = cdict_create();
	cfg->tokens         = token_dict_create();
	cfg->it_group       = SIZE_MAX;
	cfg->it             = SIZE_MAX;
	cfg->restricted     = false;
	cfg->err            = CERR_NONE;

	if (update_err(cfg))
	{
		ccfg_destroy(cfg);
		return CCFG_PLACEHOLDER;
	}

	return cfg;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ccfg_destroy(ccfg *cfg)
{
	if (cfg == CCFG_PLACEHOLDER)
	{
		return;
	}

	cbook_destroy(cfg->params);
	cbook_destroy(cfg->sequences);
	cbook_destroy(cfg->sources);
	cdict_destroy(cfg->keys_params);
	cdict_destroy(cfg->keys_sequences);
	cdict_destroy(cfg->tokens);

	free(cfg);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cerr
ccfg_error(const ccfg *cfg)
{
	return cfg->err;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ccfg_fetch(ccfg *cfg, const char *namespace, const char *property)
{
	size_t i;

	if (cfg->err)
	{
		return;
	}

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
	if (cfg->err || cfg->it >= cbook_group_length(cfg->sequences, cfg->it_group))
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
	const char *source;

	if (cfg->err || (source = select_source(cfg, NULL))[0] == '\0')
	{
		return;
	}

	cbook_clear(cfg->sequences);
	cdict_clear(cfg->keys_sequences);
	source_parse_root(cfg, source, false);

	update_err(cfg);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ccfg_load_internal(ccfg *cfg, const char *buffer)
{
	if (cfg->err)
	{
		return;
	}

	cbook_clear(cfg->sequences);
	cdict_clear(cfg->keys_sequences);
	source_parse_root(cfg, buffer, true);

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
	size_t i;

	if (cfg->err)
	{
		return;
	}

	/* try to rewrite param value if it already exists */

	if (cdict_find(cfg->keys_params, name, 0, &i)
	 && cbook_rewrite(cfg->params, i, str))
	{
		return;
	}

	/* otherwhise create new param */

	cbook_write(cfg->params, str);
	if (!cbook_error(cfg->params))
	{
		cdict_write(cfg->keys_params, name, 0, cbook_words_number(cfg->params) - 1);
	}

	update_err(cfg);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ccfg_push_source(ccfg *cfg, const char *filename)
{
	if (cfg->err)
	{
		return;
	}

	cbook_write(cfg->sources, filename);

	update_err(cfg);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ccfg_repair(ccfg *cfg)
{
	if (cfg->err == CERR_INVALID)
	{
		return;
	}

	cbook_repair(cfg->params);
	cbook_repair(cfg->sequences);
	cbook_repair(cfg->sources);
	cdict_repair(cfg->keys_params);
	cdict_repair(cfg->keys_sequences);
	cdict_repair(cfg->tokens);
	
	cfg->err = CERR_NONE;

	update_err(cfg);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const char *
ccfg_resource(const ccfg *cfg)
{
	if (cfg->err)
	{
		return "";
	}

	return cbook_word_in_group(cfg->sequences, cfg->it_group, cfg->it - 1);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
ccfg_resource_length(const ccfg *cfg)
{
	if (cfg->err)
	{
		return 0;
	}

	return cbook_group_length(cfg->sequences, cfg->it_group);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ccfg_restrict(ccfg *cfg)
{
	if (cfg->err)
	{
		return;
	}

	cfg->restricted = true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ccfg_unrestrict(ccfg *cfg)
{
	if (cfg->err)
	{
		return;
	}

	cfg->restricted = false;
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

static enum cerr
update_err(ccfg *cfg)
{
	SET_ERR(cbook_error(cfg->params))
	SET_ERR(cbook_error(cfg->sequences))
	SET_ERR(cbook_error(cfg->sources))
	SET_ERR(cdict_error(cfg->keys_params))
	SET_ERR(cdict_error(cfg->keys_sequences))
	SET_ERR(cdict_error(cfg->tokens))

	return cfg->err;
}

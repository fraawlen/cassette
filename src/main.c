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

#include <assert.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include <cassette/ccfg.h>
#include <cassette/cobj.h>

#include "file.h"
#include "main.h"
#include "token.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

struct _callback_t
{
	void (*fn)(ccfg_t *cfg, bool load_success, void *ref);
	void *ref;
};

typedef struct _callback_t _callback_t;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void        _clear_callbacks (ccfg_t *cfg);
static const char *_source_select   (const ccfg_t *cfg);
static bool        _update_status   (ccfg_t *cfg);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static ccfg_t _err_cfg =
{
	.params        = NULL,
	.sequences     = NULL,
	.sources       = NULL,
	.callbacks     = NULL,
	.ref_params    = NULL,
	.ref_sequences = NULL,
	.tokens        = NULL,
	.seed          = 0,
	.failed        = true,
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
ccfg_clear_callbacks(ccfg_t *cfg)
{
	assert(cfg);

	if (cfg->failed)
	{
		return;
	}

	_clear_callbacks(cfg);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ccfg_clear_parameters(ccfg_t *cfg)
{
	assert(cfg);

	if (cfg->failed)
	{
		return;
	}
	
	cobj_dictionary_clear(cfg->ref_params);
	cobj_book_clear(cfg->params);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ccfg_clear_sources(ccfg_t *cfg)
{
	assert(cfg);

	if (cfg->failed)
	{
		return;
	}

	cobj_book_clear(cfg->sources);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

ccfg_t *
ccfg_create(void)
{
	ccfg_t *cfg;

	if (!(cfg = malloc(sizeof(ccfg_t))))
	{
		return &_err_cfg;
	}

	cfg->sequences     = cobj_book_create(0, CCFG_MAX_WORD_BYTES);
	cfg->params        = cobj_book_create(4, CCFG_MAX_WORD_BYTES);
	cfg->sources       = cobj_book_create(4, PATH_MAX);
	cfg->callbacks     = cobj_tracker_create(2);
	cfg->ref_params    = cobj_dictionary_create(4, 0.6);
	cfg->ref_sequences = cobj_dictionary_create(0, 0.6);
	cfg->tokens        = token_dictionary_create();
	cfg->seed          = 0;
	cfg->failed        = false;

	_update_status(cfg);

	return cfg;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ccfg_destroy(ccfg_t **cfg)
{
	assert(cfg && *cfg);

	if (*cfg == &_err_cfg)
	{
		return;
	}

	_clear_callbacks(*cfg);

	cobj_book_destroy(&(*cfg)->params);
	cobj_book_destroy(&(*cfg)->sequences);
	cobj_book_destroy(&(*cfg)->sources);
	cobj_tracker_destroy(&(*cfg)->callbacks);
	cobj_dictionary_destroy(&(*cfg)->ref_params);
	cobj_dictionary_destroy(&(*cfg)->ref_sequences);
	cobj_dictionary_destroy(&(*cfg)->tokens);

	free(*cfg);

	*cfg = &_err_cfg;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
ccfg_fetch(ccfg_t *cfg, const char *namespace, const char *property)
{
	size_t i_namespace;
	size_t i_prop;

	assert(cfg);

	if (cfg->failed)
	{
		return false;
	}

	cobj_book_lock_iterator(cfg->sequences);

	if (!namespace || namespace[0] == '\0')
	{
		return false;
	}

	if (!property || property[0] == '\0')
	{
		return false;
	}

	if (!cobj_dictionary_find(cfg->ref_sequences, namespace, 0, &i_namespace))
	{
		return false;
	}

	if (!cobj_dictionary_find(cfg->ref_sequences, property, i_namespace, &i_prop))
	{
		return false;
	}

	cobj_book_reset_iterator(cfg->sequences, i_prop);

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

ccfg_t *
ccfg_get_placeholder(void)
{
	return &_err_cfg;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
ccfg_get_resource_size(const ccfg_t *cfg)
{
	assert(cfg);

	if (cfg->failed)
	{
		return 0;
	}

	return cobj_book_get_group_size(cfg->sequences, cobj_book_get_iterator_group(cfg->sequences));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const char *
ccfg_get_value(const ccfg_t *cfg)
{
	assert(cfg);
	
	if (cfg->failed)
	{
		return "";
	}

	return cobj_book_get_iteration(cfg->sequences);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
ccfg_has_failed(const ccfg_t *cfg)
{
	assert(cfg);

	return cfg->failed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
ccfg_load(ccfg_t *cfg)
{
	const _callback_t *call;

	bool success = true;

	assert(cfg);

	if (cfg->failed)
	{
		return false;
	}

	cobj_book_clear(cfg->sequences);
	cobj_dictionary_clear(cfg->ref_sequences);

	success &= file_parse_root(cfg, _source_select(cfg));
	success &= !_update_status(cfg);

	cobj_tracker_reset_iterator(cfg->callbacks);
	while (cobj_tracker_increment_iterator(cfg->callbacks))
	{
		call = cobj_tracker_get_iteration(cfg->callbacks);
		call->fn(cfg, success, call->ref);
	}

	return success;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
ccfg_pick_next_value(ccfg_t *cfg)
{
	assert(cfg);
	
	if (cfg->failed)
	{
		return false;
	}

	return cobj_book_increment_iterator(cfg->sequences);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ccfg_push_callback(ccfg_t *cfg, void (*fn)(ccfg_t *cfg, bool load_success, void *ref), void *ref)
{
	_callback_t *tmp;

	assert(cfg);

	if (cfg->failed)
	{
		return;
	}

	if (!fn)
	{
		return;
	}

	if (!(tmp = malloc(sizeof(_callback_t))))
	{
		cfg->failed = true;
		return;
	}

	tmp->fn  = fn;
	tmp->ref = ref;
	
	cobj_tracker_push(cfg->callbacks, tmp, NULL);

	_update_status(cfg);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ccfg_push_parameter(ccfg_t *cfg, const char *name, const char *value)
{
	size_t i;

	assert(cfg);

	if (cfg->failed)
	{
		return;
	}

	if (!name || !value)
	{
		return;
	}

	if (cobj_dictionary_find(cfg->ref_params, name, 0, &i))
	{
		cobj_book_rewrite_word(cfg->params, value, 0, i);
	}
	else
	{
		cobj_dictionary_write(cfg->ref_params, name, 0, cobj_book_get_group_size(cfg->params, 0));
		cobj_book_write_new_word(cfg->params, value, COBJ_BOOK_OLD_GROUP);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ccfg_push_source(ccfg_t *cfg, const char *filename)
{
	assert(cfg);

	if (cfg->failed)
	{
		return;
	}

	cobj_book_write_new_word(cfg->sources, filename, COBJ_BOOK_OLD_GROUP);

	_update_status(cfg);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ccfg_seed(ccfg_t *cfg, unsigned long long seed)
{
	assert(cfg);

	if (cfg->failed)
	{
		return;
	}

	cfg->seed = seed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const char *
ccfg_test_sources(const ccfg_t *cfg)
{
	assert(cfg);

	if (cfg->failed)
	{
		return "";
	}

	return _source_select(cfg);
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static void
_clear_callbacks(ccfg_t *cfg)
{
	cobj_tracker_reset_iterator(cfg->callbacks);
	while (cobj_tracker_increment_iterator(cfg->callbacks))
	{
		free((void*)cobj_tracker_get_iteration(cfg->callbacks));
	}

	cobj_tracker_clear(cfg->callbacks);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const char *
_source_select(const ccfg_t *cfg)
{
	FILE *f;
	const char *s;

	cobj_book_reset_iterator(cfg->sources, 0);
	while (cobj_book_increment_iterator(cfg->sources))
	{
		s = cobj_book_get_iteration(cfg->sources);
		if ((f = fopen(cobj_book_get_iteration(cfg->sources), "r")))
		{
			fclose(f);
			return s;
		}
	}

	return "";
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
_update_status(ccfg_t *cfg)
{
	cfg->failed |= cobj_book_has_failed(cfg->params);
	cfg->failed |= cobj_book_has_failed(cfg->sequences);
	cfg->failed |= cobj_book_has_failed(cfg->sources);
	cfg->failed |= cobj_tracker_has_failed(cfg->callbacks);
	cfg->failed |= cobj_dictionary_has_failed(cfg->ref_params);
	cfg->failed |= cobj_dictionary_has_failed(cfg->ref_sequences);
	cfg->failed |= cobj_dictionary_has_failed(cfg->tokens);

	return cfg->failed;
}

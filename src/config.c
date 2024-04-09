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
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include <derelict/do.h>
#include <derelict/dr.h>

#include "config.h"
#include "file.h"
#include "token.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

struct _callback_t
{
	void (*fn)(dr_config_t *cfg, bool load_success, void *ref);
	void *ref;
};

typedef struct _callback_t _callback_t;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void        _clear_callbacks (dr_config_t *cfg);
static const char *_source_select   (const dr_config_t *cfg);
static bool        _update_status   (dr_config_t *cfg);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static dr_config_t _err_cfg =
{
	.sequences     = NULL,
	.sources       = NULL,
	.callbacks     = NULL,
	.ref_sequences = NULL,
	.tokens        = NULL,
	.seed          = 0,
	.failed        = true,
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
dr_config_clear_callbacks(dr_config_t *cfg)
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
dr_config_clear_sources(dr_config_t *cfg)
{
	assert(cfg);

	if (cfg->failed)
	{
		return;
	}

	do_book_clear(cfg->sources);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

dr_config_t *
dr_config_create(size_t n)
{
	dr_config_t *cfg;

	if (!(cfg = malloc(sizeof(dr_config_t))))
	{
		return &_err_cfg;
	}

	cfg->sequences     = do_book_create(n, DR_TOKEN_N);
	cfg->sources       = do_book_create(4, PATH_MAX);
	cfg->callbacks     = do_tracker_create(2);
	cfg->ref_sequences = do_dictionary_create(n, 0.6);
	cfg->tokens        = dr_token_dictionary_create();
	cfg->seed          = 0;
	cfg->failed        = false;

	_update_status(cfg);

	return cfg;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dr_config_destroy(dr_config_t **cfg)
{
	assert(cfg && *cfg);

	if (*cfg == &_err_cfg)
	{
		return;
	}

	_clear_callbacks(*cfg);

	do_book_destroy(&(*cfg)->sequences);
	do_book_destroy(&(*cfg)->sources);
	do_tracker_destroy(&(*cfg)->callbacks);
	do_dictionary_destroy(&(*cfg)->ref_sequences);
	do_dictionary_destroy(&(*cfg)->tokens);

	free(*cfg);

	*cfg = &_err_cfg;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

dr_config_t *
dr_config_get_placeholder(void)
{
	return &_err_cfg;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
dr_config_has_failed(const dr_config_t *cfg)
{
	assert(cfg);

	return cfg->failed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
dr_config_load(dr_config_t *cfg)
{
	const _callback_t *call;

	bool success = true;

	assert(cfg);

	if (cfg->failed)
	{
		return false;
	}

	do_book_clear(cfg->sequences);
	do_dictionary_clear(cfg->ref_sequences);

	success &= dr_file_parse_root(cfg, _source_select(cfg));
	success &= !_update_status(cfg);

	do_tracker_reset_iterator(cfg->callbacks);
	while (do_tracker_increment_iterator(cfg->callbacks))
	{
		call = do_tracker_get_iteration(cfg->callbacks);
		call->fn(cfg, success, call->ref);
	}

	return success;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dr_config_push_callback(dr_config_t *cfg, void (*fn)(dr_config_t *cfg, bool load_success, void *ref), void *ref)
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
	
	do_tracker_push(cfg->callbacks, tmp, NULL);

	_update_status(cfg);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dr_config_push_source(dr_config_t *cfg, const char *filename)
{
	assert(cfg);

	if (cfg->failed)
	{
		return;
	}

	do_book_write_new_word(cfg->sources, filename, DO_BOOK_OLD_GROUP);

	_update_status(cfg);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dr_config_seed(dr_config_t *cfg, unsigned long long seed)
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
dr_config_test_sources(const dr_config_t *cfg)
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
_clear_callbacks(dr_config_t *cfg)
{
	do_tracker_reset_iterator(cfg->callbacks);
	while (do_tracker_increment_iterator(cfg->callbacks))
	{
		free((void*)do_tracker_get_iteration(cfg->callbacks));
	}

	do_tracker_clear(cfg->callbacks);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const char *
_source_select(const dr_config_t *cfg)
{
	FILE *f;
	const char *s;

	do_book_reset_iterator(cfg->sources, 0);
	while (do_book_increment_iterator(cfg->sources))
	{
		s = do_book_get_iteration(cfg->sources);
		if ((f = fopen(do_book_get_iteration(cfg->sources), "r")))
		{
			fclose(f);
			return s;
		}
	}

	return "";
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
_update_status(dr_config_t *cfg)
{
	cfg->failed |= do_book_has_failed(cfg->sequences);
	cfg->failed |= do_book_has_failed(cfg->sources);
	cfg->failed |= do_tracker_has_failed(cfg->callbacks);
	cfg->failed |= do_dictionary_has_failed(cfg->ref_sequences);
	cfg->failed |= do_dictionary_has_failed(cfg->tokens);

	return cfg->failed;
}

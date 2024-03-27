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
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
	void (*fn)(dr_config_t *cfg);	
};

typedef struct _callback_t _callback_t;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void        _clear_tracker (do_tracker_t *tracker);
static const char *_source_select (const dr_config_t *cfg);
static void        _update_status (dr_config_t *cfg);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static dr_config_t _err_cfg =
{
	.sequences  = NULL,
	.sources    = NULL,
	.callbacks  = NULL,
	.references = NULL,
	.failed     = true,
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
dr_config_clear_callbacks_load(dr_config_t *cfg)
{
	assert(cfg);

	if (cfg->failed)
	{
		return;
	}

	_clear_tracker(cfg->callbacks);
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

	_clear_tracker(cfg->sources);
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

	cfg->sequences  = do_book_create(n, DR_TOKEN_N);
	cfg->sources    = do_tracker_create(4);
	cfg->callbacks  = do_tracker_create(2);
	cfg->references = do_dictionary_create(n, 0.6);
	cfg->tokens     = dr_token_dictionary_create();
	cfg->failed     = false;

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

	_clear_tracker((*cfg)->sources);
	_clear_tracker((*cfg)->callbacks);

	do_book_destroy(&(*cfg)->sequences);
	do_tracker_destroy(&(*cfg)->sources);
	do_tracker_destroy(&(*cfg)->callbacks);
	do_dictionary_destroy(&(*cfg)->references);
	do_dictionary_destroy(&(*cfg)->tokens);

	free(*cfg);

	*cfg = &_err_cfg;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
dr_config_find(const dr_config_t *cfg, const char *namespace, const char *property, char *values,
               size_t n_values, size_t values_n)
{
	size_t i_namespace;
	size_t i_prop;
	size_t i = 0;

	assert(cfg && property && values && values_n > 0);

	if (cfg->failed)
	{
		return 0;
	}

	if (n_values > SIZE_MAX / values_n)
	{
		return 0;
	}

	/* find target sequence location */

	if (!namespace)
	{
		namespace = "_";
	}

	if (!do_dictionary_find(cfg->references, namespace, 0, &i_namespace))
	{
		return 0;
	}

	if (!do_dictionary_find(cfg->references, property, i_namespace, &i_prop))
	{
		return 0;
	}

	/* copy found sequence into the value buffer */

	do_book_reset_iterator(cfg->sequences, i_prop);
	while (do_book_increment_iterator(cfg->sequences) && i < n_values)
	{
		strncpy(values + i * values_n, do_book_get_iteration(cfg->sequences), values_n);
		values[++i * values_n - 1] = '\0';
	}

	return i;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
dr_config_has_failed(const dr_config_t *cfg)
{
	assert(cfg);

	return cfg->failed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dr_config_load(dr_config_t *cfg)
{
	assert(cfg);

	if (cfg->failed)
	{
		return;
	}

	/* clear old data */

	do_book_clear(cfg->sequences);
	do_dictionary_clear(cfg->references);

	/* parse source file */

	dr_file_parse_root(cfg, _source_select(cfg));

	_update_status(cfg);

	/* run callbacks */

	do_tracker_reset_iterator(cfg->callbacks);
	while (do_tracker_increment_iterator(cfg->callbacks))
	{
		((_callback_t*)do_tracker_get_iteration(cfg->callbacks))->fn(cfg);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dr_config_push_callback_load(dr_config_t *cfg, void (*fn)(dr_config_t *dr))
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

	tmp->fn = fn;
	
	do_tracker_push(cfg->callbacks, tmp, NULL);

	_update_status(cfg);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dr_config_push_source(dr_config_t *cfg, const char *filename)
{
	char *tmp;

	assert(cfg);

	if (cfg->failed)
	{
		return;
	}

	if (!filename)
	{
		return;
	}

	if (!(tmp = strdup(filename)))
	{
		cfg->failed = true;
		return;
	}

	do_tracker_push(cfg->sources, tmp, NULL);

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
		return NULL;
	}

	return _source_select(cfg);
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static void
_clear_tracker(do_tracker_t *tracker)
{
	do_tracker_reset_iterator(tracker);
	while (do_tracker_increment_iterator(tracker))
	{
		free((void*)do_tracker_get_iteration(tracker));
	}

	do_tracker_clear(tracker);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const char *
_source_select(const dr_config_t *cfg)
{
	FILE *f;
	char *s;

	do_tracker_reset_iterator(cfg->sources);
	while (do_tracker_increment_iterator(cfg->sources))
	{
		s = (char*)do_tracker_get_iteration(cfg->sources);
		if ((f = fopen(s, "r")))
		{
			fclose(f);
			return s;
		}
	}

	return NULL;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_update_status(dr_config_t *cfg)
{
	cfg->failed |= do_book_has_failed(cfg->sequences);
	cfg->failed |= do_tracker_has_failed(cfg->sources);
	cfg->failed |= do_tracker_has_failed(cfg->callbacks);
	cfg->failed |= do_dictionary_has_failed(cfg->references);
	cfg->failed |= do_dictionary_has_failed(cfg->tokens);
}

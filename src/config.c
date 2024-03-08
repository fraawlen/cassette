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
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <derelict/du.h>

#include "dr.h"
#include "parse.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

typedef struct {
	void (*fn)(dr_config_t *cfg);	
} _callback_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct _config_t {
	du_book_t data;
	du_tracker_t sources;
	du_tracker_t callbacks;
	du_dictionary_t sequences;
	du_dictionary_t tokens;
	du_status_t status;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static char *_source_select (const dr_config_t *cfg);
static void  _update_status (dr_config_t *cfg);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

dr_config_t *
dr_config_create(size_t n)
{
	dr_config_t *cfg = malloc(sizeof(dr_config_t));
	if (!cfg) {
		goto fail_alloc;
	}

	cfg->status = DU_STATUS_SUCCESS;

	du_book_init(&cfg->data, n * 1.5, DR_TOKEN_N);
	du_tracker_init(&cfg->sources, 4);
	du_tracker_init(&cfg->callbacks, 2);
	du_dictionary_init(&cfg->sequences, n * 2, 0.6);
	dr_token_init_dictionary(&cfg->tokens);

	_update_status(cfg);
	du_status_test(cfg->status, goto fail_init);

	return cfg;

fail_init:
	dr_config_destroy(cfg);
fail_alloc:
	return NULL;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void 
dr_config_destroy(dr_config_t *cfg)
{
	assert(cfg);

	dr_config_clear_sources(cfg);
	dr_config_clear_callbacks_load(cfg);

	du_book_reset(&cfg->data);
	du_tracker_reset(&cfg->sources);
	du_tracker_reset(&cfg->callbacks);
	du_dictionary_reset(&cfg->sequences);
	du_dictionary_reset(&cfg->tokens);

	free(cfg);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

du_status_t
dr_config_get_status(const dr_config_t *cfg)
{
	assert(cfg);

	return cfg->status;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
 
size_t
dr_config_find_resource(const dr_config_t *cfg, const char *namespace, const char *property, char *values_buf,
                        size_t n_values, size_t value_n)
{
	assert(cfg && property);
	du_status_test(cfg->status, return 0);

	int64_t i_namespace;
	int64_t i_property;

	/* find target sequence location */

	if (!namespace) {
		namespace = "_";
	}

	if (!du_dictionary_find_value(&cfg->sequences, namespace, DR_CONFIG_NAMESPACES,               &i_namespace) ||
	    !du_dictionary_find_value(&cfg->sequences, property,  DR_CONFIG_NAMESPACES + i_namespace, &i_property)) {
		return 0;
	}

	/* write sequence tokens into target value array */

	const size_t n = du_book_get_group_length(&cfg->data, i_property);
	const size_t val_n = value_n > DR_TOKEN_N ? DR_TOKEN_N : value_n;
	const size_t n_vals = n_values > n ? n : n_values;

	for (size_t i = 0; i < n_vals; i++) {
		strncpy(values_buf + i * value_n, du_book_get_word_in_group(&cfg->data, i_property, i), val_n);
	}

	return n_vals;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
dr_config_load(dr_config_t *cfg)
{
	assert(cfg);
	du_status_test(cfg->status, return false);

	/* get rid of old data */

	du_book_clear(&cfg->data);
	du_dictionary_clear(&cfg->sequences);

	/* load new config */

	if (!dr_parse_file(NULL, _source_select(cfg), &cfg->data, &cfg->sequences, &cfg->tokens)) {
		return false;
	}

	_update_status(cfg);
	du_status_test(cfg->status, return false);

	/* run added callbacks */

	for (size_t i = 0; i < cfg->callbacks.n; i++) {
		((_callback_t*)cfg->callbacks.ptr[i])->fn(cfg);
	}

	/* end & errors */

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dr_config_clear_callbacks_load(dr_config_t *cfg)
{
	assert(cfg);
	du_status_test(cfg->status, return);

	for (size_t i = 0; i < cfg->callbacks.n; i++) {
		free((void*)cfg->callbacks.ptr[i]);
	}

	du_tracker_clear(&cfg->callbacks);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dr_config_clear_sources(dr_config_t *cfg)
{
	assert(cfg);
	du_status_test(cfg->status, return);

	for (size_t i = 0; i < cfg->sources.n; i++) {
		free((void*)cfg->sources.ptr[i]);
	}

	du_tracker_clear(&cfg->sources);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dr_config_push_callback_load(dr_config_t *cfg, void (*fn)(dr_config_t *dr))
{
	assert(cfg && fn);
	du_status_test(cfg->status, return);

	_callback_t *tmp = malloc(sizeof(_callback_t));
	du_status_assert(cfg->status, tmp, return);

	tmp->fn = fn;

	du_tracker_push(&cfg->callbacks, tmp, NULL);
	du_status_assert(cfg->status, (cfg->callbacks.status == DU_STATUS_SUCCESS), return);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dr_config_push_source(dr_config_t *cfg, const char *filename)
{
	assert(cfg && filename);
	du_status_test(cfg->status, return);

	const char *tmp = strdup(filename);
	du_status_assert(cfg->status, tmp, return);

	du_tracker_push(&cfg->sources, tmp, NULL);
	du_status_assert(cfg->status, (cfg->sources.status == DU_STATUS_SUCCESS), return);
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static char *
_source_select(const dr_config_t *cfg)
{
	FILE *f;
	char *s;

	for (size_t i = cfg->sources.n; i > 0; i--) {
		s = (char*)cfg->sources.ptr[i - 1];
		if ((f = fopen(s, "r"))) {
			fclose(f);
			return s;
		}
	}

	return NULL;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_update_status (dr_config_t *cfg)
{
	du_status_test(cfg->data.status,      cfg->status = DU_STATUS_FAILURE);
	du_status_test(cfg->sources.status,   cfg->status = DU_STATUS_FAILURE);
	du_status_test(cfg->callbacks.status, cfg->status = DU_STATUS_FAILURE);
	du_status_test(cfg->sequences.status, cfg->status = DU_STATUS_FAILURE);
	du_status_test(cfg->tokens.status,    cfg->status = DU_STATUS_FAILURE);
}

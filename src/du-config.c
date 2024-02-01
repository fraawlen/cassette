/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Derelict Utilities (DU) library.
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
#include <stdlib.h>
#include <string.h>

#include "du.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

struct _config_t {
	du_status_t status;
	du_string_t path;
	du_string_t source;
	du_tracker_t resources;
	du_tracker_t callbacks;
	du_dictionary_t dict_file;
	du_dictionary_t dict_resources;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

du_config_t *
du_config_create(const char *source, const char *path, uint32_t n_resources)
{
	assert(source);

	du_config_t *cfg = malloc(sizeof(du_config_t));
	if (!cfg) {
		goto fail_alloc;
	}

	cfg->status = DU_STATUS_SUCCESS;
	du_string_init(&cfg->source, source);
	du_string_init(&cfg->path, path);
	du_tracker_init(&cfg->resources, n_resources);
	du_tracker_init(&cfg->callbacks, 1);
	du_dictionary_init(&cfg->dict_file, n_resources * 2, 0.6);
	du_dictionary_init(&cfg->dict_resources, n_resources, 0.6);

	du_status_test(cfg->source.status,         goto fail_init);
	du_status_test(cfg->path.status,           goto fail_init);
	du_status_test(cfg->resources.status,      goto fail_init);
	du_status_test(cfg->callbacks.status,      goto fail_init);
	du_status_test(cfg->dict_file.status,      goto fail_init);
	du_status_test(cfg->dict_resources.status, goto fail_init);

	return cfg;

fail_init:
	du_config_destroy(cfg);
fail_alloc:
	return NULL;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_config_destroy(du_config_t *cfg)
{
	assert(cfg);

	du_string_reset(&cfg->source);
	du_string_reset(&cfg->path);
	du_tracker_reset(&cfg->resources);
	du_tracker_reset(&cfg->callbacks);
	du_dictionary_reset(&cfg->dict_file);
	du_dictionary_reset(&cfg->dict_resources);

	free(cfg);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

du_status_t
du_config_get_status(const du_config_t *cfg)
{
	assert(cfg);

	return cfg->status;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_config_load(du_config_t *cfg)
{
	assert(cfg);
	du_status_test(cfg->status, return);
	
	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_config_pull_callbacks(du_config_t *cfg, const du_config_callbacks_t *const callbacks)
{
	assert(cfg);
	du_status_test(cfg->status, return);

	du_tracker_pull(&cfg->callbacks, callbacks);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_config_pull_resource(du_config_t *cfg, const char *name)
{
	assert(cfg);
	du_status_test(cfg->status, return);

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_config_push_callbacks(du_config_t *cfg, const du_config_callbacks_t *const callbacks)
{
	assert(cfg);
	du_status_test(cfg->status, return);

	du_tracker_push(&cfg->callbacks, callbacks, NULL);
	cfg->status = cfg->callbacks.status;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_config_push_resource(du_config_t *cfg, const char *name, void *target, du_config_kind_t kind)
{
	assert(cfg);
	du_status_test(cfg->status, return);

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_config_push_resource_custom(du_config_t *cfg, const char *name, void *target,
	void (*fn_parser)(void *target, char *value))
{
	assert(cfg);
	du_status_test(cfg->status, return);

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_config_refresh_resources(du_config_t *cfg)
{
	assert(cfg);
	du_status_test(cfg->status, return);

	// TODO
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/


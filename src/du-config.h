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

#ifndef DU_CONFIG_H
#define DU_CONFIG_H

#include <stdint.h>

#include "du-status.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define DU_CONFIG_STRING_N 128

/**
 *
 */
typedef struct _config_t du_config_t;
/**
 *
 */
typedef enum {
	DU_CONFIG_STRING,
	DU_CONFIG_INT,
	DU_CONFIG_UINT,
	DU_CONFIG_DOUBLE,
	DU_CONFIG_UDOUBLE,
	DU_CONFIG_POSITION,
	DU_CONFIG_LENGTH,
	DU_CONFIG_BOOL,
} du_config_kind_t;

typedef struct {
	void (*fn_preprocessor)(void);
	void (*fn_postprocessor)(void);
} du_config_callbacks_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
du_config_t *du_config_create(const char *source, const char *path, uint32_t n_resources);

/**
 *
 */
void du_config_destroy(du_config_t *cfg);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
void du_config_load(du_config_t *cfg);

/**
 *
 */
void du_config_refresh_resources(du_config_t *cfg);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
void du_config_pull_callbacks(du_config_t *cfg, const du_config_callbacks_t *const callbacks);

/**
 *
 */
void du_config_pull_resource(du_config_t *cfg, const char *name);

/**
 *
 */
void du_config_push_callbacks(du_config_t *cfg, const du_config_callbacks_t *const callbacks);

/**
 *
 */
void du_config_push_resource(du_config_t *cfg, const char *name, void *target, du_config_kind_t kind);

/**
 *
 */
void du_config_push_resource_custom(du_config_t *cfg, const char *name, void *target, 
                                    void (*fn_parser)(void *target, char *value));

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
du_status_t du_config_get_status(const du_config_t *cfg);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DU_CONFIG_H */

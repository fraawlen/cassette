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

#ifndef DR_CONFIG_H
#define DR_CONFIG_H

#include <stdbool.h>

#include <derelict/du.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define DR_CONFIG_SECTIONS   0
#define DR_CONFIG_VARIABLES  1
#define DR_CONFIG_NAMESPACES 2

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
typedef struct _config_t dr_config_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
dr_config_t *dr_config_create(size_t n);

/**
 *
 */
void dr_config_destroy(dr_config_t *cfg);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
bool dr_config_load(dr_config_t *cfg);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
void dr_config_clear_callbacks_load(dr_config_t *cfg);

/**
 *
 */
void dr_config_clear_sources(dr_config_t *cfg);

/**
 *
 */
void dr_config_push_callback_load(dr_config_t *cfg, void (*fn)(dr_config_t *dr));

/**
 *
 */
void dr_config_push_source(dr_config_t *cfg, const char *filename);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
du_status_t dr_config_get_status(const dr_config_t *cfg);

/**
 *
 */
size_t dr_config_get_values(const dr_config_t *cfg, const char *namespace, const char *property, char **values,
                          size_t n_values, size_t value_n);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DR_CONFIG_H */

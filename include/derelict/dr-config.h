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

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

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
void dr_config_destroy(dr_config_t **cfg);

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
bool dr_config_load(dr_config_t *cfg);

/**
 *
 */
void dr_config_push_callback_load(dr_config_t *cfg, void (*fn)(dr_config_t *cfg, bool load_success));

/**
 *
 */
void dr_config_push_source(dr_config_t *cfg, const char *filename);

/**
 *
 */
void dr_config_seed(dr_config_t *cfg, unsigned long long seed);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
bool dr_config_has_failed(const dr_config_t *cfg);

/**
 *
 */
const char *dr_config_test_sources(const dr_config_t *cfg);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DR_CONFIG_H */


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

#ifndef DR_CONVERT_H
#define DR_CONVERT_H

#include <derelict/du.h>

#include "dr-config.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
size_t dr_convert_to_bool(const dr_config_t *cfg, const char *namespace, const char *property, bool *values, size_t n);

/**
 *
 */
size_t dr_convert_to_long(const dr_config_t *cfg, const char *namespace, const char *property, long *values, size_t n);

/**
 *
 */
size_t dr_convert_to_ulong(const dr_config_t *cfg, const char *namespace, const char *property, unsigned long *values, size_t n);

/**
 *
 */
size_t dr_convert_to_double(const dr_config_t *cfg, const char *namespace, const char *property, double *values, size_t n);

/**
 *
 */
size_t dr_convert_to_udouble(const dr_config_t *cfg, const char *namespace, const char *property, double *values, size_t n);

/**
 *
 */
size_t dr_convert_to_position(const dr_config_t *cfg, const char *namespace, const char *property, du_position_t *values, size_t n);

/**
 *
 */
size_t dr_convert_to_length(const dr_config_t *cfg, const char *namespace, const char *property, du_length_t *values, size_t n);

/**
 *
 */
size_t dr_convert_to_ratio(const dr_config_t *cfg, const char *namespace, const char *property, du_ratio_t *values, size_t n);

/**
 *
 */
size_t dr_convert_to_color(const dr_config_t *cfg, const char *namespace, const char *property, du_color_t *values, size_t n);

/**
 *
 */
size_t dr_convert_to_dictionary_value(const dr_config_t *cfg, const char *namespace, const char *property, int64_t *values, size_t n);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DR_CONVERT_H */

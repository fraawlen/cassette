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

#include <stdbool.h>
#include <stdlib.h>

#include <derelict/do.h>

#include "dr-config.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

bool dr_convert_to_bool(const dr_config_t *cfg);

do_color_t dr_convert_to_color(const dr_config_t *cfg);

double dr_convert_to_double(const dr_config_t *cfg);

long dr_convert_to_long(const dr_config_t *cfg);

double dr_convert_to_range(const dr_config_t *cfg, double min, double max);

size_t dr_convert_to_reference(const dr_config_t *cfg, const do_dictionary_t *dict, size_t group, 
                               size_t fallback);

unsigned long dr_convert_to_ulong(const dr_config_t *cfg);

double dr_convert_to_udouble(const dr_config_t *cfg);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DR_CONVERT_H */

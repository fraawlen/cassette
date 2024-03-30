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

#ifndef DR_RESOURCE_H
#define DR_RESOURCE_H

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

/**
 * Looks-up a resource by its namespace and property name. If found, its reference is kept around and the
 * resource values will become accessible through dr_resource_pick_next_value() and dr_resource_conver_*()
 * functions. Empty or null namespaces or properties are not valid and will make the function return without
 * doing anything.
 *
 * @param cfg Config to interact with
 * @param namespace Resource's namespace
 * @param property Resource property name
 *
 * @return True if found, false otherwhise
 */
bool dr_resource_fetch(dr_config_t *cfg, const char *namespace, const char *property);

/**
 * Initially, after a resource has been fetched, its values are not directly accessible by the conversion
 * functions. This function will need to be called first at least once. Each subsequent call to this function
 * will move an internal iterator forward to the next resource value. Until the last value is reached after
 * which this function has no more effect and returns false. Said cursor is reset every time a resource is
 * fetched. Because the conversion functions only access one value at a time, use this function to iterate
 * through them. If this function is called before a resource has been successfully pre-fetched, it will have
 * no effect and return false.
 *
 * @param cfg Config to interact with
 *
 * @return True is the next value could be picked, false otherwhise.
 */
bool dr_resource_pick_next_value(dr_config_t *cfg);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Gets the number of values a pre-fetched resource has.
 * If the no resource is pre-fetched, or if the configuration object is in an error state, 0 is returned.
 *
 * @param cfg Config to interact with
 *
 * @return Number of values that can be iterated through
 */
size_t dr_resource_get_size(const dr_config_t *cfg);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Accesses a single value of a pre-fetched resource and converts it to a boolean.
 *
 * @param cfg Config to interact with
 *
 * @return Converted value
 */
bool dr_resource_convert_to_bool(const dr_config_t *cfg);

/**
 * Accesses a single value of a pre-fetched resource and converts it to a DO color.
 *
 * @param cfg Config to interact with
 *
 * @return Converted value
 */
do_color_t dr_resource_convert_to_color(const dr_config_t *cfg);

/**
 * Accesses a single value of a pre-fetched resource and converts it to a double.
 *
 * @param cfg Config to interact with
 *
 * @return Converted value
 */
double dr_resource_convert_to_double(const dr_config_t *cfg);

/**
 * Accesses a single value of a pre-fetched resource and converts it to a long.
 *
 * @param cfg Config to interact with
 *
 * @return Converted value
 */
long dr_resource_convert_to_long(const dr_config_t *cfg);

/**
 * Accesses a single value of a pre-fetched resource and converts it to a double bound between min and max.
 *
 * @param cfg Config to interact with
 * @param min First double bound
 * @param max Second double bound
 *
 * @return Converted value
 */
double dr_resource_convert_to_range(const dr_config_t *cfg, double min, double max);

/**
 * Accesses a single value of a pre-fetched resource and converts it to a double bound betweem 0.0 and 1.0.
 *
 * @param cfg Config to interact with
 *
 * @return Converted value
 */
double dr_resource_convert_to_ratio(const dr_config_t *cfg);

/**
 * Accesses a single value of a pre-fetched resource and uses it as a key to access a different value stored
 * in a DO dictionary, then returns the new value instead. If there is no match, or if the dictionary is in a
 * failure state, the fallback value is returned instead.
 *
 * @param cfg Config to interact with
 * @param dict DO dictionary to compare the value against
 * @param group DO dictionary group
 * @param fallback Value to return in case of a lack of match
 *
 * @return Converted value
 */
size_t dr_resource_convert_to_reference(const dr_config_t *cfg, const do_dictionary_t *dict, size_t group, size_t fallback);

/**
 * Accesses a single value of a pre-fetched resource and converts it to a string.
 *
 * @param cfg Config to interact with
 *
 * @return Converted value
 */
const char *dr_resource_convert_to_string(const dr_config_t *cfg);

/**
 * Accesses a single value of a pre-fetched resource and converts it to an unsigned double.
 *
 * @param cfg Config to interact with
 *
 * @return Converted value
 */
double dr_resource_convert_to_udouble(const dr_config_t *cfg);

/**
 * Accesses a single value of a pre-fetched resource and converts it to an unsigned long.
 *
 * @param cfg Config to interact with
 *
 * @return Converted value
 */
unsigned long dr_resource_convert_to_ulong(const dr_config_t *cfg);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DR_RESOURCE_H */

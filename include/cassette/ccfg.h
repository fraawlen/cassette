/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Configuration (CCFG) library.
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

#ifndef CCFG_H
#define CCFG_H

#include <stdbool.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define CCFG_MAX_WORD_BYTES 32

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Opaque configuration instance object that holds all parsed resources.
 * This object holds an internal fail state boolean that can be checked with ccfg_has_failed(). If
 * happens to be put in a failure state due to a memory failure, any function that take this object as
 * argument will return early with no side effects and default return values. The only 2 functions that are
 * an exception to this rule are ccfg_destroy() and ccfg_has_failed().
 */
typedef struct ccfg_t ccfg_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Allocates memory and initializes a CCFG configuration instance.
 * This function always returns a valid and safe-to-use or destroy object instance. Even in case of memory
 * allocation failure, the returned value points to an internal static Configuration instanceuration object
 * instance set in a failed state. Therefore, checking for a NULL returned value is useless, instead, use
 * ccfg_has_failed().
 *
 * @return Created config instance object
 */
ccfg_t *ccfg_create(void);

/**
 * Gets a valid pointer to an internal configuration instance set in a failed state. To be used to avoid
 * leaving around uninitialised configuration instance pointers.
 *
 * @return Placeholder config instance object
 */
ccfg_t *ccfg_get_placeholder(void);

/**
 * Destroy a given instance and free allocated memory. The pointed value is then replaced by a placeholder
 * value that points to an internal static configuration instance set in a failed state. Hence, it is safe
 * to call this function multiple times.
 *
 * @param cfg Configuration instance to interact with
 */
void ccfg_destroy(ccfg_t **cfg);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Clears the list of callback functions that were added with ccfg_push_callback().
 *
 * @param cfg Configuration instance to interact with
 */
void ccfg_clear_callbacks(ccfg_t *cfg);

/**
 * Clears the list of added parameters that were added with ccfg_push_parameter_*().
 *
 * @param cfg Configuration instance to interact with
 */
void ccfg_clear_parameters(ccfg_t *cfg);

/**
 * Clears the list of source files that were added with ccfg_push_source().
 *
 * @param cfg Configuration instance to interact with
 */
void ccfg_clear_sources(ccfg_t *cfg);

/**
 * Looks-up a resource by its namespace and property name. If found, its reference is kept around and the
 * resource values will become accessible through ccfg_pick_next_value() and ccfg_get_value().
 * functions. Empty or null namespaces or properties are not valid and will make the function return without
 * doing anything.
 *
 * @param cfg Configuration instance to interact with
 * @param namespace Resource's namespace
 * @param property Resource property name
 *
 * @return True if found, false otherwhise
 */
bool ccfg_fetch(ccfg_t *cfg, const char *namespace, const char *property);

/**
 * Reads the first source file that can be opened, parses it, and stores the resolved resources. Once done,
 * callback functions that were added with ccfg_push_callback() will be called successively in the order
 * they were added. Every time this function is called the saved resources will be cleared first before
 * reading the source. The return value will be set to false if the root source file cannot be opened or if
 * the configuration object has been put in a failure state. Note that if the load fails because no source
 * file could not be opened, the configuration object is not put in a failure state.
 *
 * @param cfg Configuration instance to interact with
 *
 * @return Load success
 */
bool ccfg_load(ccfg_t *cfg);

/**
 * Initially, after a resource has been fetched, its values are not directly accessible by the getter
 * functions. This function will need to be called first at least once. Each subsequent call to this function
 * will move an internal iterator forward to the next resource value. Until the last value is reached after
 * which this function has no more effect and returns false. Said cursor is reset every time a resource is
 * fetched. Because the conversion functions only access one value at a time, use this function to iterate
 * through them. If this function is called before a resource has been successfully pre-fetched, it will have
 * no effect and return false.
 *
 * @param cfg Configuration instance to interact with
 *
 * @return True is the next value could be picked, false otherwhise.
 */
bool ccfg_pick_next_value(ccfg_t *cfg);

/**
 * Adds a function to call every time the given configuration object is done loading a source file. The
 * callback parameter load_success is set to the value of the return of ccfg_load(). The ref parameter
 * here is a pointer of arbitrary value. That same value will be passed down to the callback's ref parameter
 * with no side effects. It is therefore the responsibility of the caller to ensure that the ref's value is
 * valid when the callback is triggered.
 *
 * @param cfg Configuration instance to interact with
 * @param fn Callback function
 * @param ref Abitrary pointer value to pass to callback
 */
void ccfg_push_callback(ccfg_t *cfg, void (*fn)(ccfg_t *cfg, bool load_success, void *ref), void *ref);

/**
 * Adds a configuration parameter in C-string format whose value can be accessed and used from a configuration
 * source file. Unlike user-defined variables, only one value per parameter can be defined.
 *
 * @param cfg Configuration instance to interact with
 * @param name Name of the parameter to use in the source configuration
 * @param value Parameter's value
 */
void ccfg_push_parameter(ccfg_t *cfg, const char *name, const char *value);

/**
 * Adds a source file to potentially parse. Only the first added source that can be opened will be parsed,
 * the remaining sources act as fallback.
 *
 * @param cfg Configuration instance to interact with
 * @param filename Full path to the source file
 */
void ccfg_push_source(ccfg_t *cfg, const char *filename);

/**
 * Seeds an internal random function that affects the output of random tokens. The seed only affects the
 * given object's instance, meaning multiple configuration objects can co-exist with different seeds.
 *
 * @param cfg Configuration instance to interact with
 * @param seed Seed value
 */
void ccfg_seed(ccfg_t *cfg, unsigned long long seed);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Gets the number of values the last fetched resource has.
 * If the no resource is pre-fetched, or if the configuration object is in an error state, 0 is returned.
 *
 * @param cfg Configuration instance to interact with
 *
 * @return Number of values that can be iterated through
 */
size_t ccfg_get_fetch_size(const ccfg_t *cfg);

/**
 * Accesses a single value of a pre-fetched resource and returns it as a string. This function never returns
 * NULL. Instead, if cgui_fetch() and cgui_pick_next_value() weren't called beforehand, an empty string
 * consisting of a single '\0' character is returned instead.
 *
 * @param cfg Configuration instance to interact with
 *
 * @return Converted value
 */
const char *ccfg_get_value(const ccfg_t *cfg);

/**
 * Checks if the configuration is in a failure state due to memory issues.
 *
 * @param cfg Configuration instance to interact with
 *
 * @return Configuration instance error state
 */
bool ccfg_has_failed(const ccfg_t *cfg);

/**
 * Checks which source file will be opened and read through.
 * This function always returns a non-null string pointer. If no source file can be opened, none were added
 * with ccfg_push_source() or the config object has failed an empty null-terminated string is returned
 * instead.
 *
 * @param cfg Configuration instance to interact with
 *
 * @return First source file that can be opened
 */
const char *ccfg_test_sources(const ccfg_t *cfg);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#endif /* CCFG_H */


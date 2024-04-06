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
 * Opaque configuration object that holds all parsed resources.
 * This object holds an internal fail state boolean that can be checked with dr_config_has_failed(). If
 * happens to be put in a failure state due to a memory failure, any function that take this object as
 * argument will return early with no side effects and default return values. The only 2 functions that are
 * an exception to this rule are dr_config_destroy() and dr_config_has_failed().
 */
typedef struct _config_t dr_config_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Allocates memory and initializes a DR configuration object.
 * This function always returns a valid and safe-to-use or destroy object pointer. Even in case of memory
 * allocation failure, the returned value points to an internal static Configuration object set in a failed
 * state. Therefore, checking for a NULL returned value is useless, instead, use dr_config_has_failed().
 *
 * @param n expected amount of resources to load
 *
 * @return Created config object
 */
dr_config_t *dr_config_create(size_t n);

/**
 * Destroy a given object and free allocated memory. The pointed value is then replaced by a placeholder
 * value that points to an internal static configuration object set in a failed state. Hence, it is safe to
 * call this function multiple times.
 *
 * @param cfg Config to interact with
 */
void dr_config_destroy(dr_config_t **cfg);

/**
 * Gets a valid pointer to an internal configuration object set in a failed state. To be used to avoid leaving
 * around uninitialised configuration pointers.
 *
 * @return Placeholder config object
 */
dr_config_t *dr_config_get_placeholder(void);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Clears the list of callback functions that were added with dr_config_push_callback().
 *
 * @param cfg Config to interact with
 */
void dr_config_clear_callbacks(dr_config_t *cfg);

/**
 * Clears the list of source files that were added with dr_config_push_source().
 *
 * @param cfg Config to interact with
 */
void dr_config_clear_sources(dr_config_t *cfg);

/**
 * Reads the first source file that can be opened, parses it, and stores the resolved resources. Once done,
 * callback functions that were added with dr_config_push_callback() will be called successively in the order
 * they were added. Every time this function is called the saved resources will be cleared first before
 * reading the source. The return value will be set to false if the root source file cannot be opened or if
 * the configuration object has been put in a failure state. Note that if the load fails because no source
 * file could not be opened, the configuration object is not put in a failure state.
 *
 * @param cfg Config to interact with
 *
 * @return Load success
 */
bool dr_config_load(dr_config_t *cfg);

/**
 * Adds a function to call every time the given configuration object is done loading a source file. The
 * callback parameter load_success is set to the value of the return of dr_config_load(). The ref parameter
 * here is a pointer of arbitrary value. That same value will be passed down to the callback's ref parameter
 * with no side effects. It is therefore the responsibility of the caller to ensure that the ref's value is
 * valid when the callback is triggered.
 *
 * @param cfg Config to interact with
 * @param fn Callback function
 * @param ref Abitrary pointer value to pass to callback
 */
void dr_config_push_callback(dr_config_t *cfg, void (*fn)(dr_config_t *cfg, bool load_success, void *ref), void *ref);

/**
 * Adds a source file to potentially parse. Only the first added source that can be opened will be parsed,
 * the remaining sources act as fallback.
 *
 * @param cfg Config to interact with
 * @param filename Full path to the source file
 */
void dr_config_push_source(dr_config_t *cfg, const char *filename);

/**
 * Seeds an internal random function that affects the output of random tokens. The seed only affects the
 * given object's instance, meaning multiple configuration objects can co-exist with different seeds.
 *
 * @param cfg Config to interact with
 * @param seed Seed value
 */
void dr_config_seed(dr_config_t *cfg, unsigned long long seed);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Checks if the configuration is in a failure state due to memory issues.
 *
 * @param cfg Config to interact with
 *
 * @return Config error state
 */
bool dr_config_has_failed(const dr_config_t *cfg);

/**
 * Checks which source file will be opened and read through.
 * This function always returns a non-null string pointer. If no source file can be opened, none were added
 * with dr_config_push_source() or the config object has failed an empty null-terminated string is returned
 * instead.
 *
 * @param cfg Config to interact with
 *
 * @return First source file that can be opened
 */
const char *dr_config_test_sources(const dr_config_t *cfg);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DR_CONFIG_H */


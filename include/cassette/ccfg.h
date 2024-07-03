/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Config (CCFG) library.
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

#pragma once

#include <stdbool.h>
#include <stdlib.h>

#if __GNUC__ > 4
	#define CCFG_NONNULL_RETURN __attribute__((returns_nonnull))
	#define CCFG_NONNULL(...)   __attribute__((nonnull (__VA_ARGS__)))
	#define CCFG_PURE           __attribute__((pure))
#else
	#define CCFG_NONNULL_RETURN
	#define CCFG_NONNULL(...)
	#define CCFG_PURE
#endif

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

/**
 * Opaque configuration instance object that holds all parsed resources.
 * This object holds an internal error bitfield that can be checked with ccfg_error(). Some functions, upon
 * failure, can trigger specific error bits and will exit early without side effects. If the error bitfield is
 * set to anything else than CCFG_OK, any function that takes this object as an argument will return early
 * with no side effects and default return values. It is possible to repair the object to get rid of errors.
 * See ccfg_repair() for more details. 
 */
typedef struct ccfg ccfg;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Error types.
 */
enum ccfg_err
{
	CCFG_OK       = 0,
	CCFG_INVALID  = 1,
	CCFG_OVERFLOW = 1 << 1,
	CCFG_MEMORY   = 1 << 2,
};

/************************************************************************************************************/
/* GLOBALS **************************************************************************************************/
/************************************************************************************************************/

/**
 * A macro that gives uninitialized config objects a non-NULL value that is safe to use with the config's
 * related functions. However, any function called with a handle set to this value will return early without
 * any side effects.
 */
#define CCFG_PLACEHOLDER &ccfg_placeholder_instance

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Global string object instance with the error state set to CCFG_INVALID. This instance is only made
 * available to allow the static initialization of string object pointers with the macro CCFG_PLACEHOLDER.
 */
extern ccfg ccfg_placeholder_instance;

/************************************************************************************************************/
/* CONSTRUCTORS / DESTRUCTORS *******************************************************************************/
/************************************************************************************************************/

/**
 * Creates a config instance and deep copy the contents of another config instance into it.
 *
 * @return     : Created config instance
 * @return_err : CCFG_PLACEHOLDER
 */
ccfg *
ccfg_clone(ccfg *cfg)
CCFG_NONNULL_RETURN
CCFG_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Creates an empty config instance.
 *
 * @return     : Created config instance
 * @return_err : CCFG_PLACEHOLDER
 */
ccfg *
ccfg_create(void)
CCFG_NONNULL_RETURN;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Destroys the given config and frees memory.
 *
 * @param cfg : Config instance to interact with
 */
void
ccfg_destroy(ccfg *cfg)
CCFG_NONNULL(1);

/************************************************************************************************************/
/* PROCEDURES ***********************************************************************************************/
/************************************************************************************************************/

/**
 * Convenience generic wrapper for parameter types.
 */
#define cstr_push_param(DST, SRC) \
	_Generic (SRC, \
		char *       : ccfg_push_param_str,    \
		const char * : ccfg_push_param_str,    \
		float        : ccfg_push_param_double, \
		double       : ccfg_push_param_double, \
		default      : ccfg_push_param_long    \
	)(DST, SRC, 0)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 * @param cfg : Config instance to interact with
 */
void
ccfg_clear_resources(ccfg *cfg)
CCFG_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 * @param cfg : Config instance to interact with
 */
void
ccfg_clear_params(ccfg *cfg)
CCFG_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 * @param cfg : Config instance to interact with
 */
void
ccfg_clear_sources(ccfg *cfg)
CCFG_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 * @param cfg       : Config instance to interact with
 * @param namespace : Resource namespace
 * @param property  : Resource property name
 *
 * @return : True if found, false otherwhise
 */
void
ccfg_fetch(ccfg *cfg, const char *namespace, const char *property)
CCFG_NONNULL(1, 2, 3);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 * @param cfg Config instance to interact with
 *
 * @return True is the next value could be picked, false otherwhise.
 */
bool
ccfg_iterate(ccfg *cfg)
CCFG_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 * @param cfg : Config instance to interact with
 *
 * @return : Load success
 */
bool
ccfg_load(ccfg *cfg)
CCFG_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 * @param cfg  : Config instance to interact with
 * @param name : Name of the parameter to use in the source configuration
 * @param d    : Value
 */
void
ccfg_push_param_double(ccfg *cfg, const char *name, double d)
CCFG_NONNULL(1, 2);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 * @param cfg  : Config instance to interact with
 * @param name : Name of the parameter to use in the source configuration
 * @param l    : Value
 */
void
ccfg_push_param_long(ccfg *cfg, const char *name, long long l)
CCFG_NONNULL(1, 2);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 * @param cfg  : Config instance to interact with
 * @param name : Name of the parameter to use in the source configuration
 * @param str  : Value
 */
void
ccfg_push_param_str(ccfg *cfg, const char *name, const char *str)
CCFG_NONNULL(1, 2, 3);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 * @param cfg      : Config instance to interact with
 * @param filename : Full path to the source file
 */
void
ccfg_push_source(ccfg *cfg, const char *filename)
CCFG_NONNULL(1, 2);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
void
ccfg_repair(ccfg *cfg)
CCFG_NONNULL(1);

/************************************************************************************************************/
/* FUNCTIONS ************************************************************************************************/
/************************************************************************************************************/

/**
 *
 * @param cfg : Config instance to interact with
 *
 * @return : 
 */
bool
ccfg_can_open_sources(const ccfg *cfg, size_t *index, const char **filename)
CCFG_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 * @param cfg : Config instance to interact with
 *
 * @return : Config instance error state
 */
enum ccfg_err
ccfg_error(const ccfg *cfg)
CCFG_NONNULL(1)
CCFG_PURE;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 * @param cfg : Config instance to interact with
 *
 * @return : Number of values that can be iterated through
 */
size_t
ccfg_resources_number(const ccfg *cfg)
CCFG_NONNULL(1)
CCFG_PURE;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 * @param cfg : Config instance to interact with
 *
 * @return : Converted value
 */
const char *
ccfg_resource(const ccfg *cfg)
CCFG_NONNULL_RETURN
CCFG_NONNULL(1)
CCFG_PURE;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

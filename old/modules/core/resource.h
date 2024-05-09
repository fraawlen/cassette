/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Derelict Graphics (DG) GUI library.
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

#ifndef DG_CORE_RESOURCE_H
#define DG_CORE_RESOURCE_H

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define DG_CORE_RESOURCE_LEN(X)  (sizeof(X) / sizeof(dg_core_resource_t))
#define DG_CORE_RESOURCE_STR_LEN 64

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Resource value type
 */
typedef enum {
	DG_CORE_RESOURCE_STR,
	DG_CORE_RESOURCE_INT,
	DG_CORE_RESOURCE_UINT,
	DG_CORE_RESOURCE_INT16,
	DG_CORE_RESOURCE_UINT16,
	DG_CORE_RESOURCE_DOUBLE,
	DG_CORE_RESOURCE_UDOUBLE,
	DG_CORE_RESOURCE_COLOR,
	DG_CORE_RESOURCE_BOOL,
} dg_core_resource_kind_t;

/**
 * Resource definition. Will be used to identify properties found in the configuration file. While the target
 * is a generic void pointer, it is expected that it points to a datatype that maches the ressource kind for
 * proper conversion.
 *
 * @param name   : name of the resource as found in the config file
 * @param kind   : type of resource
 * @param target : pointer to value to be affected by resource set in config file
 */
typedef struct {
	const char *name; 
	dg_core_resource_kind_t kind;
	void *target;
} dg_core_resource_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Resource group type
 */
typedef enum {
	DG_CORE_RESOURCE_GROUP_DEFAULT,
	DG_CORE_RESOURCE_GROUP_CUSTOM,
} dg_core_resource_group_kind_t;

/**
 * Struct to group resources together. If it is of the default kind, properties found in the config file will
 * be automatically converted and applied to the given resources. However in case of a custom resource group,
 * the conversion of the value (that will be passed along with the property defintion string) will be the
 * responsability of the owner of the structure. Custom parsers are useful in case the built-in resources
 * kinds handled by the default resource groups are not sufficient.
 * It relies on C11 anonymous unions and structs to set its kind dependent fields.
 * Only use kind dependents fields after checking the kind.
 *
 * @param kind           : kind of resource group
 * @param namespace      : namespace that need to be prepended to every property in the config file
 * @param fn_preprocess  : function to run before config parsing, usually to set default values
 * @param fn_postprocess : function to run after the config has been parsed, usually to set dependent values
 * @param resources      : array of resources to look for
 * @param n              : amount of ressources in the array
 * @param fn_parse       : custom string value parsing function
 *
 * @subparam fn_parse.prop  : property string definition (striped of the namespace)
 * @subparam fn_parse.value : property raw value
 */
typedef struct {
	dg_core_resource_group_kind_t kind;
	const char *namespace;
	const bool (*fn_preprocess)(void);
	const void (*fn_postprocess)(void);
	union {
		/* DG_CORE_RESOURCE_GROUP_DEFAULT */
		struct {
			const dg_core_resource_t *resources;
			uint32_t n;
		};
		/* DG_CORE_RESOURCE_GROUP_CUSTOM */
		const void (*fn_parse)(char *prop, char *value);
	};
} dg_core_resource_group_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Removes a group of resources from the internal tracker. The resources linked to the removed group won't be
 * refreshed during future resources reloads.
 *
 * @param group : group to remove
 */
void dg_core_resource_pull_group(const dg_core_resource_group_t *group);

/**
 * Adds a resource group to the internal tracker. The pushed group will then be automatically reprocessed
 * during each dg_core_resource_reload_*() calls until it is pulled from the internal tracker.
 * fn_preprocess and fn_postprocess are optionals functions that are called respectively before and after
 * each source configuration file read, allowing the setup of default values of the resources linked to the
 * group. if fn_prepocess is present and returns false, file parsing is cancelled, fn_post_process is not
 * called and this function will return false (and so will dg_core_resource_reload).
 * For groups of the default tupe, a resource array and its size are supplied and will be processed by the
 * internal resource parser. In the case of a custom group for special use cases, a parsing fuction has to
 * be provided and the internal parser will not be used.
 *
 * @param group : group to add
 *
 * @return : true on success, false otherwhise (due to memory issues, see errors)
 *
 * @error DG_CORE_ERRNO_STACK : inherited from internal dg_core_stack_t manipulation
 */
bool dg_core_resource_push_group(const dg_core_resource_group_t *group);

/**
 * Refresh all resources and their properties that have been added through dg_core_resource_pull_group().
 * If the environment variable DG_CORE_RESOURCE_FILE is set the file pointed to will be parsed instead of
 * the defaults.
 * If the environment variable DG_CORE_RESOURCE_USE_BUILTIN is set then no file will be parsed (even if
 * DG_CORE_RESOURCE_FILES is set) and only the preprocessing and postprocessing functions of the pushed
 * resources groups will be executed.
 *
 * @return : true on success, false otherwhise (due to memory issues, see errors)
 *
 * @error DG_CORE_ERRNO_MEMORY    : internal memory error
 * @error DG_CORE_ERRNO_STACK     : inherited from internal dg_core_stack_t manipulation
 * @error DG_CORE_ERRNO_HASHTABLE : inherited from internal dg_core_hashtable_t manipulation
 *
 */
bool dg_core_resource_load_all(void);

/**
 * Similar to dg_core_resource_reload_all, except only a single given group is affected.
 * The given group has to be part of the pushed groups ortherwhise this function will fail (however in this
 * case, no errno will be set as it is not a system error).
 *
 * @param group : group to load
 *
 * @return : true on success, false otherwhise (due to memory issues, see errors)
 *
 * @error DG_CORE_ERRNO_MEMORY    : internal memory error
 * @error DG_CORE_ERRNO_STACK     : inherited from internal dg_core_stack_t manipulation
 * @error DG_CORE_ERRNO_HASHTABLE : inherited from internal dg_core_hashtable_t manipulation
 */
bool dg_core_resource_load_group(const dg_core_resource_group_t *group);

/**
 * Similar to dg_core_resource_reload_all, except only the given groups are affected.
 * This function exists to avoid multiple redundant file reads that would happen with multiple
 * successive single group dg_core_resource_load_group() calls.
 * If n == 0, this function has no effect but still returns true.
 * The given groups have to be part of the pushed groups ortherwhise this function will fail (however in this
 * case, no errno will be set as it is not a system error).
 *
 * @param groups : array of group pointer to load
 * @param n      : amount of groups in the array
 *
 * @return : true on success, false otherwhise (due to memory issues, see errors)
 *
 * @error DG_CORE_ERRNO_MEMORY    : internal memory error
 * @error DG_CORE_ERRNO_STACK     : inherited from internal dg_core_stack_t manipulation
 * @error DG_CORE_ERRNO_HASHTABLE : inherited from internal dg_core_hashtable_t manipulation
 */
bool dg_core_resource_load_groups(const dg_core_resource_group_t *const *groups, size_t n);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Set a callback function that gets executed everytime resources get loaded.
 * This function is intended for end-user programs only. Modules, libraries and other middleware should track
 * resources reloads through resource group fn_preprocess or fn_postprocess functions.
 * Call with fn = NULL to unset the callback.
 *
 * @param fn : function to set as callback
 */
void dg_core_resource_set_callback(void (*fn)(void));

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DG_CORE_RESOURCE_H */


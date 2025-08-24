/************************************************************************************************************/
/* PRELUDE **************************************************************************************************/
/************************************************************************************************************/

#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>

#include "cerr.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Opaque dictionary object implemented as a hashmap using the FNV-1a hash function.
 * 	Collisions are resolved using linear probing, and the dictionary automatically grows
 * 	to maintain a maximum load factor (default: 0.6).
 *
 * 	Values are retrieved using both a NUL-terminated string key and a group value.
 *
 * 	Some methods may fail and set an internal error, which can be checked using cdict_error().
 * 	If an error is set, all methods will exit early with default return values and no side
 * 	effects, leaving only the destruction function available.
 */
typedef struct cdict cdict;

/************************************************************************************************************/
/* LIFECYCLE ************************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Destroys a dictionary and frees all associated memory.
 * 	Calling this function on a NULL dictionary has no effect.
 *
 * [Parameters]
 *
 * 	dict - Dictionary to destroy.
 *
 * [Returns]
 *
 * 	To prevent dangling pointers while keeping the function a one-liner, this function
 * 	conveniently returns nullptr.
 */
nullptr_t cdict_destroy(cdict *dict);

/**
 * [Description]
 *
 * 	Creates a dictionary instance and deep copies the content of another dictionary into it.
 *
 * [Parameters]
 *
 * 	dict - Dictionary to copy.
 *
 * [Returns]
 *
 * 	On success, a pointer to a newly allocated instance. Returns nullptr on failure.
 * 	If the dict is NULL or in a critical error state, this function always returns nullptr.
 * 	The caller is responsible for freeing the returned instance using cdict_destroy().
 */
[[nodiscard]] cdict *cdict_clone(const cdict *dict);

/**
 * [Description]
 *
 * 	Creates a new, empty dictionary instance.
 *
 * [Returns]
 *
 * 	On success, a pointer to a newly allocated instance. Returns nullptr on failure.
 * 	The caller is responsible for freeing the returned instance using cdict_destroy().
 */
[[nodiscard]] cdict *cdict_create(void);

/************************************************************************************************************/
/* MUTATION *************************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Clears all active slots in the dictionary.
 * 	Allocated memory is not freed. Use cdict_destroy() for that.
 * 	Calling this function on a NULL dictionary has no effect.
 *
 * [Parameters]
 *
 * 	dict - Dictionary to modify.
 */
void cdict_clear(cdict *dict) [[reproducible]];

/**
 * [Description]
 *
 * 	Clears all active slots associated with a specific group.
 * 	Allocated memory is not freed. Use cdict_destroy() for that.
 * 	Calling this function on a NULL dictionary has no effect.
 *
 * [Parameters]
 *
 * 	dict  - Dictionary to modify.
 * 	group - Group identifier to clear.
 */
void cdict_clear_group(cdict *dict, size_t group) [[reproducible]];

/**
 * [Description]
 *
 * 	Clears any warning error the dictionary may have. Does not clears criticial errors.
 * 	Calling this function on a NULL dictionary has no effect.
 *
 * [Parameters]
 *
 * 	dict - Dictionary to modify.
 */
void cdict_clear_warnings(cdict *dict) [[reproducible]];

/**
 * [Description]
 *
 * 	Deletes the slot that matches the given key and group.
 * 	If no matching slot exists, the function has no effect.
 * 	Allocated memory is not freed. Use cdict_destroy() for that.
 * 	Calling this function on a NULL dictionary has no effect.
 *
 * [Parameters]
 *
 * 	dict  - Dictionary to modify.
 * 	key   - NUL terminated string key to match. Can be NULL.
 * 	group - Group identifier to match.
 */
void cdict_erase(cdict *dict, const char *key, size_t group) [[reproducible]];

/**
 * [Description]
 *
 * 	Preallocates a set number of slots to prevent multiple automatic reallocations
 * 	and rehashes when adding new entries.
 *
 * 	To maintain the dictionary's maximum load factor (default: 0.6), the actual 
 * 	number of allocated slots will be adjusted as (slots_number / max_load_factor).
 *
 * 	This function has no effect if the requested slot number is smaller than the 
 * 	current allocation or a NULL dictionary is given.
 *
 * [Parameters]
 *
 * 	dict         - Dictionary to modify.
 * 	slots_number - Minimum number of slots to preallocate.
 *
 * [Errors]
 *
 * 	CERR_OVERFLOW
 * 	CERR_MEMORY
 */
void cdict_prealloc(cdict *dict, size_t slots_number);

/**
 * [Description]
 *
 * 	Sets the dictionary's maximum load factor.
 * 	To stay under the limit, the dictionary may automatically expand.
 * 	The default load factor is 0.6.
 *
 * 	Values outside the range (0.0, 1.0] are invalid.
 * 	Calling this function on a NULL dictionary has no effect.
 *
 * [Parameters]
 *
 * 	dict        - Dictionary to modify.
 * 	load_factor - New maximum load factor.
 *
 * [Errors]
 *
 * 	CERR_OVERFLOW
 * 	CERR_MEMORY
 * 	CERR_PARAM
 */
void cdict_set_max_load(cdict *dict, double load_factor);

/**
 * [Description]
 *
 * 	Activates a slot in the dictionary's hashtable, associating it with the given key, 
 * 	group, and value. If a slot with a matching key and group already exists, its value 
 * 	will be overwritten.
 *
 * 	The dictionary may automatically allocate additional slots to stay within its 
 * 	maximum load factor (default: 0.6).
 *
 * 	Calling this function on a NULL dictionary has no effect.
 *
 * [Parameters]
 *
 * 	dict  - Dictionary to modify.
 * 	key   - NUL terminated string key. Can be NULL.
 * 	group - Group identifier.
 * 	value - Value to associate with the slot.
 *
 * [Errors]
 *
 * 	CERR_OVERFLOW
 * 	CERR_MEMORY
 */
void cdict_write(cdict *dict, const char *key, size_t group, size_t value);

/************************************************************************************************************/
/* ACCESS ***************************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Retrieves the dictionary's current error state.
 *
 * [Parameters]
 *
 * 	dict - Dictionary to inspect.
 *
 * [Returns]
 *
 * 	The current error code.
 * 	If the dictionary is NULL, this function always returns CERR_INVALID.
 */
enum cerr cdict_error(const cdict *dict) [[reproducible]];

/**
 * [Description]
 *
 * 	Searches for a slot that matches the given key and group.
 * 	If the optional value parameter is not NULL, the associated value will be written to it.
 * 	A NULL key value is equivalent to an empty "" value.
 *
 * [Parameters]
 *
 * 	dict  - Dictionary to search.
 * 	key   - NUL terminated string key to match. Can be NULL.
 * 	group - Group identifier to match.
 * 	value - Optional pointer to store the associated value.
 *
 * [Returns]
 *
 * 	True if a matching slot is found, false otherwhise.
 * 	If the dictionary is NULL or in a critical error state, this function always returns false.
 */
bool cdict_find(const cdict *dict, const char *key, size_t group, size_t *value) [[reproducible]];

/**
 * [Description]
 *
 * 	Retrieves the number of active slots in the dictionary.
 *
 * [Parameters]
 *
 * 	dict - Dictionary to inspect.
 *
 * [Returns]
 *
 * 	The number of active slots.
 * 	If the dictionary is NULL or in a critical error state, this function always returns 0.
 */
size_t cdict_load(const cdict *dict) [[reproducible]];

/**
 * [Description]
 *
 * 	Computes the ratio of active slots to allocated slots.
 *
 * [Parameters]
 *
 * 	dict - Dictionary to inspect.
 *
 * [Returns]
 *
 * 	A floating-point value in the range [0.0, 1.0] representing the load factor.
 * 	If the dictionary is NULL or in a critical error state, this function always returns 0.0.
 */
double cdict_load_factor(const cdict *dict) [[reproducible]];

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

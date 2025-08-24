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
 * 	Opaque input tracker object implemented as a fixed length array that can be manually resized.
 * 	Used to store and track pointer or touch 2D inputs. New inputs are always added at the end of
 * 	the input array. Existing inputs, when refreshed, get pushed to the end of the array too. If
 * 	the array is full, new inputs are ignored.
 *
 * 	Some methods may fail and set an internal error, which can be checked using cinputs_error().
 * 	If a critical error is set, all methods will exit early with default return values and no
 * 	side effects, leaving only the destruction function available.
 */
typedef struct cinputs cinputs;

/************************************************************************************************************/
/* LIFECYCLE ************************************************************************************************/
/************************************************************************************************************/

/** 
 * [Description]
 *
 * 	Destroys an input tracker and frees all associated memory.
 * 	Calling this function on a NULL tracker has no effect.
 *
 * [Parameters]
 *
 * 	inputs - Input tracker to destroy.
 *
 * [Returns]
 *
 * 	To prevent dangling pointers while keeping the function a one-liner, this function
 * 	conveniently returns nullptr.
 */
nullptr_t cinputs_destroy(cinputs *inputs);

/** 
 * [Description]
 *
 * 	Creates an input tracker instance and deep copies the content of another input tracker into
 * 	it.
 *
 * [Parameters]
 *
 * 	inputs - Input tracker to copy.
 *
 * [Returns]
 *
 * 	On success, a pointer to a newly allocated instance. Returns nullptr on failure.
 * 	If the input tracker is NULL or in a critical error state, this function always returns
 * 	nullptr.
 * 	The caller is responsible for freeing the returned instance using cinputs_destroy().
 */
[[nodiscard]] cinputs *cinputs_clone(const cinputs *inputs);

/** 
 * [Description]
 *
 * 	Creates a new, empty input tracker.
 *
 * [Parameters]
 *
 * 	max_inputs - Length of the internal input array. 0 is an illegal value.
 *
 * [Returns]
 *
 * 	On success, a pointer to a newly allocated instance. Returns nullptr on failure.
 * 	The caller is responsible for freeing the returned instance using cinputs_destroy().
 */
[[nodiscard]] cinputs *cinputs_create(size_t max_inputs);

/************************************************************************************************************/
/* MUTATION *************************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Clears the contents of the input tracker.
 * 	Allocated memory is not freed, use cinputs_destroy() for that.
 * 	Calling this function on a NULL input tracker has no effect.
 *
 * [Parameters]
 *
 * 	inputs - Input tracker to modify.
 */
void cinputs_clear(cinputs *inputs) [[reproducible]];

/**
 * [Description]
 *
 * 	Clears any warning error the input tracker may have. Does not clears criticial errors.
 * 	Calling this function on a NULL input tracker has no effect.
 *
 * [Parameters]
 *
 * 	inputs - Input tracker to modify.
 */
void cinputs_clear_warnings(cinputs *inputs) [[reproducible]];

/**
 * [Description]
 *
 * 	Seaches for a tracked input with the matching id. If found, it is removed.
 * 	If an input is removed, the index of other tracked inputs may change.
 * 	This function has no effects if the id is not found or a NULL input tracker is given.
 *
 * [Parameters]
 *
 * 	inputs - Input tracker to modify.
 * 	id     - Identifier to match.
 */
void cinputs_pull_id(cinputs *inputs, unsigned int id) [[reproducible]];

/** 
 * [Description]
 *
 * 	Removes an input at the given index.
 * 	If the input is removed, the index of other inputs may change.
 * 	This function has no effects if index is out of bounds or a NULL input tracker is given.
 *
 * [Parameters]
 *
 * 	inputs - Input tracker to modify.
 * 	index  - Index within the array.
 */
void cinputs_pull_index(cinputs *inputs, size_t index) [[reproducible]];

/**
 * [Description]
 *
 * 	Adds an input at the end of the input tracking array.
 * 	If an input with a matching id already exists within the array, it is pushed to the end of the
 * 	array and its ptr, x and y details are updated.
 * 	This function has no effect if the array is full or a NULL input tracker is given.
 *
 * [Parameters]
 *
 * 	inputs - Input tracker to modify.
 * 	x      - X coordinate.
 * 	y      - Y coordinate.
 * 	ptr    - Arbitrary pointer to something related to the input. Can be NULL.
 */
void cinputs_push(cinputs *inputs, unsigned int id, int x, int y, void *ptr) [[reproducible]];

/**
 * [Description]
 *
 * 	pdates the size of input tracker.
 * 	the requested size is smaller than the current load, tailing inputs will be pulled.
 * 	Calling this function on a NULL input tracker has no effect.
 *
 * [Parameters]
 *
 * 	inputs     - Input tracker to modify.
 * 	max_inputs - Length of the internal input array. 0 is an illegal value.
 *
 * [Errors]
 *
 * 	CERR_OVERFLOW
 * 	CERR_MEMORY
 * 	CERR_PARAM
 */
void cinputs_resize(cinputs *inputs, size_t max_inputs);

/************************************************************************************************************/
/* ACCESS ***************************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Convenience for-loop wrapper.
 * 	Parameter I is declared internally, you should only provide the desired identifier.
 */
#define CINPUTS_FOR_EACH(INPUTS, I) for(size_t I = 0; I < cinputs_load(INPUTS); I++)

/**
 * [Description]
 *
 * 	Convenience inverse for-loop wrapper.
 * 	Parameter I is declared internally, you should only provide the desired identifier.
 */
#define CINPUTS_FOR_EACH_REV(INPUTS, I) for(size_t I = cinputs_load(INPUTS) - 1; I < SIZE_MAX; I--)

/** 
 * [Description]
 *
 * 	Retrieves the input tracker's current error state.
 *
 * [Parameters]
 *
 * 	inputs - Input tracker to inspect.
 * 
 * [Returns]
 *
 * 	The current error code.
 * 	If the input tracker is NULL, this function always returns CERR_INVALID.
 */
enum cerr cinputs_error(const cinputs *inputs) [[reproducible]];

/** 
 * [Description]
 *
 * 	Searches for an input that matches the given id.
 * 	If the optional index parameter is not NULL, and the input is found within the input array,
 * 	the associated input index will be written to it.
 *
 * [Parameters]
 *
 * 	inputs - Input tracker to search.
 * 	id     - Identifier to search.
 * 	index  - Optional parameter to store the associated input index.
 *
 * [Returns]
 *
 * 	True if a matching input is found, false otherwhise.
 * 	If the input tracker is NULL or in a critical error state, this function always return false.
 */
bool cinputs_find(const cinputs *inputs, unsigned int id, size_t *index) [[reproducible]];

/** 
 * [Description]
 *
 * 	Retrieves the id of an input at an index.
 *
 * [Parameters]
 *
 * 	inputs - Input tracker to inspect.
 * 	index  - Index within the array.
 *
 * [Returns]
 *
 * 	Input Id at index.
 * 	If the input tracker is NULL, in a criticial error state, or the index is out of bounds, this
 * 	function always return 0.
 */
unsigned int cinputs_id(const cinputs *inputs, size_t index) [[reproducible]];

/** 
 * [Description]
 *
 * 	Retrieves the total number of tracked inputs.
 *
 * [Parameters]
 *
 * 	inputs - Input tracker to inspect.
 *
 * [Returns]
 *
 * 	Number of unique inputs tracked.
 * 	If the input tracker is NULL or in a critical error state, this function always returns 0.
 */
size_t cinputs_load(const cinputs *inputs) [[reproducible]];

/** 
 * [Description]
 *
 * 	Retrieves the pointer of an input at an index.
 *
 * [Parameters]
 *
 * 	inputs - Input tracker to inspect.
 * 	index  - Index within the array.
 *
 * [Returns]
 *
 * 	Input arbitrary pointer at index.
 * 	If the input tracker is NULL, in a criticial error state, or the index is out of bounds,
 * 	this function always return nullptr.
 */
void *cinputs_ptr(const cinputs *inputs, size_t index) [[reproducible]];

/** 
 * [Description]
 *
 * 	Retrieves the X coordinate of an input at an index.
 *
 * [Parameters]
 *
 * 	inputs - Input tracker to inspect.
 * 	index  - Index within the array.
 *
 * [Returns]
 *
 * 	Input coordinate at index.
 * 	If the input tracker is NULL, in a criticial error state, or the index is out of bounds,
 * 	this function always return 0.
 */
int16_t cinputs_x(const cinputs *inputs, size_t index) [[reproducible]];

/** 
 * [Description]
 *
 * 	Retrieves the Y coordinate of an input at an index.
 *
 * [Parameters]
 *
 * 	inputs - Input tracker to inspect.
 * 	index  - Index within the array.
 *
 * [Returns]
 *
 * 	Input coordinate at index.
 * 	If the input tracker is NULL, in a criticial error state, or the index is out of bounds,
 * 	this function always return 0.
 */
int16_t cinputs_y(const cinputs *inputs, size_t index) [[reproducible]];

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

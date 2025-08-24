/************************************************************************************************************/
/* PRELUDE **************************************************************************************************/
/************************************************************************************************************/

#pragma once

#include <cassette/cobj.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Opaque configuration parser object that hold settings (sources and parameters) and resolved
 * 	resources that were parsed previously. CCFG's parser has been designed to store parsed data
 * 	then use getter functions instead of an event-based architecture to be able to share a
 * 	configuration in software plugins (and without said plugins needing to hook themselves into
 * 	the target program before said program main routine starts).
 *
 * 	Resources definitions follow the CCFG language specification.
 *
 * 	Some methods may fail and set an internal error, which can be checked using ccfg_error().
 * 	If an error is set, all methods will exit early with default return values and no side
 * 	effects, leaving only the destruction function available.
 */
typedef struct ccfg ccfg;

/**
 * [Description]
 *
 * 	Opaque cursor value that holds active fetch and iteration states.
 */
typedef struct { uintptr_t data[4]; } ccfg_cursor;

/************************************************************************************************************/
/* GLOBALS **************************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Maximum length of a CCFG token. NUL terminator included.
 */
#define CCFG_TOKEN_LENGTH 256

/************************************************************************************************************/
/* LIFECYCLE ************************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Destroys a parser and frees all associated memory.
 * 	Calling this function on a NULL parser has no effect.
 *
 * [Parameters]
 *
 * 	cfg - Parser to destroy.
 *
 * [Returns]
 *
 * 	To prevent dangling pointers while keeping the function a one-liner, this function
 * 	conveniently returns nullptr.
 */
nullptr_t ccfg_destroy(ccfg *cfg);

/**
 * [Description]
 *
 * 	Creates a parser instance and deep copies the contents of another parser into it.
 *
 * [Parameters]
 *
 * 	cfg - Parser to copy.
 *
 * [Returns]
 *
 * 	On succes, a pointer to a newly allocated instance. Returns nullptr on failure.
 * 	If the parser is NULL or in a critical error state, this function always returns nullptr.
 * 	The caller is responsible for freeing the returned instance using ccfg_destroy().
 */
[[nodiscard]] ccfg *ccfg_clone(ccfg *cfg);

/**
 * [Description]
 *
 * 	Creates a new, empty config instance.
 *
 * [Returns]
 *
 * 	On succes, a pointer to a newly allocated instance. Returns nullptr on failure.
 * 	The caller is responsible for freeing the returned instance using ccfg_destroy().
 */
[[nodiscard]] ccfg *ccfg_create(void);

/************************************************************************************************************/
/* MUTATION *************************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Convenience generic wrapper for parameter types.
 */
#define ccfg_push_param(CFG, NAME, VAL) \
	_Generic (VAL, \
		char *       : ccfg_push_param_str,    \
		const char * : ccfg_push_param_str,    \
		nullptr_t    : ccfg_push_param_str,    \
		float        : ccfg_push_param_double, \
		double       : ccfg_push_param_double, \
		default      : ccfg_push_param_long    \
	)(CFG, NAME, VAL)

/**
 * [Description]
 *
 * 	Convenience wrapper that fetches and loop throught resources.
 * 	Parameters VALUE and I are declared internally, and their scope is contained inside the 
 * 	macro, you should only provide the desired identifier. N limits the number of loops.
 *
 * [Notes]
 *
 * 	Two for loops are used in this macro to declare two scoped variables (VALUE and I).
 *
 * [Example]
 *
 * 	Instead of:
 *
 * 	ccfg_fetch(cfg, "name", "prop");
 * 	for (size_t i = 0; i < 3 && ccfg_iterate(cfg); i++)
 * 	{
 * 		target[i] = convertion_function(ccfg_resource(cfg));
 * 	}
 *
 * 	You can do:
 *
 * 	CCFG_RESOURCES(cfg, "name", "prop", val, i, 3)
 * 	{
 * 		target[i] = convertion_function(val);
 * 	}
 */
#define CCFG_RESOURCES(CFG, NAME, PROP, VALUE, I, N) \
	ccfg_fetch(CFG, NAME, PROP); \
	for (size_t I = 0; I == 0; I = 1) \
	for (const char *VALUE; ccfg_iterate(CFG) && I < N && (VALUE = ccfg_resource(CFG)); I++)

/**
 * [Description]
 *
 * 	Removes all added parameters.
 * 	Allocated memory is not freed, use ccfg_destroy() for that.
 * 	Calling this function on a NULL parser has no effect.
 *
 * [Parameters]
 *
 * 	cfg - Parser to modify.
 */
void ccfg_clear_params(ccfg *cfg) [[reproducible]];

/**
 * [Description]
 *
 * 	Removes all parsed resources.
 * 	Allocated memory is not freed, use ccfg_destroy() for that.
 * 	Calling this function on a NULL parser has no effect.
 *
 * [Parameters]
 *
 * 	cfg - Parser to modify.
 */
void ccfg_clear_resources(ccfg *cfg) [[reproducible]];

/**
 * [Description]
 *
 * 	Removes all added sources.
 * 	Allocated memory is not freed, use ccfg_destroy() for that.
 * 	Calling this function on a NULL parser has no effect.
 *
 * [Parameters]
 *
 * 	cfg - Parser to modify.
 */
void ccfg_clear_sources(ccfg *cfg) [[reproducible]];

/**
 * [Description]
 *
 * 	Clears any warning error the parser may have. Does not clears criticial errors.
 * 	Calling this function on a NULL parser has no effect.
 *
 * [Parameters]
 *
 * 	cfg - Parser to modify.
 */
void ccfg_clear_warnings(ccfg *cfg) [[reproducible]];

/**
 * [Description]
 *
 * 	Looks-up a resource by its namespace (group) and property name. If found, its reference is kept
 * 	around and the resource values will become accessible through ccfg_iterate() and ccfg_resource().
 * 	To get the number of values a resource has, use ccfg_resouce_length().
 * 	After calling this function, as long as ccfg_iterate() is not called at least once, the
 * 	associated resource values will not be accessible.
 *
 * 	A NULL namespace (group) or property value is equivalent to an empty "" value.
 * 	Calling this function on a NULL parser has no effect.
 *
 * [Example]
 *
 *	ccfg_fetch(cfg, "something", "something");
 *	while (ccfg_iterate(cfg))
 *	{
 *		printf("%s\n", ccfg_resource(cfg));
 *	}
 *
 * [Parameters]
 *
 * 	cfg      - Parser to modify.
 * 	group    - Resource namespace.
 * 	property - Resource property name.
 */
void ccfg_fetch(ccfg *cfg, const char *group, const char *property) [[reproducible]];

/**
 * [Description]
 *
 * 	Increments an internal iterator offset and makes available the next value associated to with 
 * 	resource fetched with ccfg_fetch(). This value can be accessed with ccfg_resource().
 * 	Calling this function on a NULL parser has no effect.
 *
 * [Parameters]
 *
 * 	cfg - Parser to modify.
 *
 * [Returns]
 *
 * 	After the iterator has been successfully incremented this function returns true.
 * 	Otherwhise, if the iterator already reached the end and cannot be incremented further, this
 * 	function returns false.
 * 	If the parser is NULL or in a critical error state, this function always returns false.
 */
bool ccfg_iterate(ccfg *cfg);

/**
 * [Description]
 *
 * 	Reads the first source file that can be opened, parses it, and stores the resolved resources.
 * 	Every time this function is called the previously parsed resources will be cleared first
 * 	before reading the source.
 *
 * 	If not source file can be read, then this function behaves like ccfg_clear_resources().
 * 	Not being able to open any source files is not considered to be an error. If such a check is
 * 	needed, use ccfg_can_open_sources().
 * 	Calling this function on a NULL parser has no effect.
 *
 * [Parameters]
 *
 * 	cfg - Parser to modify.
 *
 * [Errors]
 *
 * 	CERR_OVERFLOW
 * 	CERR_MEMORY
 */
void ccfg_load(ccfg *cfg);

/**
 * [Description]
 *
 * 	Similar to ccfg_load() except that no source file is opened. Instead, the resources will be
 * 	parsed from the passed NUL terminated buffer. Unlike file parsing, INCLUDE sequences (see the
 * 	CCFG language specification) will be ignored.
 *
 * 	If the buffer is NULL or empty, then this function behaves like ccfg_clear_resources().
 * 	Calling this function on a NULL parser has no effect.
 *
 * [Parameters]
 *
 * 	cfg    - Parser to modify.
 * 	buffer - Source to parse.
 *
 * [Errors]
 *
 * 	CERR_OVERFLOW
 * 	CERR_MEMORY
 */
void ccfg_load_internal(ccfg *cfg, const char *buffer);

/**
 * [Description]
 *
 * 	Registers a parameter in the form of a double floating value in the parser.
 * 	Any parameter resources with the related name will be subtituted with the given value during
 * 	source parsing. Unlike user-defined variables, only one value per parameter can be defined.
 * 	See the CCFG language specification for more information.
 * 	Calling this function on a NULL parser has no effect.
 *
 * [Parameters]
 *
 * 	cfg  - Parser to modify.
 * 	name - Parameter name.
 * 	d    - Double value.
 *
 * [Errors]
 *
 * 	CERR_OVERFLOW
 * 	CERR_MEMORY
 */
void ccfg_push_param_double(ccfg *cfg, const char *name, double d) [[reproducible]];

/**
 * [Description]
 *
 * 	Registers a parameter in the form of a long integer value in the parser.
 * 	Any parameter resources with the related name will be subtituted with the given value during
 * 	source parsing. Unlike user-defined variables, only one value per parameter can be defined.
 * 	See the CCFG language specification for more information.
 * 	Calling this function on a NULL parser has no effect.
 *
 * [Parameters]
 *
 * 	cfg  - Parser to modify.
 * 	name - Parameter name.
 * 	l    - Long value.
 *
 * [Errors]
 *
 * 	CERR_OVERFLOW
 * 	CERR_MEMORY
 */
void ccfg_push_param_long(ccfg *cfg, const char *name, long long l) [[reproducible]];

/**
 * [Description]
 *
 * 	Registers a parameter in the form of a NUL terminated string in the parser.
 * 	Any parameter resources with the related name will be subtituted with the given value during
 * 	source parsing. Unlike user-defined variables, only one value per parameter can be defined.
 * 	See the CCFG language specification for more information.
 * 	Calling this function on a NULL parser has no effect.
 *
 * [Parameters]
 *
 * 	cfg  - Parser to modify.
 * 	name - Parameter name.
 * 	str  - NUL terminated C string.
 *
 * [Errors]
 *
 * 	CERR_OVERFLOW
 * 	CERR_MEMORY
 */
void ccfg_push_param_str(ccfg *cfg, const char *name, const char *str) [[reproducible]];

/**
 * [Description]
 *
 * 	Registers a new source file.
 * 	Only the first source that can be opened will be parsed.
 * 	The remaining sources act as fallback.
 *
 * 	A NULL filename is equivalent to an empty "" value.
 * 	Calling this function on a NULL parser has no effect.
 *
 * [Parameters]
 *
 * 	cfg      - Parser to modify.
 * 	filename - Full path to the source file.
 *
 * [Errors]
 *
 * 	CERR_OVERFLOW 
 * 	CERR_MEMORY
 */
void ccfg_push_source(ccfg *cfg, const char *filename) [[reproducible]];

/**
 * [Description]
 *
 * 	Restores the fetched resources and iterator progress snapshoted with ccfg_snap().
 * 	Calling this function on a NULL parser, or invalid cursor has no effect.
 *
 * [Parameters]
 *
 * 	cfg    - Parser to modify.
 * 	cursor - State snapshot.
 */
void ccfg_restore(ccfg *cfg, const ccfg_cursor cursor) [[reproducible]];

/**
 * [Description]
 *
 * 	Enables the restricted parsing mode.
 * 	See the CCFG language specification for more information.
 * 	Calling this function on a NULL parser has no effect.
 *
 * [Parameters]
 *
 * 	cfg - Parser to modify.
 */
void ccfg_restrict(ccfg *cfg) [[reproducible]];

/**
 * [Description]
 *
 * 	Disables the restricted parsing mode.
 * 	See the CCFG language specification for more information.
 * 	Calling this function on a NULL parser has no effect.
 *
 * [Parameters]
 *
 * 	cfg - Parser to modify.
 */
void ccfg_unrestrict(ccfg *cfg) [[reproducible]];

/************************************************************************************************************/
/* ACCESS ***************************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Checks which source can be opened and read.
 * 	If the optional index paramter is not NULL, this function will write into it the rank of the
 * 	first source file that was opened. If no source can be opened, the value this parameter points
 * 	to will not be modified.
 *
 * [Parameters]
 *
 * 	cfg   - Parser to inspect.
 * 	index - Optional pointer to store the source rank.
 *
 * [Returns]
 *
 * 	True if any source file can be opened, false otherwhise.
 * 	If the parser is NULL or in a critical error state, this function always returns false.
 */
bool ccfg_can_open_sources(const ccfg *cfg, size_t *index);

/**
 * [Description]
 *
 * 	Retrieves the parser's current error state.
 *
 * [Parameters]
 *
 * 	cfg - Parser to inspect.
 *
 * [Returns]
 *
 * 	The current error code.
 * 	If the parser is NULL, this function always returns CERR_INVALID.
 */
enum cerr ccfg_error(const ccfg *cfg) [[reproducible]];

/**
 * [Description]
 *
 * 	Retrieves the resource value an internal iterator is pointing at.
 * 	It's the responsibility of the caller to convert it into the desired datatype.
 *
 * [Parameters]
 *
 * 	cfg - Parser to inspect.
 *
 * [Returns]
 *
 * 	The resource value as a NUL terminated C string.
 * 	If the resource iterator was not set with ccfg_fetch() and ccfg_iterate(), the parser is
 * 	NULL, or in a criticial error state, this function always returns an empty "" string.
 * 	This function never returns nullptr.
 */
[[gnu::returns_nonnull]] const char *ccfg_resource(const ccfg *cfg) [[reproducible]];

/**
 * [Description]
 *
 * 	Retrieves the number of values a pre-fetched resource has.
 *
 * [Parameters]
 *
 * 	cfg - Parser to inspect.
 *
 * [Returns]
 *
 * 	Number of values if any.
 * 	If the parser is NULL or in a critical error state, this function always returns 0.
 */
size_t ccfg_resource_length(const ccfg *cfg) [[reproducible]];

/**
 * [Description]
 *
 * 	Snapshots into an opaque struct the current fetched resource and iterator state.
 *
 * [Parameters]
 *
 * 	cfg - Parser to snapshot.
 *
 * [Returns]
 *
 * 	Parser iterator snapshot.
 * 	If no resource is fetched or if the parser is in a critical error state, then the returned snapshot
 * 	is invalid (but safe to use).
 */
ccfg_cursor ccfg_snap(const ccfg *cfg) [[reproducible]];

/**
 * [Description]
 *
 * 	Checks the validity of a given cursor against a parser.
 * 	For the cursor to be valid, the given parser instance and load should match the cursor's when it was
 * 	snapshoted, and a ressource has to have been fetched.
 *
 * [Parameters]
 *
 * 	cfg    - Parser to inspect.
 * 	cursor - Cursor snapshot to check.
 *
 * [Returns]
 *
 * 	Validity of the cursor.
 * 	If the parser is NULL or in a critical error state, this function always returns 0.
 */
bool ccfg_valid_cursor(const ccfg *cfg, const ccfg_cursor cursor) [[reproducible]];

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

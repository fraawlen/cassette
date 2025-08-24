/************************************************************************************************************/
/* PRELUDE **************************************************************************************************/
/************************************************************************************************************/

#pragma once

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Cassette errors used by COBJ, CCFG and CGUI.
 */
enum cerr
{
	CERR_NONE = 0,

	/* Warnings */

	CERR_PARAM,

	/* Critical */

	CERR_INVALID,
	CERR_OVERFLOW,
	CERR_MEMORY,
	CERR_CONFIG,
	CERR_XCB,
	CERR_CAIRO,
	CERR_MUTEX,
	CERR_INSTANCE,
	CERR_MALFORMED,
};

/************************************************************************************************************/
/* MUTATION *************************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Cleats any warning from the error. Does not clears criticial errors.
 * 	Calling this function on a NULL error has no effect.
 *
 * [Parameters]
 *
 * 	err  - Error to update.
 */
void cerr_clear_warnings(enum cerr *err);

/** 
 * [Description]
 *
 * 	Sets a new error code to the given err enum, but only if the new error if of higher severity.
 * 	Calling this function on a NULL error enum has no effect.
 *
 * [Parameters]
 *
 * 	err  - Error enum to update.
 * 	code - Error code.
 */
void cerr_set(enum cerr *err, enum cerr code);

/************************************************************************************************************/
/* ACCESS ***************************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Converts an error code into a string.
 *
 * [Parameters]
 *
 * 	code - Error code to get the name of.
 *
 * [Returns]
 *
 * 	NUL terminated string representing the error code.
 */
[[gnu::const]] const char *cerr_name(enum cerr code);

/**
 * [Description]
 *
 * 	Checks the severity of an error.
 *
 * [Parameters]
 *
 * 	code - Error code to check.
 *
 * [Returns]
 *
 * 	True if it is critical, false it's a warning or no error was set.
 */
[[gnu::const]] bool cerr_critical(enum cerr code);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

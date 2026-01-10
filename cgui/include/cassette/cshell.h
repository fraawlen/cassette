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
 * 	Opaque shell object. A shell represent a top-level window and is a container for cgrids and ccells.
 * 	Shells directly abstract and interact with the available backends. Each independent shell opens and
 * 	manages an unique connection to its backend. Multiple shells with different can be opened.
 *
 * 	Some methods may fail and set an internal error, which can be checked using cshell_error().
 * 	If a critical error is set, all methods will exit early with default return values and no
 * 	side effects, leaving only the destruction function available.
 */
typedef struct cshell cshell;

/************************************************************************************************************/
/* LIFECYCLE ************************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Destroys a shell and frees all associated memory.
 * 	Calling this function on a NULL shell has no effect.
 * 	Because the shell runs a thread when open, this function should exclusively be called when the
 * 	shell is closed, otherwhise this function raises a warning then exits with no further side effects.
 *
 * [Parameters]
 *
 * 	sh - Shell to destroy.
 *
 * [Returns]
 *
 * 	To prevent dangling pointers while keeping the function a one-liner, this function
 * 	conveniently returns nullptr.
 *
 * [Errors]
 *
 * 	CERR_CALL
 */
nullptr_t cshell_destroy(cshell *sh);

/**
 * [Description]
 *
 * 	Creates a new, empty shell instance.
 *
 * [Returns]
 *
 * 	On succes, a pointer to a newly allocated instance. Returns nullptr on failure.
 * 	The caller is responsible for freeing the returned instance using cshell_destroy().
 */
[[nodiscard]] cshell *cshell_create(void);

/************************************************************************************************************/
/* MUTATION - THREAD SAFE ***********************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Force closes the shell.
 * 	Calling this function on a NULL or already closed shell has no effect.
 *
 * [Parameters]
 *
 * 	sh - Shell to modify.
 */
void cshell_close(cshell *sh);

/**
 * [Description]
 *
 * 	Calls the given function from within the shell's event thread.
 * 	Use it to safely modify the shell while it is open.
 * 	Calling this function on a NULL shell or function has no effect.
 * 	If called from the shell's thread, or while the shell is not open, this function executes the
 * 	passed callback immediately.
 *
 * [Parameters]
 *
 * 	sh   - Shell to modify.
 * 	fn   - Function to invoke.
 * 	data - Data to be passed to the invoked function.
 */
void cshell_invoke(cshell *sh, void (*fn)(cshell *, void *), void *data);

/**
 * [Description]
 *
 * 	Waits until the shell's thread is terminated after the shell is closed.
 * 	Calling this function on a NULL or closed shell, or from the shell's thread has no effect.
 *
 * [Parameters]
 *
 * 	sh - Shell to modify.
 */
void cshell_join(cshell *sh);

/**
 * [Description]
 *
 * 	Initialises the shell's backend and configuration, then open it on the display server.
 * 	After the shell is opened, all event processing is offloaded to a hidden thread, then this
 * 	function exits immediately. To wait until the shell is closed, use cshell_join().
 * 	Calling this function on a NULL or already open shell has no effect.
 *
 * [Parameters]
 *
 * 	sh - Shell to modify.
 *
 * [Errors]
 *
 * 	CERR_THREAD
 */
void cshell_open(cshell *sh);

/**
 *
 */
void cshell_wait(cshell *sh);

/************************************************************************************************************/
/* MUTATION *************************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Clears any warning error the shell may have. Does not clears criticial errors.
 * 	Calling this function on a NULL shell has no effect.
 *
 * [Parameters]
 *
 * 	sh - Shell to modify.
 */
void cshell_clear_warnings(cshell *sh);

/**
 *
 */
void cshell_on_close(cshell *sh, void (*fn)(cshell *, void *), void *data);

/**
 *
 */
void cshell_on_open(cshell *sh, void (*fn)(cshell *, void *), void *data);

/**
 *
 */
void cshell_on_destroy(cshell *sh, void (*fn)(cshell *, void *), void *data);

/************************************************************************************************************/
/* ACCESS - THREAD SAFE *************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Retrieves the shell's current error state.
 *
 * [Parameters]
 *
 * 	sh - Shell to inspect.
 *
 * [Returns]
 *
 * 	The current error code.
 * 	If the shell is NULL, this function always returns CERR_INVALID.
 */
enum cerr cshell_error(const cshell *sh);

/**
 * [Description]
 *
 * 	Checks if a given shell is open and processing events.
 * 	This function relies on an atomic variable and is thread safe.
 *
 * [Parameters]
 *
 * 	sh - Shell to inspect.
 *
 * [Returns]
 *
 * 	The state of the shell.
 * 	If the shell is NULL, this function always returns false.
 */
bool cshell_opened(const cshell *sh);

/**
 *
 */
bool cshell_self(const cshell *sh);

/************************************************************************************************************/
/* ACCESS ***************************************************************************************************/
/************************************************************************************************************/



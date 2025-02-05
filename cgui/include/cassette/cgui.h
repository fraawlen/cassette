/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Graphics (CGUI) library.
 *
 * This library is free software; you can redistribute it and/or modify it either under the terms of the GNU
 * Lesser General Public License as published by the Free Software Foundation; either version 3.0 of the
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

#include <cassette/cobj.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <xcb/xcb.h>

/* Top-level headers */

#include "cgui-clipboard.h"
#include "cgui-event.h"
#include "cgui-grid.h"
#include "cgui-screen.h"
#include "cgui-window.h"

/* Top-level headers - Cells */

#include "cgui-beacon.h"
#include "cgui-button.h"
#include "cgui-filler.h"
#include "cgui-gauge.h"
#include "cgui-label.h"
#include "cgui-placeholder.h"
#include "cgui-stripes.h"

/* Low-level headers */

#include "cgui-attributes.h"
#include "cgui-box.h"
#include "cgui-cell.h"
#include "cgui-config.h"
#include "cgui-swap.h"
#include "cgui-text.h"
#include "cgui-types.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/* GLOBALS **************************************************************************************************/
/************************************************************************************************************/

/**
 *  The current CGUI version string.
 */
#define CGUI_VERSION "0.2.0"

/************************************************************************************************************/
/* INIT / RESET *********************************************************************************************/
/************************************************************************************************************/

/**
 * Initializes the CGUI library.
 * Argc and argv are used to set window ICCCM properties.
 * Before running this function CGUI's error state is set to CERR_INVALID and is set to CERR_NONE when it
 * succeeds. If it fails to initialize completely, any initialized internals get reseted cleanly.
 *
 * @param argc : Main's argc.
 * @param argv : Main's argv.
 */
void
cgui_init(int argc, char **argv);

/**
 * Resets the CGUI library to its initial state.
 * Marks all instanced windows, grids and cell as invalid, clears configuration and resources, resets
 * internal trackers and frees memory. CGUI's error will be set to CERR_INVALID when this function exits.
 * If CGUI was using its own, internally created XCB connection, it will close it.
 */
void
cgui_reset(void);

/************************************************************************************************************/
/* PRE-INIT METHODS *****************************************************************************************/
/************************************************************************************************************/

/**
 * Sets a custom application class for the CGUI library. If not called, a value will be generated from
 * from cgui_init()'s argc and argv.
 * This function must be called before cgui_init().
 *
 * @param class_name : NUL terminated C string
 */
void
cgui_setup_app_class(const char *class_name);

/**
 * Sets a custom application name for the CGUI library. If not called, a value will be generated from
 * cgui_init()'s argc and argv.
 * This function must be called before cgui_init()..
 *
 * @param name : NUL terminated C string
 */
void
cgui_setup_app_name(const char *name);

/**
 * Sets up an external X11 connection to use instead of cgui_init() creating its own.
 *
 * @param connection : X11 connection.
 */
void
cgui_setup_x11_connection(xcb_connection_t *connection);

/************************************************************************************************************/
/* IMPURE METHODS *******************************************************************************************/
/************************************************************************************************************/

/**
 * Allows the user to exit cgui_run() through a shortcut.
 */
void
cgui_allow_user_exit(void);

/**
 * Blocks the user from exiting cgui_run() through a shortcut.
 */
void
cgui_block_user_exit(void);

/**
 * Broadcasts a reconfiguration signal to every CGUI window on the display server.
 *
 * @error CERR_XCB
 */
void
cgui_broadcast_reconfig(void);

/**
 * Forcefully exits cgui_run() without waiting for all windows to deactivate.
 */
void
cgui_exit(void);

/**
 * Locks an internal mutex for multithreaded GUI applications. The mutex is recursive.
 */
void
cgui_lock(void);

/**
 * Registers a callback function to be executed upon exiting cgui_run().
 * If a NULL function pointer is given, the callback is disabled.
 * Usefull in multithreading scenarios.
 *
 * @param fn : Callback function
 */
void
cgui_on_exit(void (*fn)(void));

/**
 * Registers a callback function to be executed when cgui_run() enters its main event loop.
 * If a NULL function pointer is given, the callback is disabled.
 * Usefull in multithreading scenarios.
 *
 * @param fn : Callback function
 */
void
cgui_on_run(void (*fn)(void));

/**
 * Reloads configuration settings and update every active window accordingly.
 *
 * @error CERR_OVERFLOW
 * @error CERR_MEMORY
 * @error CERR_CONFIG
 * @error CERR_XCB
 */
void
cgui_reconfig(void);

/**
 * Attemps to repair the internal state of the CGUI library if an error occured. On success, the error value
 * is set back to CERR_NONE.
 */
void
cgui_repair(void);

/**
 * Enters the main loop, processes events, and updates windows until either cgui_exit() or cgui_reset() are 
 * called, an error occurs, all windows are deactivated or the end-user trigger (with a shortcut) an
 * application exit.
 *
 * @error CERR_MEMORY
 * @error CERR_OVERFLOW
 * @error CERR_CONFIG
 * @error CERR_PARAM
 * @error CERR_XCB
 */
void
cgui_run(void);

/**
 * Releases an internal mutex for multithreaded GUI applications.
 */
void
cgui_unlock(void);

/************************************************************************************************************/
/* PURE METHODS *********************************************************************************************/
/************************************************************************************************************/

/**
 * Retrieves the current error state.
 *
 * @return : Error code
 */
enum cerr
cgui_error(void)
CGUI_PURE;

/**
 * Checks if the CGUI library is initialized.
 *
 * @return     : True if initialization completed successfully; false otherwise
 * @return_err : False
 */
bool
cgui_is_init(void)
CGUI_PURE;

/**
 * Checks if the CGUI library is currently running.
 *
 * @return     : True if the main loop is running; false otherwise
 * @return_err : False
 */
bool
cgui_is_running(void)
CGUI_PURE;

/**
 * Retrieves the X11 connection used by the CGUI library.
 *
 * @return     : XCB connection used by CGUI
 * @return_err : NULL
 */
xcb_connection_t *
cgui_x11_connection(void)
CGUI_PURE;

/**
 * Retrieves the leader X11 window ID (an ICCCM convention).
 *
 * @return     : XCB window ID
 * @return_err : 0.
 */
xcb_window_t
cgui_x11_leader_window(void)
CGUI_PURE;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif


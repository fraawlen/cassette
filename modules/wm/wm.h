/**
 * Copyright © 2024 Frawwlen <fraawlen@posteo.net>
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

#ifndef DG_CORE_WM_H
#define DG_CORE_WM_H

#include <stdbool.h>
#include <stdlib.h>

#include <xcb/xcb.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/* MAIN *****************************************************************************************************/
/************************************************************************************************************/

/**
 * Initialises this module and enables the other functions of this header (unless explicitely stated).
 * Should only be called once until dg_wm_reset(). Can be called again after.
 *
 * @error DG_CORE_ERRNO_XCB          : failed to fetch X atoms
 * @error DG_CORE_ERRNO_XCB_CRITICAL : failed to setup an X sesssion, the module can't initialize
 */
void dg_wm_init(xcb_connection_t *connection);

/**
 * Resets the module to its inital state pre dg_wm_init() and frees all memory internally allocated.
 * This function can be used when the module is not initialized.
 */
void dg_wm_reset(void);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Checks if the module has been initialized.
 * This function can be used when the module is not initialized.
 *
 * @return : self-explanatory
 */
bool dg_wm_is_init(void);

/************************************************************************************************************/
/* WM HELPERS ***********************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
void dg_wm_reconfig_client(xcb_window_t window);

/**
 *
 */
void dg_wm_reconfig_all(void);

/**
 *
 */
void dg_wm_trigger_accelerator(xcb_window_t window, int accel_id);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Checks whether a given X window is a DG window or not.
 *
 * @param window  : window to check
 * @param version : optional version string pointer, if given, the DG version string will be copied into it,
 *                  the array is malloc'd internally and therefore needs to be explicitely freed.
 *
 * @return : self-explanatory
 *
 * @error DG_CORE_ERRNO_MEMORY : failed to copy version string
 * @error DG_CORE_ERRNO_XCB    : failed to fetch X properties
 */
bool dg_wm_is_dg_client(xcb_window_t window, char **version);

/**
 *
 */
bool dg_wm_test_accelerator(xcb_window_t window, int accel_id, char **name);

/**
 * Checks whether a given X window supports DG signals.
 *
 * @param window : window to check
 *
 * @return : self-explanatory
 */
bool dg_wm_test_signals(xcb_window_t window);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DG_CORE_WM_H */


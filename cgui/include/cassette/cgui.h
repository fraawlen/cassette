/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Graphics (CGUI) library.
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

#include <cassette/cobj.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <xcb/xcb.h>

/* Init dependent headers */

#include "cgui-cell.h"
#include "cgui-config.h"
#include "cgui-event.h"
#include "cgui-grid.h"
#include "cgui-screen.h"
#include "cgui-swap.h"
#include "cgui-window.h"

/* Init dependent headers - Cells */

#include "cgui-button.h"
#include "cgui-filler.h"
#include "cgui-stripes.h"

/* Independent headers */

#include "cgui-attributes.h"
#include "cgui-box.h"
#include "cgui-types.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/* GLOBALS **************************************************************************************************/
/************************************************************************************************************/

#define CGUI_VERSION "0.2.0"

/************************************************************************************************************/
/* INIT / RESET *********************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
void
cgui_init(int argc, char **argv);

/**
 *
 */
void
cgui_reset(void);

/************************************************************************************************************/
/* PRE-INIT METHODS *****************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
void
cgui_setup_app_class(const char *class_name);

/**
 *
 */
void
cgui_setup_app_name(const char *name);

/**
 *
 */
void
cgui_setup_x11_connection(xcb_connection_t *connection);

/************************************************************************************************************/
/* IMPURE METHODS *******************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
void
cgui_allow_user_exit(void);

/**
 *
 */
void
cgui_block_user_exit(void);

/**
 *
 */
void
cgui_exit(void);

/**
 *
 */
void
cgui_lock(void);

/**
 *
 */
void
cgui_reconfig(void);

/**
 *
 */
void
cgui_repair(void);

/**
 *
 */
void
cgui_run(void);

/**
 *
 */
void
cgui_unlock(void);

/************************************************************************************************************/
/* PURE METHODS *********************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
enum cerr
cgui_error(void)
CGUI_PURE;

/**
 *
 */
bool
cgui_is_init(void)
CGUI_PURE;

/**
 *
 */
bool
cgui_is_running(void)
CGUI_PURE;

/**
 *
 */
xcb_connection_t *
cgui_x11_connection(void)
CGUI_PURE;

/**
 *
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


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

#include "cgui-attributes.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
typedef struct cgui_window cgui_window;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
enum cgui_window_state
{
	CGUI_WINDOW_INITIAL      = 0,
	CGUI_WINDOW_ACTIVE       = 1,
	CGUI_WINDOW_MAPPED       = 1 << 1,
	CGUI_WINDOW_OBSCURED     = 1 << 2,
	CGUI_WINDOW_FOCUSED      = 1 << 3,
	CGUI_WINDOW_DISABLED     = 1 << 4,
	CGUI_WINDOW_LOCKED_GRID  = 1 << 5,
	CGUI_WINDOW_LOCKED_FOCUS = 1 << 6,
};

/************************************************************************************************************/
/* GLOBALS **************************************************************************************************/
/************************************************************************************************************/

/**
 * A macro that gives uninitialized window a non-NULL value that is safe to use with the window's realted
 * functions. However, any function called with a handle set to this value will return early and without any
 * side effects.
 */
#define CGUI_WINDOW_PLACEHOLDER &cgui_window_placeholder_instance

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Global window instance with the error state set to CGUI_WINDOW_INVALID. This instance is only made
 * available to allow the static initialization of window pointers with the macro CGUI_WINDOW_PLACEHOLDER.
 */
extern cgui_window cgui_window_placeholder_instance;

/************************************************************************************************************/
/* CONSTRUCTORS / DESTRUCTORS *******************************************************************************/
/************************************************************************************************************/

/**
 *
 */
cgui_window *
cgui_window_create(void)
CGUI_NONNULL_RETURN;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
void
cgui_window_destroy(cgui_window *window)
CGUI_NONNULL(1);

/************************************************************************************************************/
/* PROCEDURES ***********************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
void
cgui_window_repair(cgui_window *window)
CGUI_NONNULL(1);

/************************************************************************************************************/
/* FUNCTIONS ************************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
enum cerr
cgui_window_error(const cgui_window *window)
CGUI_NONNULL(1)
CGUI_PURE;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
enum cgui_window_state
cgui_window_state(const cgui_window *window)
CGUI_NONNULL(1)
CGUI_PURE;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

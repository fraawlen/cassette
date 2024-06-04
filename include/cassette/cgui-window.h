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

#ifndef CGUI_WINDOW_H
#define CGUI_WINDOW_H

#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

typedef struct window_t cgui_window_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cgui_window_state_t
{
	CGUI_WINDOW_STATE_INITIAL      = 0,
	CGUI_WINDOW_STATE_ACTIVE       = 1 << 0, /* CGU  */
	CGUI_WINDOW_STATE_MAPPED       = 1 << 1, /* X11  */
	CGUI_WINDOW_STATE_OBSCURED     = 1 << 2, /* X11  */
	CGUI_WINDOW_STATE_FOCUSED      = 1 << 3, /* X11  */
	CGUI_WINDOW_STATE_DISABLED     = 1 << 4, /* CGUI */
	CGUI_WINDOW_STATE_LOCKED_GRID  = 1 << 5, /* CGUI */
	CGUI_WINDOW_STATE_LOCKED_FOCUS = 1 << 6, /* CGUI */
};

typedef enum cgui_window_state_t cgui_window_state_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgui_window_t *cgui_window_create(void);

cgui_window_t *cgui_window_get_placeholder(void);

void cgui_window_destroy(cgui_window_t **window);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool cgui_window_has_failed(const cgui_window_t *window);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* CGUI_WINDOW_H */

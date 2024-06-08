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

#ifndef CGUI_H
#define CGUI_H

#include <stdbool.h>
#include <stdlib.h>

#include <xcb/xcb.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define CGUI_VERSION "0.2.0"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void cgui_init(int argc, char **argv);

void cgui_reset(void);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void cgui_setup_app_class(const char *class_name);

void cgui_setup_app_name(const char *name);

void cgui_setup_x11_connection(xcb_connection_t *connection);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void cgui_allow_user_exit(void);

void cgui_block_user_exit(void);

void cgui_exit(void);

void cgui_lock(void);

void cgui_reconfig(void);

void cgui_run(void);

void cgui_set_callback_x11_events(void (*fn)(xcb_generic_event_t *event));

void cgui_unlock(void);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

xcb_connection_t *cgui_get_x11_connection(void);

xcb_window_t cgui_get_x11_leader_window(void);

bool cgui_has_failed(void);

bool cgui_is_init(void);

bool cgui_is_running(void);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/* init dependent headers */

#include "cgui-cell.h"
#include "cgui-clipboard.h"
#include "cgui-grid.h"
#include "cgui-window.h"

/* init independent headers */

#include "cgui-config.h"
#include "cgui-input-tracker.h"
#include "cgui-input-swap.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* CGUI_H */

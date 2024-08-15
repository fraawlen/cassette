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

#include <cassette/cgui.h>
#include <cassette/cobj.h>
#include <stdbool.h>
#include <xcb/xcb.h>

/************************************************************************************************************/
/* INIT / RESET *********************************************************************************************/
/************************************************************************************************************/

void
x11_init(int argc, char **argv, const char *class_name, const char *class_class, xcb_connection_t *connection)
CGUI_NONNULL(3, 4);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_reset(bool kill_connection);

/************************************************************************************************************/
/* IMPURE METHODS *******************************************************************************************/
/************************************************************************************************************/

void
x11_update(void);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_activate(xcb_window_t id);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
x11_window_create(xcb_window_t *id, int16_t x, int16_t y, uint16_t width, uint16_t height)
CGUI_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_deactivate(xcb_window_t id);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_destroy(xcb_window_t id);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_present(xcb_window_t id, uint32_t serial);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_rename(xcb_window_t id, const char *name);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_set_accel(xcb_window_t id, int accel, const char *name);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_set_transient(xcb_window_t id, xcb_window_t id_under);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_set_urgency(xcb_window_t id, bool set_on);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_update_size_hints(xcb_window_t id, uint16_t min_width, uint16_t min_height, uint16_t max_width, uint16_t max_height);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_window_update_state_hints(xcb_window_t id, struct cgui_window_state_flags state);

/************************************************************************************************************/
/* PURE METHODS *********************************************************************************************/
/************************************************************************************************************/

xcb_connection_t *
x11_connection(void);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

xcb_window_t
x11_leader_window(void);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

xcb_visualtype_t *
x11_visual(void);

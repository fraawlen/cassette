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

#ifndef X11_H
#define X11_H

#include <stdbool.h>
#include <stdint.h>

#include <xcb/xcb.h>
#include <xcb/xcb_keysyms.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

bool x11_init(int argc, char **argv, const char *class_name, const char *class_class, xcb_connection_t *connection);

void x11_reset(bool kill_connection);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool x11_send_signal(uint32_t serial);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

xcb_connection_t *x11_get_connection(void);

xcb_key_symbols_t *x11_get_keysyms(void);

xcb_window_t x11_get_leader_window(void);

xcb_generic_event_t x11_get_next_event(void);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#endif /* X11_H */

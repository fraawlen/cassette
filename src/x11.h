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

#include <stdint.h>

#include <xcb/xcb.h>
#include <xcb/xcb_keysyms.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

xcb_colormap_t x11_create_colormap(xcb_connection_t *x_con, xcb_screen_t *x_scr, xcb_visualtype_t *x_vis);

xcb_connection_t *x11_create_connection(void);

xcb_key_symbols_t *x11_create_keysym_table(xcb_connection_t *x_con);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void  x11_destroy_colormap(xcb_connection_t *x_con, xcb_colormap_t *x_clm);

void x11_destroy_connection(xcb_connection_t **x_con);

void x11_destroy_keysym_table(xcb_key_symbols_t **x_ksm);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

xcb_atom_t x11_get_atom(xcb_connection_t *x_con, const char *name);

xcb_depth_t *x11_get_depth(xcb_screen_t *x_scr);

uint8_t x11_get_extension_opcode(xcb_connection_t *x_con, const char *name);

xcb_screen_t *x11_get_screen(xcb_connection_t *x_con);

xcb_visualtype_t *x11_get_visual(xcb_depth_t *x_dph);


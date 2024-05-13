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

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include <xcb/xcb.h>
#include <xcb/xcb_keysyms.h>

#include "x11.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static bool _test_cookie(xcb_connection_t *x_con, xcb_void_cookie_t xc);

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

xcb_colormap_t
x11_create_colormap(xcb_connection_t *x_con, xcb_screen_t *x_scr, xcb_visualtype_t *x_vis)
{
	xcb_colormap_t x_clm;
	xcb_void_cookie_t xc;

	if (!x_con)
	{
		return 0;
	}

	x_clm = xcb_generate_id(x_con);
	xc = xcb_create_colormap_checked(
		x_con,
		XCB_COLORMAP_ALLOC_NONE,
		x_clm,
		x_scr->root,
		x_vis->visual_id);

	_test_cookie(x_con, xc);

	return x_clm;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

xcb_connection_t *
x11_create_connection(void)
{
	return xcb_connect(NULL, NULL);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

xcb_key_symbols_t *
x11_create_keysym_table(xcb_connection_t *x_con)
{
	if (!x_con)
	{
		return NULL;
	}

	return xcb_key_symbols_alloc(x_con);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void 
x11_destroy_colormap(xcb_connection_t *x_con, xcb_colormap_t *x_clm)
{
	if (!x_con || x_clm == 0)
	{
		return;
	}

	xcb_free_colormap(x_con, *x_clm);

	*x_clm = 0;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_destroy_connection(xcb_connection_t **x_con)
{
	if (!x_con)
	{
		return;
	}

	xcb_disconnect(*x_con);

	*x_con = NULL;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_destroy_keysym_table(xcb_key_symbols_t **x_ksm)
{
	if (!x_ksm)
	{
		return;
	}

	xcb_key_symbols_free(*x_ksm);

	*x_ksm = NULL;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

xcb_atom_t
x11_get_atom(xcb_connection_t *x_con, const char *name)
{
	xcb_intern_atom_cookie_t xc;
	xcb_intern_atom_reply_t *xr;
	xcb_atom_t xa;

	if (!x_con || !name)
	{
		return 0;
	}

	xc = xcb_intern_atom(x_con, 0, strlen(name), name);
	xr = xcb_intern_atom_reply(x_con, xc, NULL);
	if (!xr)
	{
		return 0;
	}

	xa = xr->atom;
	free(xr);

	return xa;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

xcb_depth_t *
x11_get_depth(xcb_screen_t *x_scr)
{
	xcb_depth_iterator_t x_dph_it;

	if (!x_scr)
	{
		return NULL;
	}

	x_dph_it = xcb_screen_allowed_depths_iterator(x_scr);
	for (; x_dph_it.rem; xcb_depth_next(&x_dph_it))
	{
		if (x_dph_it.data->depth == 32)
		{
			return x_dph_it.data;
		}
	}

	return NULL;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

uint8_t
x11_get_extension_opcode(xcb_connection_t *x_con, const char *name)
{
	xcb_query_extension_cookie_t xc;
	xcb_query_extension_reply_t *xr;

	uint8_t opcode;

	if (!x_con || !name)
	{
		return 0;
	}

	xc = xcb_query_extension(x_con, strlen(name), name);
	xr = xcb_query_extension_reply(x_con, xc, NULL);
	if (!xr)
	{
		return 0;
	}

	opcode = xr->major_opcode;
	free(xr);

	return opcode;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

xcb_screen_t *
x11_get_screen(xcb_connection_t *x_con)
{
	if (!x_con)
	{
		return NULL;
	}

	return xcb_setup_roots_iterator(xcb_get_setup(x_con)).data;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

xcb_visualtype_t *
x11_get_visual(xcb_depth_t *x_dph)
{
	xcb_visualtype_iterator_t x_vis_it;

	if (!x_dph)
	{
		return NULL;
	}

	x_vis_it = xcb_depth_visuals_iterator(x_dph);
	for (; x_vis_it.rem; xcb_visualtype_next(&x_vis_it))
	{
		if (x_vis_it.data->_class == XCB_VISUAL_CLASS_TRUE_COLOR)
		{
			return x_vis_it.data;
		}
	}

	return NULL;
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static bool
_test_cookie(xcb_connection_t *x_con, xcb_void_cookie_t xc)
{
	xcb_generic_error_t *x_err;

	if ((x_err = xcb_request_check(x_con, xc)))
	{
		free(x_err);
		return true;
	}

	return false;
}


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

#ifndef CGUI_STYLE_H
#define CGUI_STYLE_H

#include <stdbool.h>

#include <cassette/cobj.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

struct cgui_style_cell_t
{
	/* geometry */

	unsigned int thickness_border;
	unsigned int thickness_outline;
	unsigned int margin;

	/* colors */

	cobj_color_t color_background;
	cobj_color_t color_border;
	cobj_color_t color_outline;
};

typedef struct cgui_style_cell_t cgui_style_cell_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct cgui_style_window_t
{
	/* geometry */

	unsigned int thickness_border;
	unsigned int padding_outer;
	unsigned int padding_inner;
	unsigned int padding_cell;

	/* colors */

	cobj_color_t color_background;
	cobj_color_t color_background_disabled;
	cobj_color_t color_background_focused;
	cobj_color_t color_background_locked;

	cobj_color_t color_border;
	cobj_color_t color_border_disabled;
	cobj_color_t color_border_focused;
	cobj_color_t color_border_locked;

	/* switches */

	bool enable_disabled;
	bool enable_focused;
	bool enable_locked;
};

typedef struct cgui_style_window_t cgui_style_window_t;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* CGUI_STYLE_H */

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

#include <cairo/cairo.h>
#include <cassette/cobj.h>

#include "cgui-attributes.h"
#include "cgui-box.h"
#include "cgui-types.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
struct cgui_text_context
{
	double x;
	double y;
	enum cgui_align align;
	enum cgui_rotation rotation;
	cairo_t *drawable;
};

/**
 *
 */
struct cgui_text_style
{
	struct ccolor color;
	struct ccolor color_background;
	bool draw_background;
	bool bold;
};

/**
 *
 */
struct cgui_text_segment
{
	struct cgui_text_style style;
	size_t length;
};

/************************************************************************************************************/
/* IMPURE METHODS *******************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
void
cgui_text_draw(struct cgui_text_context context, struct cgui_text_style style, const cstr *str)
CGUI_NONNULL(3);

/**
 *
 */
void
cgui_text_draw_segments(struct cgui_text_context context, struct cgui_text_segment *segments, size_t segments_number, const cstr *str)
CGUI_NONNULL(2, 4);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

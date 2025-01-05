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
struct cgui_text
{
	struct ccolor color;
	struct ccolor color_background;
	bool draw_background;
	bool bold;
};

/************************************************************************************************************/
/* IMPURE METHODS *******************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
void
cgui_text_align(enum cgui_align alignment);

/**
 *
 */
void
cgui_text_codepoint_range(size_t codepoint_min, size_t codepoint_max);

/**
 *
 */
void
cgui_text_col_range(size_t col_min, size_t col_max);

/**
 *
 */
void
cgui_text_draw(cairo_t *drawable, const cstr *str)
CGUI_NONNULL(1, 2);

/**
 *
 */
void
cgui_text_reset(void);

/**
 *
 */
void
cgui_text_rotation(enum cgui_rotation rotation);

/**
 *
 */
void
cgui_text_row_range(size_t row_min, size_t row_max);

/**
 *
 */
void
cgui_text_style(struct cgui_text style);

/**
 *
 */
void
cgui_text_x(double x);

/**
 *
 */
void
cgui_text_y(double y);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

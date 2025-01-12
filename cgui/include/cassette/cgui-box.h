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

#include "cgui-attributes.h"
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
struct cgui_box
{
	enum cgui_corner corner[4];

	double size_corner[4];
	double size_outline;
	double size_border;
	double padding;
	double margin;
	double shadow_x_offset;
	double shadow_y_offset;
	
	struct ccolor color_outline;
	struct ccolor color_border;
	struct ccolor color_background;
	struct ccolor color_foreground;
	struct ccolor color_shadow;

	bool draw;
	bool draw_foreground;
	bool draw_shadow;
	bool shape_outline;
	bool shape_border;
	bool hit_outline;
};

/************************************************************************************************************/
/* IMPURE METHODS *******************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
void
cgui_box_clip(cairo_t *drawable, double pad)
CGUI_NONNULL(1);

/**
 *
 */
void
cgui_box_draw(cairo_t *drawable)
CGUI_NONNULL(1);

/**
 *
 */
void
cgui_box_move(double x, double y);

/**
 *
 */
void
cgui_box_pad_corner(struct cgui_box *box, struct cgui_box box_parent, double pad, int id)
CGUI_NONNULL(1);

/**
 *
 */
void
cgui_box_reset(void);

/**
 *
 */
void
cgui_box_resize(double width, double height);

/**
 *
 */
void
cgui_box_style(struct cgui_box box);

/************************************************************************************************************/
/* PURE METHODS *********************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
bool
cgui_box_inside(cairo_t *drawable, double x, double y)
CGUI_NONNULL(1);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

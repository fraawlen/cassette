/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Graphics (CGUI) library.
 *
 * This library is free software; you can redistribute it and/or modify it either under the terms of the GNU
 * Lesser General Public License as published by the Free Software Foundation; either version 3.0 of the
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
	enum cgui_corner cn_type[4];

	double pad;
	double gap;
	double margin;
	double cn_size[4];
	double ol_size;
	double bd_size;
	double sd_offset_x;
	double sd_offset_y;
	
	struct ccolor ol_cl;
	struct ccolor bd_cl;
	struct ccolor bg_cl;
	struct ccolor sd_cl;

	bool draw;
	bool sd_draw;
	bool cn_smart;
	bool ol_shape;
	bool bd_shape;
	bool ol_hit;
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
cgui_box_move_shadow(double x_light, double y_light, double max_light_distance, double max_offset);

/**
 *
 */
void
cgui_box_pad_all_corners(struct cgui_box *box, struct cgui_box box_parent, double pad)
CGUI_NONNULL(1);

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
double
cgui_box_content_offset(struct cgui_box box)
CGUI_CONST;

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

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
#include <stdint.h>

#include "cgui-attributes.h"
#include "cgui-zone.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
enum cgui_box_corner
{
	CGUI_BOX_STRAIGHT,
	CGUI_BOX_CHAMFER,
	CGUI_BOX_RADII,
};

/**
 *
 */
struct cgui_box
{
	enum cgui_box_corner corner[4];

	double size_corner[4];
	double size_outline;
	double size_border;
	double padding;
	double margin;
	
	struct ccolor color_outline;
	struct ccolor color_border;
	struct ccolor color_background;
	struct ccolor color_foreground;

	bool draw;
	bool draw_foreground;
	bool shape_outline;
	bool shape_border;
};

/************************************************************************************************************/
/* IMPURE METHODS *******************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
void
cgui_box_clip(struct cgui_box box, struct cgui_zone zone, double pad);

/**
 *
 */
void
cgui_box_draw(struct cgui_box box, struct cgui_zone zone);

/**
 *
 */
void
cgui_box_pad_corner(struct cgui_box *box, struct cgui_box box_parent, double pad, int id)
CGUI_NONNULL(1);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

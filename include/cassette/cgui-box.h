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
#include "cgui-cell.h"
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
	enum cgui_box_corner corner_type[4];
	uint16_t corner_size[4];
	uint16_t thickness;
	uint16_t padding;
	struct ccolor color_border;
	struct ccolor color_background;
	bool outer_shaping;
};

/************************************************************************************************************/
/* IMPURE METHODS *******************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
struct cgui_box
cgui_box_adjust(struct cgui_box box, const struct cgui_cell_context *context)
CGUI_NONNULL(2);

/**
 *
 */
void
cgui_box_draw(struct cgui_box box, struct cgui_zone zone);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

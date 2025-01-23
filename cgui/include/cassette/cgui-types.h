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

/************************************************************************************************************/
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
struct cgui_mods
{
	bool capslock;
	bool shift;
	bool ctrl;
	bool mod_1;
	bool mod_2;
	bool mod_3;
	bool mod_4;
	bool mod_5;
};

/**
 *
 */
enum cgui_align
{
	CGUI_ALIGN_TOP_LEFT,
	CGUI_ALIGN_TOP,
	CGUI_ALIGN_TOP_RIGHT,
	CGUI_ALIGN_LEFT,
	CGUI_ALIGN_CENTER,
	CGUI_ALIGN_RIGHT,
	CGUI_ALIGN_BOTTOM_LEFT,
	CGUI_ALIGN_BOTTOM,
	CGUI_ALIGN_BOTTOM_RIGHT,
};

/**
 *
 */
enum cgui_rotation
{
	CGUI_ROTATION_NORMAL,
	CGUI_ROTATION_INVERTED,
	CGUI_ROTATION_LEFT,
	CGUI_ROTATION_RIGHT,
};

/**
 *
 */
enum cgui_corner
{
	CGUI_CORNER_STRAIGHT,
	CGUI_CORNER_CHAMFER,
	CGUI_CORNER_RADII,
};

/**
 *
 */
enum cgui_focus
{
	CGUI_FOCUS_NONE,
	CGUI_FOCUS_NEXT,
	CGUI_FOCUS_PREV,
	CGUI_FOCUS_FIRST,
	CGUI_FOCUS_LAST,
};

/************************************************************************************************************/
/* PURE METHODS *********************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
double
cgui_align_offset_x(enum cgui_align alignment, double width)
CGUI_CONST;

/**
 *
 */
double
cgui_align_offset_y(enum cgui_align alignment, double heigh)
CGUI_CONST;

/**
 *
 */
enum cgui_align
cgui_align_rotation(enum cgui_align alignment, enum cgui_rotation)
CGUI_CONST;

/**
 *
 */
double
cgui_rotation_angle(enum cgui_rotation rotation)
CGUI_CONST;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

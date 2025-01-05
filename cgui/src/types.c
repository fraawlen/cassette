/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Objects (COBJ) library.
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

#include <cassette/cgui.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define PI 3.14159265358979323846

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static const enum cgui_align new_align[9][4] =
{
	/*
	  NORMAL                   INVERTED                 LEFT ROT                 RIGHT ROT  
	 */
	{ CGUI_ALIGN_TOP_LEFT,     CGUI_ALIGN_BOTTOM_RIGHT, CGUI_ALIGN_BOTTOM_LEFT,  CGUI_ALIGN_TOP_RIGHT    }, /* TOP LEFT     */
	{ CGUI_ALIGN_TOP,          CGUI_ALIGN_BOTTOM,       CGUI_ALIGN_LEFT,         CGUI_ALIGN_RIGHT        }, /* TOP          */
	{ CGUI_ALIGN_TOP_RIGHT,    CGUI_ALIGN_BOTTOM_LEFT,  CGUI_ALIGN_TOP_LEFT,     CGUI_ALIGN_BOTTOM_RIGHT }, /* TOP_RIGHT    */
	{ CGUI_ALIGN_LEFT,         CGUI_ALIGN_RIGHT,        CGUI_ALIGN_BOTTOM,       CGUI_ALIGN_TOP          }, /* LEFT         */
	{ CGUI_ALIGN_CENTER,       CGUI_ALIGN_CENTER,       CGUI_ALIGN_CENTER,       CGUI_ALIGN_CENTER       }, /* CENTER       */
	{ CGUI_ALIGN_RIGHT,        CGUI_ALIGN_LEFT,         CGUI_ALIGN_TOP,          CGUI_ALIGN_BOTTOM       }, /* RIGHT        */
	{ CGUI_ALIGN_BOTTOM_LEFT,  CGUI_ALIGN_TOP_RIGHT,    CGUI_ALIGN_BOTTOM_RIGHT, CGUI_ALIGN_TOP_LEFT     }, /* BOTTOM LEFT  */
	{ CGUI_ALIGN_BOTTOM,       CGUI_ALIGN_TOP,          CGUI_ALIGN_RIGHT,        CGUI_ALIGN_LEFT         }, /* BOTTOM       */
	{ CGUI_ALIGN_BOTTOM_RIGHT, CGUI_ALIGN_TOP_LEFT,     CGUI_ALIGN_TOP_RIGHT,    CGUI_ALIGN_BOTTOM_LEFT  }, /* BOTTOM RIGHT */
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

double
cgui_align_offset_x(enum cgui_align alignment, double width)
{
	switch (alignment)
	{
		default:
		case CGUI_ALIGN_TOP_LEFT:
		case CGUI_ALIGN_LEFT:
		case CGUI_ALIGN_BOTTOM_LEFT:
			return 0.0;

		case CGUI_ALIGN_TOP:
		case CGUI_ALIGN_CENTER:
		case CGUI_ALIGN_BOTTOM:
			return width / 2;

		case CGUI_ALIGN_TOP_RIGHT:
		case CGUI_ALIGN_RIGHT:
		case CGUI_ALIGN_BOTTOM_RIGHT:
			return width;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

double
cgui_align_offset_y(enum cgui_align alignment, double height)
{
	switch (alignment)
	{
		default:
		case CGUI_ALIGN_TOP_LEFT:
		case CGUI_ALIGN_TOP:
		case CGUI_ALIGN_TOP_RIGHT:
			return 0.0;

		case CGUI_ALIGN_LEFT:
		case CGUI_ALIGN_CENTER:
		case CGUI_ALIGN_RIGHT:
			return height / 2;

		case CGUI_ALIGN_BOTTOM_LEFT:
		case CGUI_ALIGN_BOTTOM:
		case CGUI_ALIGN_BOTTOM_RIGHT:
			return height;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cgui_align
cgui_align_rotation(enum cgui_align alignment, enum cgui_rotation rotation)
{
	return new_align[alignment][rotation];
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

double
cgui_rotation_angle(enum cgui_rotation rotation)
{
	switch (rotation)
	{
		default:
		case CGUI_ROTATION_NORMAL:
			return 0.0;

		case CGUI_ROTATION_INVERTED:
			return PI;

		case CGUI_ROTATION_LEFT:
			return PI / 2;

		case CGUI_ROTATION_RIGHT:
			return -PI / 2;
	}
}

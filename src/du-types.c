/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Derelict Utilities (DU) library.
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

#include "du.h"

/************************************************************************************************************/
/* PUBLIC - RATIO *******************************************************************************************/
/************************************************************************************************************/

du_ratio_t
du_ratio_bind(du_ratio_t *ratio)
{
	*ratio = *ratio > 1.0 ? 1.0 : (*ratio < 0.0 ? 0.0 : *ratio);

	return *ratio;
}

/************************************************************************************************************/
/* PUBLIC - ORIGIN ******************************************************************************************/
/************************************************************************************************************/

du_position_t
du_origin_get_x_offset(du_origin_t og, du_length_t w)
{
	switch (og) {
		
		case DU_ORIGIN_RIGHT:
		case DU_ORIGIN_TOP_RIGHT:
		case DU_ORIGIN_BOTTOM_RIGHT:
			return w;

		case DU_ORIGIN_CENTER:
		case DU_ORIGIN_TOP:
		case DU_ORIGIN_BOTTOM:
			return w / 2;

		case DU_ORIGIN_LEFT:
		case DU_ORIGIN_TOP_LEFT:
		case DU_ORIGIN_BOTTOM_LEFT:
		default:
			return 0;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

du_position_t
du_origin_get_y_offset(du_origin_t og, du_length_t h)
{
	switch (og) {
		
		case DU_ORIGIN_BOTTOM:
		case DU_ORIGIN_BOTTOM_LEFT:
		case DU_ORIGIN_BOTTOM_RIGHT:
			return h;

		case DU_ORIGIN_CENTER:
		case DU_ORIGIN_LEFT:
		case DU_ORIGIN_RIGHT:
			return h / 2;

		case DU_ORIGIN_TOP:
		case DU_ORIGIN_TOP_LEFT:
		case DU_ORIGIN_TOP_RIGHT:
		default:
			return 0;
	}
}

/************************************************************************************************************/
/* PUBLIC - ROTATION ****************************************************************************************/
/************************************************************************************************************/

bool
du_rotation_is_horizontal(du_rotation_t rot)
{
	return rot == DU_ROTATION_NORMAL || rot == DU_ROTATION_INVERTED;
}

/************************************************************************************************************/
/* PUBLIC - COORDINATES *************************************************************************************/
/************************************************************************************************************/

bool
du_coordinates_are_in_rect(du_coordinates_t coord, du_rect_t rect)
{
	if (rect.w < 0) {
		rect.w *= -1;
		rect.x -= rect.w;
	}

	if (rect.h < 0) {
		rect.h *= -1;
		rect.y -= rect.h;
	}

	return coord.x >= rect.x && coord.x < rect.x + rect.w &&
	       coord.y >= rect.y && coord.y < rect.y + rect.h;
}

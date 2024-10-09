/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Derelict Graphics (DG) GUI library.
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

#include <assert.h>
#include <stdint.h>

#include "origin.h"
#include "zone.h"

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

int16_t
dg_base_origin_get_zone_x_position(dg_base_origin_t og, const dg_base_zone_t *z)
{
	assert(z);

	double ratio = 0.0;

	switch (og) {
		
		case DG_BASE_ORIGIN_LEFT:
		case DG_BASE_ORIGIN_TOP_LEFT:
		case DG_BASE_ORIGIN_BOTTOM_LEFT:
			ratio = 0.0;
			break;

		case DG_BASE_ORIGIN_CENTER:
		case DG_BASE_ORIGIN_TOP:
		case DG_BASE_ORIGIN_BOTTOM:
			ratio = 0.5;
			break;

		case DG_BASE_ORIGIN_RIGHT:
		case DG_BASE_ORIGIN_TOP_RIGHT:
		case DG_BASE_ORIGIN_BOTTOM_RIGHT:
			ratio = 1.0;
			break;
	}

	return (double)z->pw * ratio;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

int16_t
dg_base_origin_get_zone_y_position(dg_base_origin_t og, const dg_base_zone_t *z)
{
	assert(z);

	double ratio = 0.0;

	switch (og) {
		
		case DG_BASE_ORIGIN_TOP:
		case DG_BASE_ORIGIN_TOP_LEFT:
		case DG_BASE_ORIGIN_TOP_RIGHT:
			ratio = 0.0;
			break;

		case DG_BASE_ORIGIN_CENTER:
		case DG_BASE_ORIGIN_LEFT:
		case DG_BASE_ORIGIN_RIGHT:
			ratio = 0.5;
			break;

		case DG_BASE_ORIGIN_BOTTOM:
		case DG_BASE_ORIGIN_BOTTOM_LEFT:
		case DG_BASE_ORIGIN_BOTTOM_RIGHT:
			ratio = 1.0;
			break;
	}

	return (double)z->ph * ratio;
}

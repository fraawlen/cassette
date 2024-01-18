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

#ifndef DG_BASE_ORIGIN_H
#define DG_BASE_ORIGIN_H

#include "zone.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/**
 * List of origin points.
 */
typedef enum {
	DG_BASE_ORIGIN_CENTER,
	DG_BASE_ORIGIN_LEFT,
	DG_BASE_ORIGIN_RIGHT,
	DG_BASE_ORIGIN_TOP,
	DG_BASE_ORIGIN_TOP_LEFT,
	DG_BASE_ORIGIN_TOP_RIGHT,
	DG_BASE_ORIGIN_BOTTOM,
	DG_BASE_ORIGIN_BOTTOM_LEFT,
	DG_BASE_ORIGIN_BOTTOM_RIGHT,
} dg_base_origin_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Gets the relative x horizontal coordinate of a point at a given origin on a given zone.
 *
 * @param og : point origin
 * @param z  : zone to get coordinate from
 *
 * @return : self-explanatory
 */
int16_t dg_base_origin_get_zone_x_position(dg_base_origin_t og, const dg_base_zone_t *z);

/**
 * Gets the relative y vertical coordinate of a point at a given origin on a given zone.
 *
 * @param og : point origin
 * @param z  : zone to get coordinate from
 *
 * @return : self-explanatory
 */
int16_t dg_base_origin_get_zone_y_position(dg_base_origin_t og, const dg_base_zone_t *z);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DG_BASE_ORIGIN_H */

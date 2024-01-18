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

#ifndef DG_BASE_ROTATION_H
#define DG_BASE_ROTATION_H

#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/**
 * 90deg rotations positions.
 */
typedef enum {
	DG_BASE_ROTATION_NORMAL,
	DG_BASE_ROTATION_INVERTED,
	DG_BASE_ROTATION_LEFT,
	DG_BASE_ROTATION_RIGHT,
} dg_base_rotation_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Tests wether text written with the given rotation will be horizontal or vertical.
 *
 * @param rot : rotation to test
 *
 * @return : true if horizontal, false otherwhise
 */
bool dg_base_rotation_is_horizontal(dg_base_rotation_t rot);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DG_BASE_ROTATION_H */

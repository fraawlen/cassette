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

#ifndef DU_TYPES_H
#define DU_TYPES_H

#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/**
 * Integer type for lengths. In most situations, its value is expected to be positive unless otherwhise
 * stated.
 */
typedef int16_t du_length_t;

/**
 * Integer type for positions.
 */
typedef int16_t du_position_t;

/**
 * Double type for angles.
 */
typedef double du_angle_t;

/**
 * Double type for ratios. Significant values are generally expected to be >= 0.0 and <= 1.0.
 */
typedef double du_ratio_t;

/**
 * List of origin points.
 */
typedef enum {
	DU_ORIGIN_CENTER,
	DU_ORIGIN_LEFT,
	DU_ORIGIN_RIGHT,
	DU_ORIGIN_TOP,
	DU_ORIGIN_TOP_LEFT,
	DU_ORIGIN_TOP_RIGHT,
	DU_ORIGIN_BOTTOM,
	DU_ORIGIN_BOTTOM_LEFT,
	DU_ORIGIN_BOTTOM_RIGHT,
} du_origin_t;

/**
 * List of main rotation sides. Left and right rotations correspond respectively to a clockwise and
 * counter-clockwise 90deg turn to match X's left and right monitor rotation directions.
 */
typedef enum {
	DU_ROTATION_NORMAL,
	DU_ROTATION_RIGHT,
	DU_ROTATION_LEFT,
	DU_ROTATION_INVERTED,
} du_rotation_t;

/**
 * Struct to hold a 2D position.
 *
 * @param x : horizonal position
 * @param y : vertical position
 */
typedef struct {
	du_position_t x;
	du_position_t y;
} du_coordinates_t;

/**
 * Struct to hold the size of something defined within a 2D rectangular bounding box.
 *
 * @param w : width
 * @param h : height
 */
typedef struct {
	du_length_t w;
	du_length_t h;
} du_size_t;

/**
 * 2D Rectangular area.
 *
 * @param x : horizonal position
 * @param y : vertical position
 * @param w : width
 * @param h : height
 */
typedef struct {
	du_position_t x;
	du_position_t y;
	du_length_t w;
	du_length_t h;
} du_rect_t;

/************************************************************************************************************/
/* RATIO ****************************************************************************************************/
/************************************************************************************************************/

/**
 * Corrects the value of a given ratio to be within 0.0 and 1.0.
 *
 * @param ratio : ratio to bind, the pointed value will be modified.
 *
 * return : the ratio's new value
 */
du_ratio_t du_ratio_bind(du_ratio_t *ratio);

/************************************************************************************************************/
/* ORIGIN ***************************************************************************************************/
/************************************************************************************************************/

/**
 * Gets the relative x horizontal coordinate of a point at a given origin on a given zone.
 *
 * @param og : point origin
 * @param z  : zone to get coordinate from
 *
 * @return : horizontal offset
 */
du_position_t du_origin_get_x_offset(du_origin_t og, du_length_t w);

/**
 * Gets the relative y vertical coordinate of a point at a given origin on a given zone.
 *
 * @param og : point origin
 * @param z  : zone to get coordinate from
 *
 * @return : vertical offset
 */
du_position_t du_origin_get_y_offset(du_origin_t og, du_length_t h);

/************************************************************************************************************/
/* ROTATION *************************************************************************************************/
/************************************************************************************************************/

/**
 * Tests wether text written with the given rotation will be horizontal or vertical.
 *
 * @param rot : rotation to test
 *
 * @return : true if horizontal, false otherwhise
 */
bool du_rotation_is_horizontal(du_rotation_t rot);

/************************************************************************************************************/
/* COORDINATES **********************************************************************************************/
/************************************************************************************************************/

/**
 * Checks if a given set of coordinates is within a given rectangle.
 * Negative rect width and height are allowed (results in a rect "facing the other side" on the negative
 * axis). A rect width or height of 0 will always return false.
 *
 * @param coord : coordinates to test
 * @param rect  : bounding rectangle area
 *
 * @return : true if they are whithin, false otherwhise
 */
bool du_coordinates_are_in_rect(du_coordinates_t coord, du_rect_t rect);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DU_TYPES_H */

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

#ifndef DG_BASE_DRAW_H
#define DG_BASE_DRAW_H

#include <stdbool.h>
#include <stdint.h>

#include <dg/core/core.h>

#include "config.h"
#include "origin.h"
#include "rotation.h"
#include "string.h"
#include "zone.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define DG_BASE_DRAW_PI 3.1415926536

#define DG_BASE_DRAW_POINTS_LEN(X) (sizeof(X) / sizeof(dg_base_draw_point_t))

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Small struct to bundle 2D coordinates.
 *
 * @param x : x coordinate
 * @param y : y coordinate
 */
typedef struct {
	double x;
	double y;
} dg_base_draw_point_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Draws a cell's background, border in the given zone following the given style.
 * Its expected that the zone is obtained directly from dg_base_zone_get_from_context().
 * Requires a valid zone with a valid cairo context and positive width and height.
 * 
 * @param z     : zone to draw on
 * @param style : style to follow
 */
void dg_base_draw_body(dg_base_zone_t *z, const dg_base_config_style_t *style);

/**
 * Draws a cell's focus in the given drawing context following the given style.
 * Its expected that the zone is obtained directly from dg_base_zone_get_from_context().
 * Requires a valid zone with a valid cairo context and positive width and height.
 *
 * @param z     : zone to draw on
 * @param style : style to follow
 * @param dc    : drawing context used to obtain the focus level and cell state
 */
void dg_base_draw_focus(dg_base_zone_t *z, const dg_base_config_style_t *style,
                        dg_core_cell_drawing_context_t *dc);

/**
 * Draws a separator in the given zone following the given style. The separator is drawn from its top-left
 * corner, and the rotation is done around that point too.
 * Requires a valid zone with a valid cairo context and positive width and height.
 *
 * @param z      : zone to draw on
 * @param style  : style to follow
 * @param px     : starting x point of the separator, relative to the zone, in pixels
 * @param py     : starting x point of the separator, relative to the zone, in pixels
 * @param length : length, in pixels, of the separator
 * @param rot    : rotation to apply to the separator
 */
void dg_base_draw_separator(dg_base_zone_t *z, const dg_base_config_style_t *style, int16_t px, int16_t py,
                            int16_t length, dg_base_rotation_t rot);

/**
 * Draws a given string in a DG sort of standard way in the given zone following the given style. If the
 * given zone does not have enought width to fully draw the string, then limiters will be added on the sides
 * and the string will get automatically clipped to the zone. But because there is an internal clipping (and
 * unclipping) cairo operation, it may remove any existing clipping zone.
 * Requires a valid zone with a valid cairo context and positive width and height.
 *
 * @param z      : zone to draw on
 * @param style  : style to follow
 * @param str    : 2D DG string to draw
 * @param og     : origin of the string, relative to the zone bounding box
 */
void dg_base_draw_label(dg_base_zone_t *z, const dg_base_config_style_t *style, const dg_base_string_t *str,
                        dg_base_origin_t og);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Unlike dg_base_draw_label(), this function is a more raw way to draw strings. No width checking and
 * clipping is done, nor any limiters are drawn. It just simply puts the given string on screen, regardless
 * of context or cell style.
 * Requires a valid zone with a valid cairo context and positive width and height.
 *
 * @param z    : zone to draw on
 * @param cl   : color to draw glyphs with
 * @param str  : 2D DG string to draw
 * @param px   : position of the origin, relative to the zone
 * @param py   : position of the origin, relative to the zone
 * @param bold : draw glyphs with bold font variant
 * @param og   : origin of the string, relative to the string's bounding box
 * @param rot  : rotation of string, with its rotation point being the og parameter
 */
void dg_base_draw_string(dg_base_zone_t *z, dg_core_color_t cl, const dg_base_string_t *str, int16_t px,
                         int16_t py, bool bold, dg_base_origin_t og, dg_base_rotation_t rot);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Draw an arc primitive in a normalised [0,1] coordinate system mapped to the zone.
 * If the thickness <= 0, then the shape is drawn filled, otherwhise an outline of the shape is drawn.
 * The drawn shape is antialiased.
 * Requires a valid zone with a valid cairo context and positive width and height.
 *
 * @param z         : zone to draw on
 * @param cl        : color to draw shape with
 * @param x         : center coordinate
 * @param y         : center coordinate
 * @param r         : radius
 * @param a1        : first angle, in radians
 * @param a2        : second andle, in radians
 * @param thickness : outline pixel thickness
 */
void dg_base_draw_arc(dg_base_zone_t *z, dg_core_color_t cl, double x, double y, double r, double a1,
                      double a2, int16_t thickness);

/**
 * Draw a circle primitive in a normalised [0,1] coordinate system mapped to the zone.
 * If the thickness <= 0, then the shape is drawn filled, otherwhise an outline of the shape is drawn.
 * The drawn shape is antialiased.
 * Requires a valid zone with a valid cairo context and positive width and height.
 *
 * @param z         : zone to draw on
 * @param cl        : color to draw shape with
 * @param x         : center coordinate
 * @param y         : center coordinate
 * @param r         : radius
 * @param thickness : outline pixel thickness
 */
void dg_base_draw_circle(dg_base_zone_t *z, dg_core_color_t cl, double x, double y, double r,
                         int16_t thickness);

/**
 * Draws a contour around the given zone. If the given thickness is > 0, said contour is draw inside the
 * zone, otherwhise it's drawn outside the zone.
 * Requires a valid zone with a valid cairo context and positive width and height.
 *
 * @param z         : zone to draw on
 * @param cl        : color to draw shape with
 * @param thickness : countour pixel thickness
 */
void dg_base_draw_contour(dg_base_zone_t *z, dg_core_color_t cl, int16_t thickness);

/**
 * Fills a zone with the given color.
 * Requires a valid zone with a valid cairo context and positive width and height.
 *
 * @param z : zone to draw on
 * @param cl : color to fill the zone with
 */
void dg_base_draw_fill(dg_base_zone_t *z, dg_core_color_t cl);

/**
 * Draw a series of connected lines in a normalised [0,1] coordinate system mapped to the zone.
 * Antialiasing will be automatically applied if any of the lines is not horizontal or vertical. For that
 * reason, it is recommended to split the vertical or horizontal line sets from those that are not, or the
 * vertical/horizontal lines will also get antialiased and potentially look bad.
 * Requires a valid zone with a valid cairo context and positive width and height.
 *
 * @param z         : zone to draw on
 * @param cl        : color to draw shape with
 * @param points    : array of point cooridnates
 * @param n         : size of array / amounts of points
 * @param thickness : lines pixel thickness
 */
void dg_base_draw_lines(dg_base_zone_t *z, dg_core_color_t cl, const dg_base_draw_point_t *points, size_t n,
                        int16_t thickness);

/**
 * Draw a rectangle primitive in a normalised [0,1] coordinate system mapped to the zone.
 * If the thickness <= 0, then the shape is drawn filled, otherwhise an outline of the shape is drawn.
 * Requires a valid zone with a valid cairo context and positive width and height.
 *
 * @param z         : zone to draw on
 * @param cl        : color to draw shape with
 * @param x1        : top left corner coordinate
 * @param y1        : top left corner coordinate
 * @param x2        : bottom right corner coordinate
 * @param y2        : bottom right corner coordinate
 * @param thickness : outline pixel thickness
 */
void dg_base_draw_rectangle(dg_base_zone_t *z, dg_core_color_t cl, double x1, double y1, double x2,
                            double y2, int16_t thickness);

/**
 * Draw a segment primitive in a normalised [0,1] coordinate system mapped to the zone.
 * Antialiasing will be automatically applied if the segment is not horizontal or vertical.
 * Requires a valid zone with a valid cairo context and positive width and height.
 *
 * @param z         : zone to draw on
 * @param cl        : color to draw shape with
 * @param x1        : start point coordinate
 * @param y1        : start point coordinate
 * @param x2        : end point coordinate
 * @param y2        : end point coordinate
 * @param thickness : segment pixel thickness
 */
void dg_base_draw_segment(dg_base_zone_t *z, dg_core_color_t cl, double x1, double y1, double x2, double y2,
                          int16_t thickness);

/**
 * Draw a triangle primitive in a normalised [0,1] coordinate system mapped to the zone.
 * If the thickness <= 0, then the shape is drawn filled, otherwhise an outline of the shape is drawn.
 * The drawn shape is antialiased.
 * Requires a valid zone with a valid cairo context and positive width and height.
 *
 * @param z         : zone to draw on
 * @param cl        : color to draw shape with
 * @param x1        : 1st point coordinate
 * @param y1        : 1st point coordinate
 * @param x2        : 2nd point coordinate
 * @param y2        : 2nd point coordinate
 * @param x3        : 3rd point coordinate
 * @param y3        : 3rd point coordinate
 * @param thickness : outline pixel thickness
 */
void dg_base_draw_triangle(dg_base_zone_t *z, dg_core_color_t cl, double x1, double y1, double x2, double y2, 
                      double x3, double y3, int16_t thickness);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DG_BASE_DRAW_H */



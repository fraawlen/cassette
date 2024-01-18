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

#ifndef DG_BASE_ZONE_H
#define DG_BASE_ZONE_H

#include <stdbool.h>
#include <stdint.h>

#include <dg/core/core.h>

#include "config.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/**
 * Drawing area.
 *
 * @param px    : pixel x coordinate
 * @param py    : pixel y coordinate
 * @param pw    : pixel width
 * @param ph    : pixel height
 * @param c_ctx : cairo drawing context
 */
typedef struct {
	int16_t px;
	int16_t py;
	int16_t pw;
	int16_t ph;
	cairo_t *c_ctx;
} dg_base_zone_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Obtains a drawing zone covering the cell's main body, with the style's margin taken into account.
 *
 * @param dc    : drawing context to extract cell geometry from
 * @param style : styling attributes to apply
 *
 * @return : self-explanatory
 */
dg_base_zone_t dg_base_zone_get_body(const dg_core_cell_drawing_context_t *dc,
                                     const dg_base_config_style_t *style);

/**
 * Obtains a drawing zone covering the cell's maximum surface area.
 *
 * @param dc : drawing context to extract cell geometry from
 *
 * @return : self-explanatory
 */
dg_base_zone_t dg_base_zone_get_cell(const dg_core_cell_drawing_context_t *dc);

/**
 * Obtains a drawing zone of the foreground area of a cell.
 *
 * @param dc    : drawing context to extract cell geometry from
 * @param style : styling attributes to apply
 *
 * @return : self-explanatory
 */
dg_base_zone_t dg_base_zone_get_foreground(const dg_core_cell_drawing_context_t *dc,
                                           const dg_base_config_style_t *style);

/**
 * Obtains a drawing zone of the label area of a cell.
 *
 * @param dc             : drawing context to extract cell geometry from
 * @param style          : styling attributes to apply
 * @param substract_icon : set to true to take into account the area taken by an icon, if any
 *
 * @return : self-explanatory
 */
dg_base_zone_t dg_base_zone_get_label(const dg_core_cell_drawing_context_t *dc,
                                      const dg_base_config_style_t *style, bool substract_icon);

/**
 * Obtains a drawing zone of the icon area of a cell.
 *
 * @param dc    : drawing context to extract cell geometry from
 *
 * @param style : styling attributes to apply
 * @return : self-explanatory
 */
dg_base_zone_t dg_base_zone_get_icon(const dg_core_cell_drawing_context_t *dc,
                                      const dg_base_config_style_t *style);

/**
 * Obtains a drawing zone exactly covering the given window.
 *
 * @param w : window to get zone from
 *
 * @return : self-explanatory
 */
dg_base_zone_t dg_base_zone_get_window(dg_core_window_t *w);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Pads the area defined by the zone, negative values pads toward the outside.
 *
 * @param z   : zone to use
 * @param pad : padding length
 */
void dg_base_zone_pad(dg_base_zone_t *z, int16_t pad);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Applies the font and its parameters defined in the Core's config.
 *
 * @param z    : zone to apply font settings to
 * @param bold : wheter or not apply a bold font variant
 *
 * @error DG_CORE_ERRNO_CAIRO : cairo font option creation failure
 */
void dg_base_zone_apply_core_font(dg_base_zone_t *z, bool bold);

/**
 * Clips the drawing context to the area defined by the zone.
 *
 * @param z : zone to use
 */
void dg_base_zone_clip(dg_base_zone_t *z);

/**
 * Unclips the drawing area.
 *
 * @param z : zone to use to retrieve the cairo context
 */
void dg_base_zone_unclip(dg_base_zone_t *z);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DG_BASE_ZONE_H */

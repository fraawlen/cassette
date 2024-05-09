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
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#include <cairo/cairo.h>

#include <dg/core/config.h>
#include <dg/core/core.h>
#include <dg/core/errno.h>

#include "config.h"
#include "zone.h"

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
dg_base_zone_apply_core_font(dg_base_zone_t *z, bool bold)
{	
	assert(z && z->c_ctx);

	cairo_antialias_t      antialias    = CAIRO_ANTIALIAS_DEFAULT;
	cairo_subpixel_order_t subpixel     = CAIRO_SUBPIXEL_ORDER_DEFAULT;
	cairo_hint_metrics_t   hint_metrics = CAIRO_HINT_METRICS_DEFAULT;
	cairo_hint_style_t     hint_style   = CAIRO_HINT_STYLE_SLIGHT;

	cairo_font_options_t *c_opt = cairo_font_options_create();
	if (cairo_font_options_status(c_opt) != CAIRO_STATUS_SUCCESS) {
		dg_core_errno_set(DG_CORE_ERRNO_CAIRO);
		cairo_font_options_destroy(c_opt);
		return;
	}

	switch (DG_CORE_CONFIG->ft_antialias) {

		case DG_CORE_CONFIG_ANTIALIAS_NONE:
			antialias = CAIRO_ANTIALIAS_NONE;
			break;
		case DG_CORE_CONFIG_ANTIALIAS_GRAY:
			antialias = CAIRO_ANTIALIAS_GRAY;
			break;
		case DG_CORE_CONFIG_ANTIALIAS_SUBPIXEL:
			antialias = CAIRO_ANTIALIAS_SUBPIXEL;
			break;
	}

	switch (DG_CORE_CONFIG->ft_subpixel) {

		case DG_CORE_CONFIG_SUBPIXEL_RGB:
			subpixel = CAIRO_SUBPIXEL_ORDER_RGB;
			break;
		case DG_CORE_CONFIG_SUBPIXEL_BGR:
			subpixel = CAIRO_SUBPIXEL_ORDER_BGR;
			break;
		case DG_CORE_CONFIG_SUBPIXEL_VRGB:
			subpixel = CAIRO_SUBPIXEL_ORDER_VRGB;
			break;
		case DG_CORE_CONFIG_SUBPIXEL_VBGR:
			subpixel = CAIRO_SUBPIXEL_ORDER_VBGR;
			break;
	}

	if (DG_CORE_CONFIG->ft_hint_metrics) {
		hint_metrics = CAIRO_HINT_METRICS_ON;
	} else {
		hint_metrics = CAIRO_HINT_METRICS_OFF;
	}

	cairo_font_options_set_antialias(c_opt, antialias);
	cairo_font_options_set_subpixel_order(c_opt, subpixel);
	cairo_font_options_set_hint_metrics(c_opt, hint_metrics);
	cairo_font_options_set_hint_style(c_opt, hint_style);

	cairo_set_font_size(z->c_ctx, DG_CORE_CONFIG->ft_size);
	cairo_set_font_options(z->c_ctx, c_opt);
	cairo_select_font_face(
		z->c_ctx,
		DG_CORE_CONFIG->ft_face,
		CAIRO_FONT_SLANT_NORMAL,
		bold ? CAIRO_FONT_WEIGHT_BOLD : CAIRO_FONT_WEIGHT_NORMAL);

	cairo_font_options_destroy(c_opt);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_zone_clip(dg_base_zone_t *z)
{
	assert(z && z->c_ctx);

	if (z->pw <= 0 || z->ph <= 0) {
		return;
	}

	cairo_rectangle(z->c_ctx, z->px, z->py, z->pw, z->ph);
	cairo_clip(z->c_ctx);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

dg_base_zone_t
dg_base_zone_get_body(const dg_core_cell_drawing_context_t *dc, const dg_base_config_style_t *style)
{
	assert(style);

	dg_base_zone_t z = dg_base_zone_get_cell(dc);

	dg_base_zone_pad(&z, style->margin);

	return z;
}	

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

dg_base_zone_t
dg_base_zone_get_cell(const dg_core_cell_drawing_context_t *dc)
{
	assert(dc);

	dg_base_zone_t z = {
		.px    = dc->cell_px,
		.py    = dc->cell_py,
		.pw    = dc->cell_pw,
		.ph    = dc->cell_ph,
		.c_ctx = dc->c_ctx,
	};

	return z;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

dg_base_zone_t
dg_base_zone_get_foreground(const dg_core_cell_drawing_context_t *dc, const dg_base_config_style_t *style)
{
	assert(style);

	dg_base_zone_t z = dg_base_zone_get_cell(dc);

	dg_base_zone_pad(&z, style->margin + style->pad_fg);

	return z;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

dg_base_zone_t
dg_base_zone_get_icon(const dg_core_cell_drawing_context_t *dc, const dg_base_config_style_t *style)
{
	assert(dc && style);

	const int16_t l = dg_core_config_get_cell_height(1);

	dg_base_zone_t z = {
		.px    = dc->cell_px +  dc->cell_pw - l,
		.py    = dc->cell_py + (dc->cell_ph - l) / 2,
		.pw    = l,
		.ph    = l,
		.c_ctx = dc->c_ctx,
	};

	dg_base_zone_pad(&z, style->pad_icon);

	return z;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

dg_base_zone_t
dg_base_zone_get_label(const dg_core_cell_drawing_context_t *dc, const dg_base_config_style_t *style,
                       bool substract_icon)
{
	assert(style);

	dg_base_zone_t z = dg_base_zone_get_cell(dc);

	dg_base_zone_pad(&z, DG_CORE_CONFIG->win_pad_cell);
	if (substract_icon) {
		z.pw -= dg_core_config_get_cell_height(1) - style->pad_icon - style->thick_bd - style->margin * 2;
	}

	return z;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

dg_base_zone_t
dg_base_zone_get_window(dg_core_window_t *w)
{
	assert(w);

	dg_base_zone_t z = {
		.px    = 0,
		.py    = 0,
		.pw    = dg_core_window_get_pixel_width(w), 
		.ph    = dg_core_window_get_pixel_height(w),
		.c_ctx = dg_core_window_get_cairo_context(w),
	};

	return z;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_zone_pad(dg_base_zone_t *z, int16_t pad)
{
	assert(z);

	z->px += pad;
	z->py += pad;
	z->pw -= pad * 2;
	z->ph -= pad * 2;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_zone_unclip(dg_base_zone_t *z)
{
	assert(z && z->c_ctx);

	cairo_reset_clip(z->c_ctx);
}

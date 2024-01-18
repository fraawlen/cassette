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
#include <string.h>

#include <cairo/cairo.h>

#include <dg/core/config.h>
#include <dg/core/core.h>
#include <dg/core/errno.h>

#include "config.h"
#include "draw.h"
#include "origin.h"
#include "rotation.h"
#include "string.h"
#include "zone.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define _IS_ZONE_VALID(Z) {assert(z && z->c_ctx); if (Z->pw <= 0 || Z->ph <= 0) { return; }}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

typedef struct {
	bool left;
	bool right;
} _lim_sides_t;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void _draw_text_row    (dg_base_zone_t *z, int16_t px, int16_t py, const char *str, size_t str_n, dg_base_origin_t og);
static void _matrix_normalize (dg_base_zone_t *z);
static void _matrix_restore   (dg_base_zone_t *z, cairo_matrix_t *c_mat);
static void _matrix_rotate    (dg_base_zone_t *z, dg_base_rotation_t rot, double x, double y);
static void _set_antialias    (dg_base_zone_t *z, bool ena);

static cairo_matrix_t _matrix_get (dg_base_zone_t *z);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static const _lim_sides_t _lim_sides[9] = {
	{1, 1}, /* CENTER       */
	{0, 1}, /* LEFT         */
	{1, 0}, /* RIGHT        */
	{1, 1}, /* TOP          */
	{0, 1}, /* TOP LEFT     */
	{1, 0}, /* TOP_RIGHT    */
	{1, 1}, /* BOTTOM       */
	{0, 1}, /* BOTTOM LEFT  */
	{1, 0}, /* BOTTOM RIGHT */
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
dg_base_draw_arc(dg_base_zone_t *z, dg_core_color_t cl, double x, double y, double r, double a1, double a2,
                 int16_t thickness)
{
	_IS_ZONE_VALID(z);

	cairo_matrix_t c_mat = _matrix_get(z);

	_set_antialias(z, true);
	_matrix_normalize(z);
		cairo_arc(z->c_ctx, x, y, r, a1, a2);
	_matrix_restore(z, &c_mat);

	cairo_set_source_rgba(z->c_ctx, cl.r, cl.g, cl.b, cl.a);
	cairo_set_line_width(z->c_ctx, thickness);
	cairo_stroke(z->c_ctx);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_draw_circle(dg_base_zone_t *z, dg_core_color_t cl, double x, double y, double r, int16_t thickness)
{
	_IS_ZONE_VALID(z);

	cairo_matrix_t c_mat = _matrix_get(z);

	_set_antialias(z, true);
	_matrix_normalize(z);
		cairo_arc(z->c_ctx, x, y, r, 0.0, DG_BASE_DRAW_PI * 2);
	_matrix_restore(z, &c_mat);

	cairo_set_source_rgba(z->c_ctx, cl.r, cl.g, cl.b, cl.a);

	if (thickness > 0) {
		cairo_set_line_width(z->c_ctx, thickness);
		cairo_stroke(z->c_ctx);
	} else {
		cairo_fill(z->c_ctx);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_draw_body(dg_base_zone_t *z, const dg_base_config_style_t *style)
{
	_IS_ZONE_VALID(z);

	assert(style);

	if (style->margin > 0) {
		dg_base_zone_pad(z, -style->margin);
		dg_base_draw_fill(z, DG_CORE_CONFIG->win_cl_bg);
		dg_base_zone_pad(z, style->margin);
	}

	dg_base_draw_fill(z, style->cl_bd);
	dg_base_zone_pad(z, style->thick_bd);
	dg_base_draw_fill(z, style->cl_bg);
	dg_base_zone_pad (z, -style->thick_bd);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_draw_contour(dg_base_zone_t *z, dg_core_color_t cl, int16_t thickness)
{
	_IS_ZONE_VALID(z);

	cairo_set_source_rgba(z->c_ctx, cl.r, cl.g, cl.b, cl.a);
	cairo_rectangle(z->c_ctx, z->px,         z->py,               z->pw,             thickness);
	cairo_rectangle(z->c_ctx, z->px,         z->py + z->ph,       z->pw,           - thickness);
	cairo_rectangle(z->c_ctx, z->px,         z->py + thickness,   thickness, z->ph - thickness * 2);
	cairo_rectangle(z->c_ctx, z->px + z->pw, z->py + thickness, - thickness, z->ph - thickness * 2);
	cairo_fill(z->c_ctx);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_draw_fill(dg_base_zone_t *z, dg_core_color_t cl)
{
	_IS_ZONE_VALID(z);

	cairo_rectangle(z->c_ctx, z->px, z->py, z->pw, z->ph);
	cairo_set_source_rgba(z->c_ctx, cl.r, cl.g, cl.b, cl.a);
	cairo_fill(z->c_ctx);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_draw_focus(dg_base_zone_t *z, const dg_base_config_style_t *style,
                   dg_core_cell_drawing_context_t *dc)
{
	_IS_ZONE_VALID(z);

	assert(style && dc);
	
	dg_core_color_t cl;

	switch (dc->focus) {
		
		case DG_CORE_CELL_FOCUS_NONE:
			cl = DG_CORE_CONFIG->win_cl_bg;
			break;

		case DG_CORE_CELL_FOCUS_SECONDARY:
			cl = DG_BASE_CONFIG->focus_cl_secondary;
			break;

		case DG_CORE_CELL_FOCUS_PRIMARY:
			cl = DG_BASE_CONFIG->focus_cl_primary;
			break;

		case DG_CORE_CELL_FOCUS_PRIMARY_LOCKED:
			cl = DG_BASE_CONFIG->focus_cl_primary_lock;
			break;
	}

	if (!dc->is_enabled) {
		cl = DG_CORE_CONFIG->win_cl_bg;
	}

	dg_base_zone_pad(z, -DG_BASE_CONFIG->focus_margin);
	dg_base_draw_contour(z, cl, -DG_BASE_CONFIG->focus_thick);
	dg_base_zone_pad(z, DG_BASE_CONFIG->focus_margin);

	/* assuming the given zone was obtained from dg_base_zone_get_body(), if the focus is drawn outside the */
	/* cell's area then add the out-of-bounds message to the drawing context to be processed by the core    */

	if (style->margin < DG_BASE_CONFIG->focus_margin + DG_BASE_CONFIG->focus_thick) {
		dc->msg |= DG_CORE_CELL_DRAW_MSG_OUT_OF_BOUNDS;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_draw_label(dg_base_zone_t *z, const dg_base_config_style_t *style, const dg_base_string_t *str, 
                   dg_base_origin_t og)
{
	_IS_ZONE_VALID(z);

	assert(style && str);

	dg_base_zone_t z_tmp = *z;

	/* draw limits and adjust label zone if needed */

	if (dg_core_config_fit_str_width(z->pw) >= str->n_cols) {
		goto skip_lim;
	}

	if (_lim_sides[og].left) {
		cairo_rectangle(z->c_ctx, z->px, z->py, style->thick_lim, z->ph);
		z->px += style->thick_lim + style->gap_lim;
		z->pw -= style->thick_lim + style->gap_lim;
	}

	if (_lim_sides[og].right) {
		cairo_rectangle(z->c_ctx, z->px + z->pw, z->py, -style->thick_lim, z->ph);
		z->pw -= style->thick_lim + style->gap_lim;
	}

	cairo_set_source_rgba(z->c_ctx, style->cl_lim.r, style->cl_lim.g, style->cl_lim.b, style->cl_lim.a);
	cairo_fill(z->c_ctx);

skip_lim:

	/* draw label */

	dg_base_zone_clip(z);
	dg_base_draw_string(
		z,
		style->cl_ft,
		str,
		dg_base_origin_get_zone_x_position(og, z),
		dg_base_origin_get_zone_y_position(og, z),
		style->ft_bold,
		og,
		DG_BASE_ROTATION_NORMAL);
	dg_base_zone_unclip(z);

	*z = z_tmp;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_draw_lines(dg_base_zone_t *z, dg_core_color_t cl, const dg_base_draw_point_t *points, size_t n,
                   int16_t thickness)
{
	_IS_ZONE_VALID(z);

	assert(points);

	if (n < 2) {
		return;
	}

	cairo_matrix_t c_mat = _matrix_get(z);
	bool antialias = false;

	_matrix_normalize(z);
		cairo_move_to(z->c_ctx, points[0].x, points[0].y);
		for (size_t i = 1; i < n; i++) {
			cairo_line_to(z->c_ctx, points[i].x, points[i].y);
			antialias |= points[i].x != points[i - 1].x && points[i].y != points[i - 1].y;
		}
	_matrix_restore(z, &c_mat);

	_set_antialias(z, antialias);
	cairo_set_source_rgba(z->c_ctx, cl.r, cl.g, cl.b, cl.a);
	cairo_set_line_width(z->c_ctx, thickness);
	cairo_stroke(z->c_ctx);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_draw_rectangle(dg_base_zone_t *z, dg_core_color_t cl, double x1, double y1, double x2, double y2,
                       int16_t thickness)
{
	_IS_ZONE_VALID(z);

	cairo_matrix_t c_mat = _matrix_get(z);

	_set_antialias(z, false);
	_matrix_normalize(z);
		cairo_rectangle(z->c_ctx, x1, y1, x2 - x1, y2 - y1);
	_matrix_restore(z, &c_mat);

	cairo_set_source_rgba(z->c_ctx, cl.r, cl.g, cl.b, cl.a);

	if (thickness > 0) {
		cairo_set_line_width(z->c_ctx, thickness);
		cairo_stroke(z->c_ctx);
	} else {
		cairo_fill(z->c_ctx);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_draw_segment(dg_base_zone_t *z, dg_core_color_t cl, double x1, double y1, double x2, double y2,
                     int16_t thickness)
{
	_IS_ZONE_VALID(z);

	cairo_matrix_t c_mat = _matrix_get(z);

	_set_antialias(z, x1 != x2 && y1 != y2);
	_matrix_normalize(z);
		cairo_move_to(z->c_ctx, x1, y1);
		cairo_line_to(z->c_ctx, x2, y2);
	_matrix_restore(z, &c_mat);

	cairo_set_source_rgba(z->c_ctx, cl.r, cl.g, cl.b, cl.a);
	cairo_set_line_width(z->c_ctx, thickness);
	cairo_stroke(z->c_ctx);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_draw_separator(dg_base_zone_t *z, const dg_base_config_style_t *style, int16_t px, int16_t py,
                       int16_t length, dg_base_rotation_t rot)
{
	_IS_ZONE_VALID(z);

	assert(style);

	if (style->thick_sep <= 0) {
		return;
	}

	const dg_core_color_t cl = style->cl_sep;
	const int16_t x = z->px + px;
	const int16_t y = z->py + py;

	cairo_matrix_t c_mat = _matrix_get(z);

	_set_antialias(z, false);
	_matrix_rotate(z, rot, px, py);
		cairo_rectangle(z->c_ctx, x, y, style->thick_sep, length);	
		cairo_set_source_rgba(z->c_ctx, cl.r, cl.g, cl.b, cl.a);
		cairo_fill(z->c_ctx);
	_matrix_restore(z, &c_mat);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void dg_base_draw_string(dg_base_zone_t *z, dg_core_color_t cl, const dg_base_string_t *str, int16_t px,
                         int16_t py, bool bold, dg_base_origin_t og, dg_base_rotation_t rot)
{
	_IS_ZONE_VALID(z);

	assert(str);

	if (!str->chars) {
		return;
	}

	int16_t offset = DG_CORE_CONFIG->ft_ascent;

	/* calc initial vertical offset */

	switch (og) {

		case DG_BASE_ORIGIN_TOP:
		case DG_BASE_ORIGIN_TOP_LEFT:
		case DG_BASE_ORIGIN_TOP_RIGHT:
			break;

		case DG_BASE_ORIGIN_CENTER:
		case DG_BASE_ORIGIN_LEFT:
		case DG_BASE_ORIGIN_RIGHT:
			offset -= dg_core_config_convert_str_height(str->n_rows) / 2;
			break;

		case DG_BASE_ORIGIN_BOTTOM:
		case DG_BASE_ORIGIN_BOTTOM_LEFT:
		case DG_BASE_ORIGIN_BOTTOM_RIGHT:
			offset -= dg_core_config_convert_str_height(str->n_rows);
			break;
	}

	/* setup drawing context */

	cairo_matrix_t c_mat = _matrix_get(z);

	cairo_set_source_rgba(z->c_ctx, cl.r, cl.g, cl.b, cl.a);
	dg_base_zone_apply_core_font(z, bold);
	_matrix_rotate(z, rot, px, py);

	/* draw rows */

	const char *s = str->chars;
	bool end = false;
	size_t n = 0;

	for (size_t i = 0; !end; i++) {
		switch (str->chars[i]) {
			
			case '\0':
				end = true;
				/* fallthrough */

			case '\n':
				_draw_text_row(z, px, py + offset, s, n, og);
				offset += DG_CORE_CONFIG->ft_ph + DG_CORE_CONFIG->ft_spacing_h;
				s = str->chars + i + 1;
				n = 0;
				break;

			default:
				n++;
				break;
		}
	}

	/* end */
		
	_matrix_restore(z, &c_mat);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_draw_triangle(dg_base_zone_t *z, dg_core_color_t cl, double x1, double y1, double x2, double y2,
                      double x3, double y3, int16_t thickness)
{
	_IS_ZONE_VALID(z);

	cairo_matrix_t c_mat = _matrix_get(z);

	_set_antialias(z, true);
	_matrix_normalize(z);
		cairo_move_to(z->c_ctx, x1, y1);
		cairo_line_to(z->c_ctx, x2, y2);
		cairo_line_to(z->c_ctx, x3, y3);
		cairo_line_to(z->c_ctx, x1, y1);
	_matrix_restore(z, &c_mat);

	cairo_set_source_rgba(z->c_ctx, cl.r, cl.g, cl.b, cl.a);

	if (thickness > 0) {
		cairo_set_line_width(z->c_ctx, thickness);
		cairo_stroke(z->c_ctx);
	} else {
		cairo_fill(z->c_ctx);
	}
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static void
_draw_text_row(dg_base_zone_t *z, int16_t px, int16_t py, const char *str, size_t str_n, dg_base_origin_t og)
{
	int16_t offset = 0;

	px += z->px;
	py += z->py;

	/* get glyph array */

	int glyphs_n = 0;
	cairo_glyph_t *glyphs = NULL;
	cairo_scaled_font_t *c_sft = cairo_get_scaled_font(z->c_ctx);
	cairo_status_t status;

	status = cairo_scaled_font_text_to_glyphs(c_sft, px, py, str, str_n, &glyphs, &glyphs_n, NULL, NULL, NULL);
	if (status != CAIRO_STATUS_SUCCESS) {
		dg_core_errno_set(DG_CORE_ERRNO_CAIRO);
		return;
	}

	/* calc initial horizontal offset */

	switch (og) {

		case DG_BASE_ORIGIN_LEFT:
		case DG_BASE_ORIGIN_TOP_LEFT:
		case DG_BASE_ORIGIN_BOTTOM_LEFT:
			break;
		
		case DG_BASE_ORIGIN_CENTER:
		case DG_BASE_ORIGIN_TOP:
		case DG_BASE_ORIGIN_BOTTOM:
			offset -= dg_core_config_convert_str_width(glyphs_n) / 2;
			break;

		case DG_BASE_ORIGIN_RIGHT:
		case DG_BASE_ORIGIN_TOP_RIGHT:
		case DG_BASE_ORIGIN_BOTTOM_RIGHT:
			offset -= dg_core_config_convert_str_width(glyphs_n);
			break;
	}

	/* transformations */

	for (int i = 0; i < glyphs_n; i++) {
		glyphs[i].x  = px + offset + DG_CORE_CONFIG->ft_offset_x;
		glyphs[i].y += DG_CORE_CONFIG->ft_offset_y;
		offset += DG_CORE_CONFIG->ft_pw + DG_CORE_CONFIG->ft_spacing_w;
	}

	/* draw */

	cairo_show_glyphs(z->c_ctx, glyphs, glyphs_n);
	cairo_glyph_free(glyphs);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static cairo_matrix_t
_matrix_get(dg_base_zone_t *z)
{
	cairo_matrix_t c_mat;

	cairo_get_matrix(z->c_ctx, &c_mat);

	return c_mat;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_matrix_normalize(dg_base_zone_t *z)
{
	cairo_translate(z->c_ctx, z->px, z->py);
	cairo_scale(z->c_ctx, z->pw, z->ph);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_matrix_restore(dg_base_zone_t *z, cairo_matrix_t *c_mat)
{
	cairo_set_matrix(z->c_ctx, c_mat);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_matrix_rotate(dg_base_zone_t *z, dg_base_rotation_t rot, double x, double y)
{
	double a = 0;

	switch (rot) {
		
		case DG_BASE_ROTATION_NORMAL:
			a = 0;
			break;

		case DG_BASE_ROTATION_INVERTED:
			a = DG_BASE_DRAW_PI;
			break;

		case DG_BASE_ROTATION_LEFT:
			a = DG_BASE_DRAW_PI / 2;
			break;

		case DG_BASE_ROTATION_RIGHT:
			a = -DG_BASE_DRAW_PI / 2;
			break;
	}

	cairo_translate(z->c_ctx, z->px + x, z->py + y);
	cairo_rotate(z->c_ctx, a);
	cairo_translate(z->c_ctx, - z->px - x, - z->py - y);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_set_antialias(dg_base_zone_t *z, bool ena)
{
	cairo_set_antialias(z->c_ctx, ena ? CAIRO_ANTIALIAS_DEFAULT : CAIRO_ANTIALIAS_NONE);

	// TODO add more options
}

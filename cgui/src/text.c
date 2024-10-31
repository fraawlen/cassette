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

#include <cairo/cairo.h>
#include <cassette/cgui.h>
#include <cassette/cobj.h>

#include "config.h"
#include "main.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define PI 3.14159265358979323846

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void draw_row (struct cgui_text, cairo_t *, const char *, size_t, double, double, enum cgui_origin) CGUI_NONNULL(2, 3);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cgui_text_draw(struct cgui_text style, const cstr *str, double x, double y, enum cgui_origin origin, enum cgui_rotation rotation, cairo_t *drawable)
{
	cairo_matrix_t matrix;
	const char *s = cstr_chars(str);
	double offset = CONFIG->font_ascent;
	double a      = 0;
	size_t n      = 0;
	bool end      = false;

	/* initial vertical offset */

	switch (origin)
	{
		case CGUI_ORIGIN_TOP:
		case CGUI_ORIGIN_TOP_LEFT:
		case CGUI_ORIGIN_TOP_RIGHT:
			break;

		case CGUI_ORIGIN_CENTER:
		case CGUI_ORIGIN_LEFT:
		case CGUI_ORIGIN_RIGHT:
			offset -= cgui_config_str_height(cstr_height(str)) / 2;
			break;

		case CGUI_ORIGIN_BOTTOM:
		case CGUI_ORIGIN_BOTTOM_LEFT:
		case CGUI_ORIGIN_BOTTOM_RIGHT:
			offset -= cgui_config_str_height(cstr_height(str));
			break;
	}

	/* setup matrix context */

	switch (rotation) {
		
		case CGUI_ROTATION_NORMAL:
			a = 0;
			break;

		case CGUI_ROTATION_INVERTED:
			a = PI;
			break;

		case CGUI_ROTATION_LEFT:
			a = PI / 2;
			break;

		case CGUI_ROTATION_RIGHT:
			a = -PI / 2;
			break;
	}

	cairo_get_matrix(drawable, &matrix);
	cairo_translate(drawable, x, y);
	cairo_rotate(drawable, a);
	cairo_translate(drawable, - x, - y);

	/* font setup */

	cairo_set_font_size(drawable, CONFIG->font_size);
	cairo_set_font_options(drawable, config_font_options());
	cairo_select_font_face(
		drawable,
		CONFIG->font_face,
		CAIRO_FONT_SLANT_NORMAL,
		style.bold ? CAIRO_FONT_WEIGHT_BOLD  : CAIRO_FONT_WEIGHT_NORMAL);

	/* draw rows */

	for (size_t i = 0; !end; i++)
	{
		switch (s[i])
		{
			case '\0':
				end = true;
				/* fallthrough */

			case '\n':
				draw_row(style, drawable, s + n, i - n, x, y + offset, origin);
				offset += CONFIG->font_height + CONFIG->font_spacing_vertical;
				n = i + 1;
				break;
				
			default:
				break;
		}
	}

	/* end */
		
	cairo_set_matrix(drawable, &matrix);
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
draw_row(struct cgui_text style, cairo_t *drawable, const char *str, size_t str_n, double x, double y, enum cgui_origin origin)
{
	cairo_scaled_font_t *font;
	cairo_glyph_t *glyphs = NULL;
	cairo_status_t status;
	struct ccolor color;
	double offset = 0.0;
	double og = 0.0;
	int glyphs_n  = 0;

	/* get glyph array */

	font   = cairo_get_scaled_font(drawable);
	status = cairo_scaled_font_text_to_glyphs(font, x, y, str, str_n, &glyphs, &glyphs_n, NULL, NULL, NULL);
	if (status != CAIRO_STATUS_SUCCESS)
	{
		main_set_error(CERR_CAIRO);
		return;
	}

	/* initial horizontal offset */

	switch (origin)
	{
		case CGUI_ORIGIN_TOP_LEFT:
		case CGUI_ORIGIN_LEFT:
		case CGUI_ORIGIN_BOTTOM_LEFT:
			break;

		case CGUI_ORIGIN_TOP:
		case CGUI_ORIGIN_CENTER:
		case CGUI_ORIGIN_BOTTOM:
			offset -= cgui_config_str_width(glyphs_n) / 2;
			break;

		case CGUI_ORIGIN_TOP_RIGHT:
		case CGUI_ORIGIN_RIGHT:
		case CGUI_ORIGIN_BOTTOM_RIGHT:
			offset -= cgui_config_str_width(glyphs_n);
			break;
	}

	og = offset;

	/* glyph position transformations */

	for (int i = 0; i < glyphs_n; i++)
	{
		glyphs[i].x  = CONFIG->font_offset_x + x + offset;
		glyphs[i].y += CONFIG->font_offset_y;
		offset      += CONFIG->font_width + CONFIG->font_spacing_horizontal;
	}

	/* draw text background */

	if (!style.draw_background)
	{
		goto skip_background;
	}

	color = style.color_background;
	
	cairo_set_source_rgba(drawable, color.r, color.g, color.b, color.a);
	cairo_rectangle(
		drawable,
		x + og,
		y - CONFIG->font_ascent - CONFIG->font_spacing_vertical / 2,
		cgui_config_str_width(glyphs_n),
		CONFIG->font_height + CONFIG->font_spacing_vertical);
	cairo_fill(drawable);

skip_background:

	/* draw text */

	color = style.color;

	cairo_set_source_rgba(drawable, color.r, color.g, color.b, color.a);
	cairo_show_glyphs(drawable, glyphs, glyphs_n);
	cairo_glyph_free(glyphs);
}

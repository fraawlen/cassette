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

static void draw_row (struct cgui_text_context, struct cgui_text_segment *, size_t, cairo_glyph_t *, int, const char *, size_t) CGUI_NONNULL(2, 4, 6);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cgui_text_draw(struct cgui_text_context context, struct cgui_text_style style, const cstr *str)
{
	struct cgui_text_segment seg =
	{
		.style  = style,
		.length = SIZE_MAX,
	};

	cgui_text_draw_segments(context, &seg, 1, str);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_text_draw_segments(struct cgui_text_context context, struct cgui_text_segment *segments, size_t segments_number, const cstr *str)
{
	cairo_matrix_t matrix;
	cairo_glyph_t *glyphs;
	const char *s = cstr_chars(str);
	bool end = false;
	size_t n = 0;
	double a = 0;
	size_t tmp;

	/* glyph buffer setup */

	if (!csafe_mul(&tmp, cstr_width(str), sizeof(cairo_glyph_t)))
	{
		main_set_error(CERR_OVERFLOW);
		return;
	}

	if (!(glyphs = malloc(tmp)))
	{
		main_set_error(CERR_MEMORY);
		return;
	}

	/* initial vertical offset */

	switch (context.align)
	{
		case CGUI_ALIGN_TOP:
		case CGUI_ALIGN_TOP_LEFT:
		case CGUI_ALIGN_TOP_RIGHT:
			break;

		case CGUI_ALIGN_CENTER:
		case CGUI_ALIGN_LEFT:
		case CGUI_ALIGN_RIGHT:
			context.y -= cgui_config_str_height(cstr_height(str)) / 2;
			break;

		case CGUI_ALIGN_BOTTOM:
		case CGUI_ALIGN_BOTTOM_LEFT:
		case CGUI_ALIGN_BOTTOM_RIGHT:
			context.y -= cgui_config_str_height(cstr_height(str));
			break;
	}

	/* setup cairo context */

	switch (context.rotation)
	{
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

	cairo_set_font_size(context.drawable, CONFIG->font_size);
	cairo_set_font_options(context.drawable, config_font_options());
	cairo_get_matrix(context.drawable, &matrix);
	cairo_translate(context.drawable, context.x, context.y);
	cairo_rotate(context.drawable, a);
	cairo_translate(context.drawable, -context.x, -context.y);

	/* draw rows */

	for (const char *c = s; !end; c = cstr_next_char(c))
	{
		switch (*c)
		{
			case '\0':
				end = true;
				/* fallthrough */

			case '\n':
				draw_row(context, segments, segments_number, glyphs, n, s, c - s);
				context.y += CONFIG->font_height + CONFIG->font_spacing_vertical;
				s = c + 1;
				n = 0;
				break;

			default:
				break;
		}
		n++;
	}

	/* end */
		
	cairo_set_matrix(context.drawable, &matrix);
	free(glyphs);
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
draw_row(
	struct cgui_text_context context,
	struct cgui_text_segment *segments,
	size_t segments_number,
	cairo_glyph_t *glyphs,
	int glyphs_n,
	const char *str,
	size_t str_n)
{
	cairo_status_t status;
	struct ccolor color;

	/* font setup */

	cairo_select_font_face(
		context.drawable,
		CONFIG->font_face,
		CAIRO_FONT_SLANT_NORMAL,
		segments[0].style.bold ? CAIRO_FONT_WEIGHT_BOLD : CAIRO_FONT_WEIGHT_NORMAL);

	/* get glyph array */

	status = cairo_scaled_font_text_to_glyphs(
		cairo_get_scaled_font(context.drawable),
		context.x,
		context.y,
		str,
		str_n,
		&glyphs,
		&glyphs_n,
		NULL, NULL, NULL);

	if (status != CAIRO_STATUS_SUCCESS)
	{
		main_set_error(CERR_CAIRO);
		return;
	}

	/* initial horizontal offset */

	switch (context.align)
	{
		case CGUI_ALIGN_TOP_LEFT:
		case CGUI_ALIGN_LEFT:
		case CGUI_ALIGN_BOTTOM_LEFT:
			break;

		case CGUI_ALIGN_TOP:
		case CGUI_ALIGN_CENTER:
		case CGUI_ALIGN_BOTTOM:
			context.x -= cgui_config_str_width(glyphs_n) / 2;
			break;

		case CGUI_ALIGN_TOP_RIGHT:
		case CGUI_ALIGN_RIGHT:
		case CGUI_ALIGN_BOTTOM_RIGHT:
			context.x -= cgui_config_str_width(glyphs_n);
			break;
	}

	/* glyph position transformations */

	for (int i = 0; i < glyphs_n; i++)
	{
		glyphs[i].x = context.x + CONFIG->font_offset_x;
		glyphs[i].y = context.y + CONFIG->font_offset_y + CONFIG->font_ascent;
		context.x  += CONFIG->font_width + CONFIG->font_spacing_horizontal;
	}

	/* draw text */

	color = segments[0].style.color;

	cairo_set_source_rgba(context.drawable, color.r, color.g, color.b, color.a);
	cairo_show_glyphs(context.drawable, glyphs, glyphs_n);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
draw_segment(struct cgui_text_context context, struct cgui_text_segment segment, cairo_glyph_t *glyphs, int glyphs_n)
{
	// TODO
}

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
#include <math.h>
#include <stdbool.h>
#include <stdlib.h>

#include "config.h"
#include "main.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define PI        3.14159265358979323846
#define GLYPH_ARR 512

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void draw_row (cairo_t *, const char *, const char *, size_t, size_t, size_t, double) CGUI_NONNULL(1, 2);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static size_t ctx_row_min = 0;
static size_t ctx_row_max = SIZE_MAX;
static size_t ctx_col_min = 0;
static size_t ctx_col_max = SIZE_MAX;
static size_t ctx_cdp_min = 0;
static size_t ctx_cdp_max = SIZE_MAX;
static double ctx_x       = 0.0;
static double ctx_y       = 0.0;
static bool   ctx_link    = false;

static enum   cgui_align      ctx_align = CGUI_ALIGN_TOP_LEFT;
static enum   cgui_rotation   ctx_rot   = CGUI_ROTATION_NORMAL;
static struct cgui_text ctx_style = {0};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cgui_text_align(enum cgui_align alignment)
{
	ctx_align = alignment;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_text_col_range(size_t col_min, size_t col_max)
{
	ctx_col_min = col_min;
	ctx_col_max = col_max;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_text_draw(cairo_t *drawable, const cstr *str)
{
	cairo_font_weight_t weight;
	cairo_matrix_t matrix;
	double y;
	double a = 0.0;

	/* row parsing params */

	const char *s1 = NULL; /* first byte to render        */
	const char *s2 = NULL; /* last  byte to render        */
	size_t b1 = 0;         /* row render boundary col 1   */
	size_t b2 = 0;         /* row render boundary col 2   */
	size_t n = 0;          /* total row width             */
	size_t r = 0;          /* row offset                  */

	/* vertical alignment */

	y = ctx_y + CONFIG->font_offset_y + CONFIG->font_ascent;

	switch (ctx_align)
	{
		case CGUI_ALIGN_TOP:
		case CGUI_ALIGN_TOP_LEFT:
		case CGUI_ALIGN_TOP_RIGHT:
			break;

		case CGUI_ALIGN_CENTER:
		case CGUI_ALIGN_LEFT:
		case CGUI_ALIGN_RIGHT:
			y -= cgui_config_str_height(cstr_height(str)) / 2;
			break;

		case CGUI_ALIGN_BOTTOM:
		case CGUI_ALIGN_BOTTOM_LEFT:
		case CGUI_ALIGN_BOTTOM_RIGHT:
			y -= cgui_config_str_height(cstr_height(str));
			break;
	}

	/* setup rotation matrix */

	switch (ctx_rot)
	{
		case CGUI_ROTATION_NORMAL:
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
	cairo_translate(drawable, ctx_x, y);
	cairo_rotate(drawable, a);
	cairo_translate(drawable, -ctx_x, -y);

	/* setup cairo font */

	weight = ctx_style.bold ? CAIRO_FONT_WEIGHT_BOLD : CAIRO_FONT_WEIGHT_NORMAL;

	cairo_set_font_size(drawable, CONFIG->font_size);
	cairo_set_font_options(drawable, config_font_options());
	cairo_select_font_face(drawable, CONFIG->font_face, CAIRO_FONT_SLANT_NORMAL, weight);

	/* draw rows */

	for (const char *c = cstr_chars(str);; c = cstr_next_char(c))
	{
		if (*c == '\0' || *c == '\n')
		{
			if (r >= ctx_row_min && n > 0)
			{
				draw_row(drawable, s1, s2, b1, b2, n, y);
			}
			if (*c == '\0' || ++r > ctx_row_max)
			{
				break;
			}
			y += CONFIG->font_height + CONFIG->font_spacing_vertical;
			s1 = c + 1;
			b1 = 0;
			n  = 0;
		}
		else
		{
			if (n <= ctx_col_min && (r == ctx_row_min || !ctx_link))
			{
				s1 = c;
				b1 = n;
			}
			if (n <= ctx_col_max || (r != ctx_row_max && ctx_link))
			{
				s2 = c;
				b2 = n;
			}
			n++;
		}
	}

	/* end */
		
	cairo_set_matrix(drawable, &matrix);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_text_link_ranges(void)
{
	ctx_link = true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_text_reset(void)
{
	ctx_row_min = 0;
	ctx_row_max = SIZE_MAX;
	ctx_col_min = 0;
	ctx_col_max = SIZE_MAX;
	ctx_cdp_min = 0;
	ctx_cdp_max = SIZE_MAX;
	ctx_x       = 0.0;
	ctx_y       = 0.0;
	ctx_align   = CGUI_ALIGN_TOP_LEFT;
	ctx_rot     = CGUI_ROTATION_NORMAL;
	ctx_style   = (struct cgui_text){0};
	ctx_link    = false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_text_row_range(size_t row_min, size_t row_max)
{
	ctx_row_min = row_min;
	ctx_row_max = row_max;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_text_rotation(enum cgui_rotation rotation)
{
	ctx_rot = rotation;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_text_style(struct cgui_text style)
{
	ctx_style = style;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_text_unlink_ranges(void)
{
	ctx_link = false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_text_x(double x)
{
	ctx_x = x;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_text_y(double y)
{
	ctx_y = y;
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void 
draw_row(cairo_t *drawable, const char *s1, const char *s2, size_t c1, size_t c2, size_t w, double y)
{
	cairo_scaled_font_t *font;
	cairo_glyph_t arr[GLYPH_ARR];
	cairo_glyph_t *glyphs;
	cairo_status_t status;
	struct ccolor cl;
	double x;
	double l = CONFIG->font_spacing_horizontal;
	int    n = GLYPH_ARR;

	/* horizontal alignment */

	x = ctx_x + CONFIG->font_offset_x + cgui_config_str_width(c1) + (c1 > 0 ? l : 0); 

	switch (ctx_align)
	{
		case CGUI_ALIGN_TOP_LEFT:
		case CGUI_ALIGN_LEFT:
		case CGUI_ALIGN_BOTTOM_LEFT:
			break;

		case CGUI_ALIGN_TOP:
		case CGUI_ALIGN_CENTER:
		case CGUI_ALIGN_BOTTOM:
			x -= cgui_config_str_width(w) / 2;
			break;

		case CGUI_ALIGN_TOP_RIGHT:
		case CGUI_ALIGN_RIGHT:
		case CGUI_ALIGN_BOTTOM_RIGHT:
			x -= cgui_config_str_width(w);
			break;
	}

	/* get glyphs */

	glyphs = arr;
	font   = cairo_get_scaled_font(drawable);
	status = cairo_scaled_font_text_to_glyphs(font, 0, 0, s1, s2 - s1 + 1, &glyphs, &n, NULL, NULL, NULL);
	
	if (status != CAIRO_STATUS_SUCCESS)
	{
		main_set_error(CERR_CAIRO);
		return;
	}

	/* draw background */

	if (ctx_style.draw_background)
	{
		cl = ctx_style.color_background;
		
		cairo_set_source_rgba(drawable, cl.r, cl.g, cl.b, cl.a);
		cairo_rectangle(drawable,
			x - CONFIG->font_background_hpad,
			y - CONFIG->font_background_vpad - CONFIG->font_ascent,
			2 * CONFIG->font_background_hpad + cgui_config_str_width(c2 - c1 + 1),
			2 * CONFIG->font_background_vpad + CONFIG->font_height);
		cairo_fill(drawable);
	}

	/* draw glyphs */

	for (int i = 0; i < n; i++)
	{
		glyphs[i].x = x;
		glyphs[i].y = y;
		x          += CONFIG->font_width + l;
	}

	cl = ctx_style.color;

	cairo_set_source_rgba(drawable, cl.r, cl.g, cl.b, cl.a);
	cairo_show_glyphs(drawable, glyphs, n);

	/* potential cleanup (see cairo_scaled_font_text_to_glyphs()) */

	if (glyphs != arr)
	{
		free(glyphs);
	}
}


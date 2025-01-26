/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Objects (COBJ) library.
 *
 * This library is free software; you can redistribute it and/or modify it either under the terms of the GNU
 * Lesser General Public License as published by the Free Software Foundation; either version 3.0 of the
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

#include "config.h"
#include "main.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define PI 3.14159265358979323846
#define  U 0.382683432 /* sin(PI/8) */

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void paint     (cairo_t *, struct ccolor color)                    CGUI_NONNULL(1);
static void path      (cairo_t *, bool, double)                           CGUI_NONNULL(1);
static void subpath_1 (cairo_t *, double, double, double, double, double) CGUI_NONNULL(1);
static void subpath_2 (cairo_t *, double, double, double, double, double) CGUI_NONNULL(1);
static void subpath_3 (cairo_t *, double, double, double, double, double) CGUI_NONNULL(1);
static void subpath_4 (cairo_t *, double, double, double, double, double) CGUI_NONNULL(1);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static double          ctx_x      = 0.0;
static double          ctx_y      = 0.0;
static double          ctx_width  = 0.0;
static double          ctx_height = 0.0;
static struct cgui_box ctx_box    = {0};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cgui_box_clip(cairo_t *drawable, double pad)
{
	path(drawable, true, pad);
	cairo_clip(drawable);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_box_draw(cairo_t *drawable)
{
	if (!ctx_box.draw)
	{
		return;
	}

	cairo_new_path(drawable);
	cairo_set_operator(drawable, CAIRO_OPERATOR_SOURCE);

	/* shadow */

	if (ctx_box.draw_shadow)
	{
		ctx_x += ctx_box.shadow_x_offset;
		ctx_y += ctx_box.shadow_y_offset;
		path(drawable, ctx_box.shape_outline && ctx_box.shape_border, -ctx_box.size_outline);
		paint(drawable, ctx_box.color_shadow);
		ctx_x -= ctx_box.shadow_x_offset;
		ctx_y -= ctx_box.shadow_y_offset;
	}

	/* outline */

	if (ctx_box.size_outline > 0.0)
	{
		path(drawable, ctx_box.shape_outline && ctx_box.shape_border, -ctx_box.size_outline);
		paint(drawable, ctx_box.color_outline);
	}

	/* border */

	if (ctx_box.size_border > 0.0)
	{
		path(drawable, ctx_box.shape_border, 0.0);
		paint(drawable, ctx_box.color_border);
	}

	/* background */

	path(drawable, true, ctx_box.size_border);
	paint(drawable, ctx_box.color_background);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgui_box_inside(cairo_t *drawable, double x, double y)
{
	cairo_new_path(drawable);
	path(drawable, ctx_box.shape_border, ctx_box.hit_outline ? -ctx_box.size_outline : 0);

	return cairo_in_fill(drawable, x, y);

	// TODO checking without using cairo
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_box_move(double x, double y)
{
	ctx_x = x;
	ctx_y = y;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_box_move_shadow(double x_light, double y_light, double max_light_distance, double max_offset)
{
	double x;
	double y;
	double d;
	double a;
	double m;
	double r;

	/* light position relative to box's center */

	x = x_light - ctx_width  / 2;
	y = y_light - ctx_height / 2;
	
	/* calculate shadow offset ratios */

	m = ctx_width / 2 + max_light_distance;
	a = atan2(y, x) + PI;
	d = sqrt(pow(x, 2) + pow(y, 2));
	r = d > m ? 1.0 : d / m;

	/* end */

	ctx_box.shadow_x_offset = r * cos(a) * max_offset;
	ctx_box.shadow_y_offset = r * sin(a) * max_offset;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_box_pad_all_corners(struct cgui_box *box, struct cgui_box box_parent, double pad)
{
	cgui_box_pad_corner(box, box_parent, pad, 0);
	cgui_box_pad_corner(box, box_parent, pad, 1);
	cgui_box_pad_corner(box, box_parent, pad, 2);
	cgui_box_pad_corner(box, box_parent, pad, 3);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_box_pad_corner(struct cgui_box *box, struct cgui_box box_parent, double pad, int id)
{
	if (id > 3 || id < 0)
	{
		main_set_error(CERR_PARAM);
		return;
	}

	if (!box->smart_corners || box_parent.corner[id] == CGUI_CORNER_STRAIGHT)
	{
		return;
	}
	
	box->corner[id]      = box_parent.corner[id];
	box->size_corner[id] = box_parent.size_corner[id]
	                       - pad * (1 - (box_parent.corner[id] == CGUI_CORNER_CHAMFER ? U : 0));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_box_reset(void)
{
	ctx_x      = 0.0;
	ctx_y      = 0.0;
	ctx_width  = 0.0;
	ctx_height = 0.0;
	ctx_box    = (struct cgui_box){0};
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_box_resize(double width, double height)
{
	ctx_width  = width;
	ctx_height = height;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_box_style(struct cgui_box box)
{
	ctx_box = box;
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
paint(cairo_t *drawable, struct ccolor color)
{
	cairo_set_source_rgba(drawable, color.r, color.g, color.b, color.a);
	cairo_fill(drawable);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
path(cairo_t *drawable, bool shape, double pad)
{
	double x;
	double y;
	double w;
	double h;
	double r[4];

	pad += ctx_box.margin;

	x = ctx_x + pad;
	y = ctx_y + pad;
	w = ctx_width  - pad * 2;
	h = ctx_height - pad * 2;

	if (w < 1.0 || h < 1.0)
	{
		return;
	}

	if (!shape)
	{
		cairo_rectangle(drawable, x, y, w, h);
		return;
	}

	for (size_t i = 0; i < 4; i++)
	{
		r[i] = ctx_box.size_corner[i] - pad * (1 - (ctx_box.corner[i] == CGUI_CORNER_CHAMFER ? U : 0));
		if (r[i] < 0.0)
		{
			r[i] = 0.0;
		}
	}

	subpath_1(drawable, x, y, w, h, r[0]);
	subpath_2(drawable, x, y, w, h, r[1]);
	subpath_3(drawable, x, y, w, h, r[2]);
	subpath_4(drawable, x, y, w, h, r[3]);	
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
subpath_1(cairo_t *d, double x, double y, double w, double h, double r)
{
	(void)w;
	(void)h;

	switch (ctx_box.corner[0])
	{
		case CGUI_CORNER_STRAIGHT:
			cairo_move_to(d, x, y);
			break;

		case CGUI_CORNER_RADII:
			cairo_new_sub_path(d);
			cairo_arc(d, x + r, y + r, r, PI, -PI / 2);
			break;

		case CGUI_CORNER_CHAMFER:
			cairo_move_to(d, x,     y + r);
			cairo_line_to(d, x + r, y);
			break;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
subpath_2(cairo_t *d, double x, double y, double w, double h, double r)
{
	(void)h;

	switch (ctx_box.corner[1])
	{
		case CGUI_CORNER_STRAIGHT:
			cairo_line_to(d, x + w, y);
			break;

		case CGUI_CORNER_RADII:
			cairo_arc(d, x + w - r, y + r, r, -PI / 2, 0);
			break;

		case CGUI_CORNER_CHAMFER:
			cairo_line_to(d, x + w - r, y);
			cairo_line_to(d, x + w,     y + r);
			break;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
subpath_3(cairo_t *d, double x, double y, double w, double h, double r)
{
	switch (ctx_box.corner[2])
	{
		case CGUI_CORNER_STRAIGHT:
			cairo_line_to(d, x + w, y + h);
			break;

		case CGUI_CORNER_RADII:
			cairo_arc(d, x + w - r, y + h - r, r, 0, PI / 2);
			break;

		case CGUI_CORNER_CHAMFER:
			cairo_line_to(d, x + w,     y + h - r);
			cairo_line_to(d, x + w - r, y + h);
			break;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
subpath_4(cairo_t *d, double x, double y, double w, double h, double r)
{
	(void)w;

	switch (ctx_box.corner[3])
	{
		case CGUI_CORNER_STRAIGHT:
			cairo_line_to(d, x, y + h);
			break;

		case CGUI_CORNER_RADII:
			cairo_arc(d, x + r, y + h - r, r, PI / 2, PI);
			break;

		case CGUI_CORNER_CHAMFER:
			cairo_line_to(d, x + r, y + h);
			cairo_line_to(d, x,     y + h - r);
			break;
	}
	
	cairo_close_path(d);
}

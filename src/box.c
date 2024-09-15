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
#include <float.h>
#include <math.h>

#include "config.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define PI 3.14159265358979323846
#define  U 0.382683432 /* sin(PI/8) */

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void paint     (cairo_t *, struct ccolor color)                                           CGUI_NONNULL(1);
static void path      (struct cgui_box, double, double, double, double, cairo_t *, bool, double) CGUI_NONNULL(6);
static void subpath_1 (struct cgui_box, double, double, double, double, cairo_t *)               CGUI_NONNULL(6);
static void subpath_2 (struct cgui_box, double, double, double, double, cairo_t *)               CGUI_NONNULL(6);
static void subpath_3 (struct cgui_box, double, double, double, double, cairo_t *)               CGUI_NONNULL(6);
static void subpath_4 (struct cgui_box, double, double, double, double, cairo_t *)               CGUI_NONNULL(6);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cgui_box_clip(struct cgui_box box, double x, double y, double width, double height, double pad, cairo_t *drawable)
{
	path(box, x, y, width, height, drawable, true, pad);
	cairo_clip(drawable);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_box_draw(struct cgui_box box, double x, double y, double width, double height, cairo_t *drawable)
{
	if (!box.draw)
	{
		return;
	}

	/* outline */

	if (box.size_outline > 0.0)
	{
		path(box, x, y, width, height, drawable, box.shape_outline && box.shape_border, -box.size_outline);
		paint(drawable, box.color_outline);
	}

	/* border */

	if (box.size_border > 0.0)
	{
		path(box, x, y, width, height, drawable, box.shape_border, 0.0);
		paint(drawable, box.color_border);
	}

	/* background */

	if (box.padding > 0.0 || !box.draw_foreground)
	{
		path(box, x, y, width, height, drawable, true, box.size_border);
		paint(drawable, box.color_background);
	}

	/* foreground */

	if (box.draw_foreground)
	{
		path(box, x, y, width, height, drawable, true, box.size_border + box.padding);
		paint(drawable, box.color_foreground);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgui_box_is_in(struct cgui_box box, double x_test, double y_test, double x, double y, double width, double height, cairo_t *drawable)
{
	cairo_new_path(drawable);
	path(box, x, y, width, height, drawable, box.shape_border, 0.0);

	return cairo_in_fill(drawable, x_test, y_test);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_box_pad_corner(struct cgui_box *box, struct cgui_box box_parent, double pad, int id)
{
	if (!CONFIG->smart_corners || box_parent.corner[id] == CGUI_BOX_STRAIGHT)
	{
		return;
	}
	
	box->corner[id]      = box_parent.corner[id];
	box->size_corner[id] = box_parent.size_corner[id]
	                       - pad * (1 - (box_parent.corner[id] == CGUI_BOX_CHAMFER ? U : 0));
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

static void
path(struct cgui_box box, double x, double y, double w, double h, cairo_t *drawable, bool shape, double pad)
{
	pad += box.margin;
	x   += pad;
	y   += pad;
	w   -= pad * 2;
	h   -= pad * 2;

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
		box.size_corner[i] -= pad * (1 - (box.corner[i] == CGUI_BOX_CHAMFER ? U : 0));
		if (box.size_corner[i] < 0.0)
		{
			box.corner[i] = CGUI_BOX_STRAIGHT;
		}
	}

	subpath_1(box, x, y, w, h, drawable);
	subpath_2(box, x, y, w, h, drawable);
	subpath_3(box, x, y, w, h, drawable);
	subpath_4(box, x, y, w, h, drawable);	
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
subpath_1(struct cgui_box box, double x, double y, double w, double h, cairo_t *d)
{
	const double r = box.size_corner[0];

	(void)w;
	(void)h;

	switch (box.corner[0])
	{
		case CGUI_BOX_STRAIGHT:
			cairo_move_to(d, x, y);
			break;

		case CGUI_BOX_RADII:
			cairo_new_sub_path(d);
			cairo_arc(d, x + r, y + r, r, PI, -PI / 2);
			break;

		case CGUI_BOX_CHAMFER:
			cairo_move_to(d, x,     y + r);
			cairo_line_to(d, x + r, y);
			break;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
subpath_2(struct cgui_box box, double x, double y, double w, double h, cairo_t *d)
{
	const double r = box.size_corner[1];

	(void)h;

	switch (box.corner[1])
	{
		case CGUI_BOX_STRAIGHT:
			cairo_line_to(d, x + w, y);
			break;

		case CGUI_BOX_RADII:
			cairo_arc(d, x + w - r, y + r, r, -PI / 2, 0);
			break;

		case CGUI_BOX_CHAMFER:
			cairo_line_to(d, x + w - r, y);
			cairo_line_to(d, x + w,     y + r);
			break;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
subpath_3(struct cgui_box box, double x, double y, double w, double h, cairo_t *d)
{
	const double r = box.size_corner[2];

	switch (box.corner[2])
	{
		case CGUI_BOX_STRAIGHT:
			cairo_line_to(d, x + w, y + h);
			break;

		case CGUI_BOX_RADII:
			cairo_arc(d, x + w - r, y + h - r, r, 0, PI / 2);
			break;

		case CGUI_BOX_CHAMFER:
			cairo_line_to(d, x + w,     y + h - r);
			cairo_line_to(d, x + w - r, y + h);
			break;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
subpath_4(struct cgui_box box, double x, double y, double w, double h, cairo_t *d)
{
	const double r = box.size_corner[3];

	(void)w;

	switch (box.corner[3])
	{
		case CGUI_BOX_STRAIGHT:
			cairo_line_to(d, x, y + h);
			break;

		case CGUI_BOX_RADII:
			cairo_arc(d, x + r, y + h - r, r, PI / 2, PI);
			break;

		case CGUI_BOX_CHAMFER:
			cairo_line_to(d, x + r, y + h);
			cairo_line_to(d, x,     y + h - r);
			break;
	}
	
	cairo_close_path(d);
}

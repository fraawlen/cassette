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

#include "config.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define PI 3.14159265358979323846
#define  U 0.382683432 /* sin(PI/8) */

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void paint     (struct cgui_zone, struct ccolor color);
static void path      (struct cgui_box,  struct cgui_zone, bool shape, double pad);
static void subpath_1 (struct cgui_box,  struct cgui_zone);
static void subpath_2 (struct cgui_box,  struct cgui_zone);
static void subpath_3 (struct cgui_box,  struct cgui_zone);
static void subpath_4 (struct cgui_box,  struct cgui_zone);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cgui_box_clip(struct cgui_box box, struct cgui_zone zone, double pad)
{
	path(box, zone, true, pad);
	cairo_clip(zone.drawable);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_box_draw(struct cgui_box box, struct cgui_zone zone)
{
	if (!box.draw)
	{
		return;
	}

	/* outline */

	if (box.size_outline > 0.0)
	{
		path(box, zone, box.shape_outline, -box.size_outline);
		paint(zone, box.color_outline);
	}

	/* border */

	if (box.size_border > 0.0)
	{
		path(box, zone, box.shape_border || box.shape_outline, 0);
		paint(zone, box.color_border);
	}

	/* background */

	path(box, zone, true, box.size_border);
	paint(zone, box.color_background);
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
paint(struct cgui_zone zone, struct ccolor color)
{
	cairo_set_source_rgba(zone.drawable, color.r, color.g, color.b, color.a);
	cairo_fill(zone.drawable);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
path(struct cgui_box box, struct cgui_zone zone, bool shape, double pad)
{
	zone.x      += pad;
	zone.y      += pad;
	zone.width  -= pad * 2;
	zone.height -= pad * 2;

	if (zone.width < 1.0 || zone.height < 1.0)
	{
		return;
	}

	if (!shape)
	{
		cairo_rectangle(zone.drawable, zone.x, zone.y, zone.width, zone.height);
		return;
	}

	for (size_t i = 0; i < 4; i++)
	{
		box.size_corner[i] -= pad * (1 - (box.corner[i] == CGUI_BOX_CHAMFER ? U : 0));
		if (box.size_corner[i] < 0.0)
		{
			box.size_corner[i] = 0.0;
		}
	}

	subpath_1(box, zone);
	subpath_2(box, zone);
	subpath_3(box, zone);
	subpath_4(box, zone);	
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
subpath_1(struct cgui_box box, struct cgui_zone zone)
{
	const double r = box.size_corner[0];
	const double x = zone.x;
	const double y = zone.y;
	cairo_t     *d = zone.drawable;

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
subpath_2(struct cgui_box box, struct cgui_zone zone)
{
	const double r = box.size_corner[1];
	const double x = zone.x;
	const double y = zone.y;
	const double w = zone.width;
	cairo_t     *d = zone.drawable;

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
subpath_3(struct cgui_box box, struct cgui_zone zone)
{
	const double r = box.size_corner[2];
	const double x = zone.x;
	const double y = zone.y;
	const double w = zone.width;
	const double h = zone.height;
	cairo_t     *d = zone.drawable;

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
subpath_4(struct cgui_box box, struct cgui_zone zone)
{
	const double r = box.size_corner[3];
	const double x = zone.x;
	const double y = zone.y;
	const double h = zone.height;
	cairo_t     *d = zone.drawable;

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

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

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define PI 3.14159265358979323846
#define  U 0.382683432 /* sin(PI/8) */

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void _path_1 (struct cgui_box, struct cgui_zone, bool);
static void _path_2 (struct cgui_box, struct cgui_zone, bool);
static void _path_3 (struct cgui_box, struct cgui_zone, bool);
static void _path_4 (struct cgui_box, struct cgui_zone, bool);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cgui_box_draw(struct cgui_box box, struct cgui_zone zone)
{
	struct ccolor cl = box.color_border;

	/* outer path */

	if (box.thickness == 0)
	{
		goto skip_outer;
	}

	cl = box.color_border;

	if (box.outer_shaping)
	{
		_path_1(box, zone, true);
		_path_2(box, zone, true);
		_path_3(box, zone, true);
		_path_4(box, zone, true);
	}
	else
	{
		cairo_rectangle(zone.drawable, zone.x, zone.y, zone.width, zone.height);
	}

	cairo_set_source_rgba(zone.drawable, cl.r, cl.g, cl.b, cl.a);
	cairo_fill(zone.drawable);

skip_outer:

	/* inner path */

	cl = box.color_background;

	_path_1(box, zone, false);
	_path_2(box, zone, false);
	_path_3(box, zone, false);
	_path_4(box, zone, false);

	cairo_set_source_rgba(zone.drawable, cl.r, cl.g, cl.b, cl.a);
	cairo_fill(zone.drawable);
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
_path_1(struct cgui_box box, struct cgui_zone zone, bool outer)
{
	cairo_t *d = zone.drawable;
	 int16_t x = zone.x;
	 int16_t y = zone.y;
	uint16_t t = box.thickness;
	uint16_t r = box.corner_size[0];
	uint16_t o = outer ? t : 0;
	uint16_t u = outer ? t * U : 0;

	switch (box.corner_type[0])
	{
		case CGUI_BOX_STRAIGHT:
			cairo_move_to(d, x + t - o, y + t - o);
			break;

		case CGUI_BOX_RADII:
			cairo_new_sub_path(d);
			cairo_arc(d, x + t + r, y + t + r, r + o, PI, -PI / 2);
			break;

		case CGUI_BOX_CHAMFER:
			cairo_move_to(d, x + t - o, y + t + r - u);
			cairo_line_to(d, x + t + r - u, y + t - o);
			break;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_path_2(struct cgui_box box, struct cgui_zone zone, bool outer)
{
	cairo_t *d = zone.drawable;
	uint16_t x = zone.x;
	uint16_t y = zone.y;
	uint16_t w = zone.width;
	uint16_t t = box.thickness;
	uint16_t r = box.corner_size[1];
	uint16_t o = outer ? t : 0;
	uint16_t u = outer ? t * U : 0;

	switch (box.corner_type[1])
	{
		case CGUI_BOX_STRAIGHT:
			cairo_line_to(d, w + x - t + o, y + t - o);
			break;

		case CGUI_BOX_RADII:
			cairo_arc(d, w + x - t - r, y + t + r, r + o, -PI / 2, 0);
			break;

		case CGUI_BOX_CHAMFER:
			cairo_line_to(d, w + x - t - r + u, y + t - o);
			cairo_line_to(d, w + x - t + o, y + t + r - u);
			break;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_path_3(struct cgui_box box, struct cgui_zone zone, bool outer)
{
	cairo_t *d = zone.drawable;
	uint16_t x = zone.x;
	uint16_t y = zone.y;
	uint16_t w = zone.width;
	uint16_t h = zone.height;
	uint16_t t = box.thickness;
	uint16_t r = box.corner_size[2];
	uint16_t o = outer ? t : 0;
	uint16_t u = outer ? t * U : 0;

	switch (box.corner_type[2])
	{
		case CGUI_BOX_STRAIGHT:
			cairo_line_to(d, w + x - t + o, h + y - t + o);
			break;

		case CGUI_BOX_RADII:
			cairo_arc(d, w + x - t - r, h + y - t - r, r + o, 0, PI / 2);
			break;

		case CGUI_BOX_CHAMFER:
			cairo_line_to(d, w + x - t + o, h + y - t - r + u);
			cairo_line_to(d, w + x - t - r + u, h + y - t + o);
			break;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_path_4(struct cgui_box box, struct cgui_zone zone, bool outer)
{
	cairo_t *d = zone.drawable;
	uint16_t x = zone.x;
	uint16_t y = zone.y;
	uint16_t h = zone.height;
	uint16_t t = box.thickness;
	uint16_t r = box.corner_size[3];
	uint16_t o = outer ? t : 0;
	uint16_t u = outer ? t * U : 0;

	switch (box.corner_type[3])
	{
		case CGUI_BOX_STRAIGHT:
			cairo_line_to(d, x + t - o, h + y - t + o);
			break;

		case CGUI_BOX_RADII:
			cairo_arc(d, x + t + r, h + y - t - r, r + o, PI / 2, PI);
			break;

		case CGUI_BOX_CHAMFER:
			cairo_line_to(d, x + t + r - u, h + y - t + o);
			cairo_line_to(d, x + t - o, h + y - t - r + u);
			break;
	}
	
	cairo_close_path(d);
}

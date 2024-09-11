/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Graphics (CGUI) library.
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

#include "config.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void draw  (cgui_cell *, struct cgui_cell_context *) CGUI_NONNULL(1, 2);
static void frame (cgui_cell *, struct cgui_box          *) CGUI_NONNULL(1, 2);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

cgui_cell *
cgui_stripes_create(void)
{
	cgui_cell *cell;

	cell = cgui_cell_create();

	cgui_cell_on_draw (cell, draw);
	cgui_cell_on_frame(cell, frame);

	return cell;
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
draw(cgui_cell *cell, struct cgui_cell_context *context)
{
	const struct ccolor cl = CONFIG->stripes_color;
	const double w         = CONFIG->stripes_width;
	const double s         = CONFIG->stripes_spacing * 1.414213562;

	(void)cell;

	cgui_box_draw(context->frame, context->zone);
	cgui_box_clip(context->frame, context->zone);

	for (double x = context->x; x < context->x + context->width + w; x += w + s)
	{
		cairo_move_to(context->drawable, x, context->y);
		cairo_line_to(context->drawable, x + context->height, context->y + context->height);
	}

	for (double y = context->y; y < context->y + context->height + w; y += w + s)
	{
		cairo_move_to(context->drawable, context->x, y);
		cairo_line_to(context->drawable, context->x + context->width, y + context->width);
	}
	
	cairo_set_source_rgba(context->drawable, cl.r, cl.g, cl.b, cl.a);
	cairo_set_line_cap(context->drawable, CAIRO_LINE_CAP_SQUARE);
	cairo_set_line_width(context->drawable, w);
	cairo_stroke(context->drawable);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
frame(cgui_cell *cell, struct cgui_box *box)
{
	(void)cell;

	*box = CONFIG->stripes_frame;
}


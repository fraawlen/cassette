/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Graphics (CGUI) library.
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

#include "cell.h"
#include "config.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void draw  (cgui_cell *, struct cgui_cell_context) CGUI_NONNULL(1);
static void frame (cgui_cell *, struct cgui_box *)        CGUI_NONNULL(1, 2);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

cgui_cell *
cgui_placeholder_create(void)
{
	cgui_cell *cell;

	cell = cgui_cell_create();

	cgui_cell_on_draw (cell, draw);
	cgui_cell_on_frame(cell, frame);
	cgui_cell_set_serial(cell, CELL_PLACEHOLDER);

	return cell;
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
draw(cgui_cell *cell, struct cgui_cell_context context)
{
	struct ccolor cl = CONFIG->placeholder_line_color;
	double o = cgui_box_content_offset(context.frame);
	double w = context.width  - o * 2;
	double h = context.height - o * 2;
	double x = context.x      + o;
	double y = context.y      + o;

	(void)cell;

	/* frame */

	cgui_cell_draw_frame(context);
	cgui_cell_clip_frame(context);

	/* cross */

	cairo_move_to(context.drawable, x,     y);
	cairo_line_to(context.drawable, x + w, y + h);
	cairo_move_to(context.drawable, x + w, y);
	cairo_line_to(context.drawable, x,     y + h);

	cairo_set_source_rgba(context.drawable, cl.r, cl.g, cl.b, cl.a);
	cairo_set_line_cap(context.drawable, CAIRO_LINE_CAP_SQUARE);
	cairo_set_line_width(context.drawable, CONFIG->placeholder_line_width);
	cairo_stroke(context.drawable);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
frame(cgui_cell *cell, struct cgui_box *box)
{
	(void)cell;

	*box = CONFIG->placeholder_frame;
}


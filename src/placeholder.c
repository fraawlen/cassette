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

#include <cassette/cgui.h>

#include "config.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void draw(cgui_cell *cell, struct cgui_cell_context *context) CGUI_NONNULL(1, 2);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

cgui_cell *
cgui_placeholder_create(void)
{
	cgui_cell *cell;

	cell = cgui_cell_create();

	cgui_cell_on_draw(cell, draw);

	return cell;
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
draw(cgui_cell *cell, struct cgui_cell_context *context)
{
	const double t = CONFIG->placeholder_frame.size_border + CONFIG->placeholder_frame.margin + 10;
	const double x = context->zone.x;
	const double y = context->zone.y;
	const double w = context->zone.width;
	const double h = context->zone.height;
	cairo_t     *d = context->zone.drawable;

	(void)cell;

	cgui_box_draw(CONFIG->placeholder_frame, context->zone);
	cgui_box_clip(CONFIG->placeholder_frame, context->zone, t);

	cairo_move_to(d, x + t,     y + t);
	cairo_line_to(d, x - t + w, y - t + h);
	cairo_set_source_rgb(d, 0.0, 0.15, 0.15);
	cairo_set_line_width(d, 30.0);
	cairo_stroke(d);
	cairo_reset_clip(d);
}

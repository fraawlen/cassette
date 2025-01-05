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
#include <cassette/cobj.h>
#include <stdbool.h>
#include <stdlib.h>

#include "cell.h"
#include "config.h"
#include "main.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define DATA ((struct data*)cgui_cell_data(cell))

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct data
{
	cstr *label;
	enum cgui_align align;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void destroy (cgui_cell *)                           CGUI_NONNULL(1);
static void draw    (cgui_cell *, struct cgui_cell_context) CGUI_NONNULL(1);
static void frame   (cgui_cell *, struct cgui_box *)        CGUI_NONNULL(1, 2);
static bool invalid (const cgui_cell *)                     CGUI_NONNULL(1);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cgui_label_align(cgui_cell *cell, enum cgui_align alignment)
{
	if (invalid(cell))
	{
		return;
	}
	
	DATA->align = alignment;
	
	cgui_cell_redraw(cell);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgui_cell *
cgui_label_create(void)
{
	cgui_cell   *cell;
	struct data *data;

	if (cgui_error())
	{
		goto fail_main;
	}

	if (!(data = malloc(sizeof(struct data))))
	{
		goto fail_data;
	}

	if ((data->label = cstr_create()) == CSTR_PLACEHOLDER)
	{
		goto fail_label;
	}

	if ((cell = cgui_cell_create()) == CGUI_CELL_PLACEHOLDER)
	{
		goto fail_cell;
	}

	data->align = CGUI_ALIGN_TOP_LEFT;

	cgui_cell_on_destroy(cell, destroy);
	cgui_cell_on_draw(cell, draw);
	cgui_cell_on_frame(cell, frame);
	cgui_cell_set_data(cell, data);
	cgui_cell_set_serial(cell, CELL_LABEL);

	return cell;

	/* error */

fail_cell:
	cstr_destroy(data->label);
fail_label:
	free(data);
fail_data:
	main_set_error(CERR_INSTANCE);
fail_main:
	return CGUI_CELL_PLACEHOLDER;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_label_set_label(cgui_cell *cell, const char *label)
{
	if (invalid(cell))
	{
		return;
	}

	cstr_clear(DATA->label);
	cstr_append(DATA->label, label);

	cgui_cell_redraw(cell);
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
destroy(cgui_cell *cell)
{
	cstr_destroy(DATA->label);
	free(DATA);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
draw(cgui_cell *cell, struct cgui_cell_context context)
{
	double l = context.frame.margin + context.frame.size_border + context.frame.padding;
	double x = context.x + l;
	double y = context.y + l;

	/* frame */

	cgui_cell_draw_frame(context);

	/* label */

	context.width  -= l * 2;
	context.height -= l * 2;

	switch (DATA->align)
	{
		case CGUI_ALIGN_TOP_LEFT:
			break;

		case CGUI_ALIGN_TOP:
			y += context.height / 2;
			break;

		case CGUI_ALIGN_TOP_RIGHT:
			y += context.height;
			break;

		case CGUI_ALIGN_LEFT:
			x += context.width / 2;
			break;

		case CGUI_ALIGN_CENTER:
			x += context.width  / 2;
			y += context.height / 2;
			break;

		case CGUI_ALIGN_RIGHT:
			x += context.width / 2;
			y += context.height;
			break;

		case CGUI_ALIGN_BOTTOM_LEFT:
			x += context.width;
			break;

		case CGUI_ALIGN_BOTTOM:
			x += context.width;
			y += context.height / 2;
			break;

		case CGUI_ALIGN_BOTTOM_RIGHT:
			x += context.width;
			y += context.height;
			break;
	}

	cgui_text_x(x);
	cgui_text_y(y);
	cgui_text_align(DATA->align);

	cgui_text_style(CONFIG->label_text);
	cgui_text_draw(context.drawable, DATA->label);

	cgui_text_row_range(2, 5);
	cgui_text_col_range(2, 4);
	cgui_text_link_ranges();
	cgui_text_style(CONFIG->button_text_disabled);
	cgui_text_draw(context.drawable, DATA->label);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
frame(cgui_cell *cell, struct cgui_box *box)
{
	(void)cell;

	*box = CONFIG->label_frame;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
invalid(const cgui_cell *cell)
{
	if (cell->serial != CELL_LABEL)
	{
		main_set_error(CERR_PARAM);
	}

	return cgui_error() || !cell->valid;
}


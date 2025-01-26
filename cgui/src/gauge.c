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

#include <cassette/cgui.h>
#include <cassette/cobj.h>
#include <float.h>
#include <stdbool.h>
#include <stdlib.h>

#include "cell.h"
#include "config.h"
#include "main.h"
#include "util.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define DATA ((struct data*)cgui_cell_data(cell))

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct data
{
	cstr *units;
	cstr *label;
	ssize_t label_size;
	enum cgui_align label_align;
	enum cgui_rotation label_rot;
	enum cgui_rotation rot;
	double val;
	double min;
	double max;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void destroy      (cgui_cell *)                           CGUI_NONNULL(1);
static void draw         (cgui_cell *, struct cgui_cell_context) CGUI_NONNULL(1);
static void frame        (cgui_cell *, struct cgui_box *)        CGUI_NONNULL(1, 2);
static bool invalid      (const cgui_cell *)                     CGUI_NONNULL(1);
static void update_label (cgui_cell *)                           CGUI_NONNULL(1);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cgui_gauge_align_label(cgui_cell *cell, enum cgui_align alignment)
{
	if (invalid(cell))
	{
		return;
	}
	
	DATA->label_align = alignment;
	
	cgui_cell_redraw(cell);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgui_cell *
cgui_gauge_create(void)
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

	if ((data->units = cstr_create()) == CSTR_PLACEHOLDER)
	{
		goto fail_units;
	}

	if ((cell = cgui_cell_create()) == CGUI_CELL_PLACEHOLDER)
	{
		goto fail_cell;
	}

	data->label_size  = 0;
	data->label_align = CGUI_ALIGN_CENTER;
	data->label_rot   = CGUI_ROTATION_NORMAL;
	data->rot         = CGUI_ROTATION_NORMAL;
	data->min         = 0.0;
	data->max         = 0.0;
	data->val         = 0.0;

	cgui_cell_on_destroy(cell, destroy);
	cgui_cell_on_draw(cell, draw);
	cgui_cell_on_frame(cell, frame);
	cgui_cell_set_data(cell, data);
	cgui_cell_set_serial(cell, CELL_GAUGE);

	cgui_gauge_style_percent(cell, 0);

	return cell;

	/* error */

fail_cell:
	cstr_destroy(data->units);
fail_units:
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
cgui_gauge_resize_label(cgui_cell *cell, ssize_t size)
{
	if (invalid(cell))
	{
		return;
	}
	
	DATA->label_size = size;
	
	cgui_cell_redraw(cell);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_gauge_rotate(cgui_cell *cell, enum cgui_rotation rotation)
{
	if (invalid(cell))
	{
		return;
	}
	
	DATA->rot = rotation;
	
	cgui_cell_redraw(cell);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_gauge_rotate_label(cgui_cell *cell, enum cgui_rotation rotation)
{
	if (invalid(cell))
	{
		return;
	}
	
	DATA->label_rot = rotation;
	
	cgui_cell_redraw(cell);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_gauge_set_value(cgui_cell *cell, double value)
{
	if (invalid(cell))
	{
		return;
	}

	DATA->val = value;

	update_label(cell);
	cgui_cell_redraw(cell);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_gauge_limit_value(cgui_cell *cell, double lim_1, double lim_2)
{
	if (invalid(cell))
	{
		return;
	}

	DATA->min = lim_1 < lim_2 ? lim_1 : lim_2;
	DATA->max = lim_1 < lim_2 ? lim_2 : lim_1;

	update_label(cell);
	cgui_cell_redraw(cell);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_gauge_style_label(cgui_cell *cell, int precision, const char *units)
{
	if (invalid(cell))
	{
		return;
	}

	cstr_clear(DATA->units);
	cstr_append(DATA->units, units);
	cstr_set_precision(DATA->label, precision);

	update_label(cell);
	cgui_cell_redraw(cell);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_gauge_style_percent(cgui_cell *cell, int precision)
{
	cgui_gauge_limit_value(cell, 0.0, 100.0);
	cgui_gauge_style_label(cell, precision, "%");
	cgui_gauge_resize_label(cell, 4 + (precision > 0 ? precision + 1: 0));
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
destroy(cgui_cell *cell)
{
	cstr_destroy(DATA->label);
	cstr_destroy(DATA->units);
	free(DATA);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
draw(cgui_cell *cell, struct cgui_cell_context context)
{
	struct cgui_box box;

	double p1 = CONFIG->gauge_frame.padding + CONFIG->gauge_frame.size_border;
	double p2 = CONFIG->gauge_bar.padding   + CONFIG->gauge_bar.size_border;
	double p3 = CONFIG->gauge_label.padding + CONFIG->gauge_label.size_border;

	double l1 = 0;
	double l2; 
	double x;
	double y;

	/* calculate label box length */

	l1 = DATA->label_size != 0 ? cgui_config_str_width(DATA->label_size) + p3 * 2 : 0;
	l1 = l1 < CONFIG->gauge_min_size ? CONFIG->gauge_min_size : l1;

	/* calculate bar length without label box */

	l2  = context.width - l1 - ((l1 > 0.0 ? p2 : 0) + p1) * 2;
	l2 *= (util_limit(DATA->val, DATA->min, DATA->max) - DATA->min) / (DATA->max - DATA->min);

	/* frame */

	cgui_cell_draw_frame(context);
	cgui_cell_clip_frame(context);

	/* bar */

	box = CONFIG->gauge_bar;

	cgui_box_pad_all_corners(&box, context.frame, p1);
	cgui_box_move(context.x + p1, context.y + p1);
	cgui_box_resize(l2 + l1 + p2 * 2, context.height - p1 * 2);
	cgui_box_style(box);
	cgui_box_draw(context.drawable);
	cgui_box_clip(context.drawable, p2);

	/* label box */

	box = CONFIG->gauge_label;

	cgui_box_pad_all_corners(&box, context.frame, p1 + p2);
	cgui_box_move(context.x + p1 + p2 + l2, context.y + p1 + p2);
	cgui_box_resize(l1, context.height - (p1 + p2) * 2);
	cgui_box_style(box);
	cgui_box_draw(context.drawable);
	cgui_box_clip(context.drawable, p3);

	/* label */

	if (DATA->label_size == 0)
	{
		return;
	}

	x = context.x + p1 + p2 + p3 + l2 + cgui_align_offset_x(DATA->label_align, l1 - p3 * 2);
	y = context.y + p1 + p2 + p3 + cgui_align_offset_y(DATA->label_align, context.height - (p1 + p2 + p3) * 2);

	cgui_text_move(x, y);
	cgui_text_align(cgui_align_rotation(DATA->label_align, DATA->label_rot));
	cgui_text_rotate(DATA->label_rot);
	cgui_text_style(CONFIG->gauge_text);
	cgui_text_draw(context.drawable, DATA->label);	
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
frame(cgui_cell *cell, struct cgui_box *box)
{
	(void)cell;

	*box = CONFIG->gauge_frame;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
invalid(const cgui_cell *cell)
{
	if (cell->serial != CELL_GAUGE)
	{
		main_set_error(CERR_PARAM);
	}

	return cgui_error() || !cell->valid;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
update_label(cgui_cell *cell)
{
	size_t min;
	size_t max;

	/* get the glyph col length of the max and min values           */
	/* these will be used to potientally pad the value of the label */
	/* example ([min, val, max]) : [0, 43, 100] --> _43             */

	cstr_clear(DATA->label);
	cstr_append(DATA->label, DATA->min);
	min = cstr_width(DATA->label);

	cstr_clear(DATA->label);
	cstr_append(DATA->label, DATA->max);
	max = cstr_width(DATA->label);

	/* compose the label */

	cstr_clear(DATA->label);
	cstr_append(DATA->label, util_limit(DATA->val, DATA->min, DATA->max));
	cstr_pad(DATA->label, CONFIG->font_padding_pattern, 0, min > max ? min : max);
	cstr_append(DATA->label, DATA->units);
}

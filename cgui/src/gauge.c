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
#include <math.h>
#include <stdbool.h>
#include <stdlib.h>

#include "cell.h"
#include "config.h"
#include "main.h"
#include "util.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define DATA   ((struct data*)cgui_cell_data(cell, CGUI_CELL_IMPLEMENTATION))
#define FRAME  CONFIG->gauge_frame
#define BAR    CONFIG->gauge_bar
#define CURSOR CONFIG->gauge_cursor
#define LABEL  CONFIG->gauge_text

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct zone
{
	struct cgui_box frame;
	double x;
	double y;
	double width;
	double height;
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct bar_context
{
	double l1; /* bar length without cursor length and padding     */
	double l2; /* cursor length                                    */
	double o1; /* frame content offset                             */
	double o2; /* bar content offset                               */
	double o3; /* cursor content offset                            */
	double a;  /* bar extra length to account for cursor + padding */
	double b;  /* frame remaining length                           */
	double c;  /* bar thickness (without max taken in account)     */
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct data
{
	cstr *units;
	cstr *label;
	unsigned long last_load;
	enum cgui_align label_align;
	enum cgui_rotation label_rot;
	enum cgui_rotation rot;
	bool show_label;
	double val;
	double min;
	double max;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/* impure */

static void destroy      (cgui_cell *)                           CGUI_NONNULL(1);
static void draw         (cgui_cell *, struct cgui_cell_context) CGUI_NONNULL(1);
static void frame        (cgui_cell *, struct cgui_box *)        CGUI_NONNULL(1, 2);
static bool invalid      (const cgui_cell *)                     CGUI_NONNULL(1);
static void setup_bar    (const cgui_cell *, struct zone)        CGUI_NONNULL(1);
static void setup_cursor (const cgui_cell *, struct zone)        CGUI_NONNULL(1);
static void setup_label  (const cgui_cell *, struct zone)        CGUI_NONNULL(1);
static void update_label (cgui_cell *)                           CGUI_NONNULL(1);

/* pure */

static struct bar_context pre_setup (const cgui_cell *, struct zone) CGUI_NONNULL(1) CGUI_PURE;

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

void
cgui_gauge_clamp_value(cgui_cell *cell, double lim_1, double lim_2)
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

	data->last_load   = CONFIG->loads;
	data->label_align = CGUI_ALIGN_CENTER;
	data->label_rot   = CGUI_ROTATION_NORMAL;
	data->rot         = CGUI_ROTATION_NORMAL;
	data->show_label  = true;
	data->max         = 100.0;
	data->min         = 0.0;
	data->val         = 0.0;

	cstr_append(data->units, "%");
	cstr_set_precision(data->label, 0);

	cgui_cell_on_destroy(cell, destroy);
	cgui_cell_on_draw(cell, draw);
	cgui_cell_on_frame(cell, frame);
	cgui_cell_set_data(cell, CGUI_CELL_IMPLEMENTATION, data);
	cgui_cell_set_serial(cell, CELL_GAUGE);

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
cgui_gauge_hide_label(cgui_cell *cell)
{
	if (invalid(cell))
	{
		return;
	}
	
	DATA->show_label = false;
	
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
cgui_gauge_set_precision(cgui_cell *cell, int precision)
{
	if (invalid(cell))
	{
		return;
	}

	cstr_set_precision(DATA->label, precision);

	update_label(cell);
	cgui_cell_redraw(cell);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_gauge_set_units(cgui_cell *cell, const char *units)
{
	if (invalid(cell))
	{
		return;
	}

	cstr_clear(DATA->units);
	cstr_append(DATA->units, units);

	update_label(cell);
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
cgui_gauge_show_label(cgui_cell *cell)
{
	if (invalid(cell))
	{
		return;
	}
	
	DATA->show_label = true;
	
	cgui_cell_redraw(cell);
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
	struct zone zone =
	{
		.frame  = context.frame,
		.x      = context.x,
		.y      = context.y,
		.width  = context.width,
		.height = context.height,
	};

	/* check if config reload happened                  */
	/* and update the label if it did (padding pattern) */

	if (CONFIG->loads != DATA->last_load)
	{
		DATA->last_load = CONFIG->loads;
		update_label(cell);
	}

	/* frame */

	cgui_cell_draw_frame(context);
	if (CONFIG->gauge_clip)
	{
		cgui_cell_clip_frame(context);
	}

	/* bar */

	setup_bar(cell, zone);
	cgui_box_draw(context.drawable);

	/* cursor */

	setup_cursor(cell, zone);
	cgui_box_draw(context.drawable);

	/* label */

	if (DATA->show_label)
	{
		setup_label(cell, zone);
		cgui_text_draw(context.drawable, DATA->label);	
	}
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

static struct bar_context
pre_setup(const cgui_cell *cell, struct zone z)
{
	struct bar_context ctx;
	double ratio;
	ssize_t lw;
	ssize_t lh;

	/* independent values */

	ratio  = util_progress(DATA->val, DATA->min, DATA->max);
	ctx.o1 = cgui_box_content_offset(z.frame);
	ctx.o2 = cgui_box_content_offset(BAR);
	ctx.o3 = cgui_box_content_offset(CURSOR);

	/* label length */

	switch (DATA->label_rot)
	{
		case CGUI_ROTATION_LEFT:
		case CGUI_ROTATION_RIGHT:
			lw = -(ssize_t)cstr_height(DATA->label);
			lh = -(ssize_t)cstr_width (DATA->label);
			break;

		default:
		case CGUI_ROTATION_INVERTED:
		case CGUI_ROTATION_NORMAL:
			lw = cstr_width (DATA->label);
			lh = cstr_height(DATA->label);
			break;
	}

	/* other values */

	switch (DATA->rot)
	{
		case CGUI_ROTATION_LEFT:
		case CGUI_ROTATION_RIGHT:
			ctx.l2 = DATA->show_label ? cgui_config_str_height(lh) + ctx.o3 * 2 : 0.0;
			ctx.l2 = ctx.l2 < CONFIG->gauge_min_length ? CONFIG->gauge_min_length : ctx.l2;
			ctx.a  = BAR.bd_size * 2 + (ctx.l2 > 0.0 ? ctx.l2 + BAR.pad * 2 : 0.0);
			ctx.b  = (z.height - ctx.o1 * 2 - ctx.a) * (1 - ratio);
			ctx.c  =  z.width  - ctx.o1 * 2;
			ctx.l1 =  z.height - ctx.o1 * 2 - ctx.a - ctx.b;
			break;

		default:
		case CGUI_ROTATION_INVERTED:
		case CGUI_ROTATION_NORMAL:
			ctx.l2 = DATA->show_label ? cgui_config_str_width(lw) + ctx.o3 * 2 : 0.0;
			ctx.l2 = ctx.l2 < CONFIG->gauge_min_length ? CONFIG->gauge_min_length : ctx.l2;
			ctx.a  = BAR.bd_size * 2 + (ctx.l2 > 0.0 ? ctx.l2 + BAR.pad * 2 : 0.0);
			ctx.b  = (z.width  - ctx.o1 * 2 - ctx.a) * (1 - ratio);
			ctx.c  =  z.height - ctx.o1 * 2;
			ctx.l1 =  z.width  - ctx.o1 * 2 - ctx.a - ctx.b;
			break;
	}

	return ctx;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
setup_bar(const cgui_cell *cell, struct zone z)
{
	struct bar_context ctx = pre_setup(cell, z);
	struct cgui_box box = BAR;
	double x = 0.0;
	double y = 0.0;
	double w;
	double h;
	double o;
	double c;
	double l;

	/* geometry */

	c = util_clamp(ctx.c, 0.0, CONFIG->gauge_max_thick);
	l = ctx.l1 + ctx.a;
	o = ctx.o1;

	switch (DATA->rot)
	{
		case CGUI_ROTATION_RIGHT:
			w = c;
			h = l;
			x = (ctx.c - w) / 2;
			y = ctx.b;
			break;

		case CGUI_ROTATION_LEFT:
			w = c;
			h = l;
			x = (ctx.c - w) / 2;
			break;

		case CGUI_ROTATION_INVERTED:
			w = l;
			h = c;
			x = ctx.b;
			y = (ctx.c - h) / 2;
			break;

		default:
		case CGUI_ROTATION_NORMAL:
			w = l;
			h = c;
			y = (ctx.c - h) / 2;
			break;
	}

	x += z.x + o;
	y += z.y + o;

	/* box setup */

	cgui_box_pad_all_corners(&box, z.frame, o);
	cgui_box_style(box);
	cgui_box_move(round(x), round(y));
	cgui_box_resize(round(w), round(h));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
setup_cursor(const cgui_cell *cell, struct zone z)
{
	struct bar_context ctx = pre_setup(cell, z);
	struct cgui_box box = CURSOR;
	double x = 0.0;
	double y = 0.0;
	double w;
	double h;
	double o;

	/* geometry */

	o = ctx.o1 + ctx.o2;

	switch (DATA->rot)
	{
		case CGUI_ROTATION_RIGHT:
			w = z.width - o * 2;
			h = ctx.l2;
			y = ctx.b;
			break;

		case CGUI_ROTATION_LEFT:
			w = z.width - o * 2;
			h = ctx.l2;
			y = ctx.l1;
			break;

		case CGUI_ROTATION_INVERTED:
			w = ctx.l2;
			h = z.height - o * 2;
			x = ctx.b;
			break;

		default:
		case CGUI_ROTATION_NORMAL:
			w = ctx.l2;
			h = z.height - o * 2;
			x = ctx.l1;
			break;
	}

	x += z.x + o;
	y += z.y + o;

	/* box setup */

	cgui_box_pad_all_corners(&box, z.frame, o);
	cgui_box_style(box);
	cgui_box_move(round(x), round(y));
	cgui_box_resize(round(w), round(h));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
setup_label(const cgui_cell *cell, struct zone z)
{
	struct bar_context ctx = pre_setup(cell, z);
	double x = 0.0;
	double y = 0.0;
	double w;
	double h;
	double o;

	/* geometry */

	o = ctx.o1 + ctx.o2 + ctx.o3;

	switch (DATA->rot)
	{
		case CGUI_ROTATION_RIGHT:
			w = z.width - o * 2;
			h = ctx.l2 - ctx.o3 * 2;
			y = ctx.b;
			break;

		case CGUI_ROTATION_LEFT:
			w = z.width - o * 2;
			h = ctx.l2 - ctx.o3 * 2;
			y = ctx.l1;
			break;

		case CGUI_ROTATION_INVERTED:
			w = ctx.l2 - ctx.o3 * 2;
			h = z.height - o * 2;
			x = ctx.b;
			break;

		default:
		case CGUI_ROTATION_NORMAL:
			w = ctx.l2 - ctx.o3 * 2;
			h = z.height - o * 2;
			x = ctx.l1;
			break;
	}

	x += z.x + o + cgui_align_offset_x(DATA->label_align, w);
	y += z.y + o + cgui_align_offset_y(DATA->label_align, h);

	/* text setup */

	cgui_text_move(x, y);
	cgui_text_align(cgui_align_rotation(DATA->label_align, DATA->label_rot));
	cgui_text_rotate(DATA->label_rot);
	cgui_text_style(LABEL);
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
	cstr_append(DATA->label, util_clamp(DATA->val, DATA->min, DATA->max));
	cstr_pad(DATA->label, CONFIG->font_pad_pattern, 0, min > max ? min : max);
	cstr_append(DATA->label, DATA->units);
}

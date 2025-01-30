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
	unsigned long blink_last;
	unsigned int  blink_factor;
	enum cgui_beacon_state state;
	enum cgui_align align;
	enum cgui_rotation rot;
	bool blink_on;
	cstr *label;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void blink                  (cgui_cell *, unsigned long)            CGUI_NONNULL(1);
static void destroy                (cgui_cell *)                           CGUI_NONNULL(1);
static void draw                   (cgui_cell *, struct cgui_cell_context) CGUI_NONNULL(1);
static void frame                  (cgui_cell *, struct cgui_box *)        CGUI_NONNULL(1, 2);
static bool invalid                (const cgui_cell *)                     CGUI_NONNULL(1);
static struct cgui_text text_style (const cgui_cell *)                     CGUI_NONNULL(1);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cgui_beacon_align_label(cgui_cell *cell, enum cgui_align alignment)
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
cgui_beacon_create(void)
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

	data->state        = CGUI_BEACON_OFF;
	data->align        = CGUI_ALIGN_CENTER;
	data->rot          = CGUI_ROTATION_NORMAL;
	data->blink_on     = true;
	data->blink_last   = 0;
	data->blink_factor = 1;

	cgui_cell_on_destroy(cell, destroy);
	cgui_cell_on_draw(cell, draw);
	cgui_cell_on_frame(cell, frame);
	cgui_cell_on_pre_draw(cell, blink);
	cgui_cell_set_data(cell, data);
	cgui_cell_set_serial(cell, CELL_BEACON);

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
cgui_beacon_rotate_label(cgui_cell *cell, enum cgui_rotation rotation)
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
cgui_beacon_set_blink_speed(cgui_cell *cell, unsigned int factor)
{
	if (factor == 0)
	{
		main_set_error(CERR_PARAM);
		return;
	}

	if (invalid(cell))
	{
		return;
	}

	DATA->blink_factor = factor;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_beacon_set_label(cgui_cell *cell, const char *label)
{
	if (invalid(cell))
	{
		return;
	}

	cstr_clear(DATA->label);
	cstr_append(DATA->label, label);

	cgui_cell_redraw(cell);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_beacon_set_state(cgui_cell *cell, enum cgui_beacon_state state)
{
	if (invalid(cell))
	{
		return;
	}

	if ((DATA->state = state) != CGUI_BEACON_CRITICAL)
	{
		DATA->blink_last = 0;
		DATA->blink_on   = false;
	}

	cgui_cell_redraw(cell);
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
blink(cgui_cell *cell, unsigned long time)
{
	unsigned long limit = 1000;
	unsigned long spent;

	if (DATA->state != CGUI_BEACON_CRITICAL)
	{
		return;
	}

	spent  = time - DATA->blink_last;
	limit *= DATA->blink_on ? CONFIG->beacon_blink_on : CONFIG->beacon_blink_off;
	limit /= DATA->blink_factor;

	if (spent >= limit)
	{
		DATA->blink_on   = !DATA->blink_on;
		DATA->blink_last = time;
		spent = 0;
	}
	
	cgui_cell_redraw_delayed(cell, limit - spent);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

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
	double o = cgui_box_content_offset(context.frame);
	double x = context.x + o + cgui_align_offset_x(DATA->align, context.width  - o * 2);
	double y = context.y + o + cgui_align_offset_y(DATA->align, context.height - o * 2);

	/* frame */

	cgui_cell_draw_frame(context);
	cgui_cell_clip_frame(context);

	/* label */

	cgui_text_move(x, y);
	cgui_text_align(cgui_align_rotation(DATA->align, DATA->rot));
	cgui_text_rotate(DATA->rot);
	cgui_text_style(text_style(cell));
	cgui_text_draw(context.drawable, DATA->label);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
frame(cgui_cell *cell, struct cgui_box *box)
{
	switch (DATA->state)
	{
		case CGUI_BEACON_CRITICAL:
			*box = DATA->blink_on ? CONFIG->beacon_frame_crit_on : CONFIG->beacon_frame_crit_off;
			break;

		case CGUI_BEACON_ON:
			*box = CONFIG->beacon_frame_on;
			break;

		case CGUI_BEACON_OFF:
		default:
			*box = CONFIG->beacon_frame_off;
			break;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
invalid(const cgui_cell *cell)
{
	if (cell->serial != CELL_BEACON)
	{
		main_set_error(CERR_PARAM);
	}

	return cgui_error() || !cell->valid;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static struct cgui_text
text_style(const cgui_cell *cell)
{
	switch (DATA->state)
	{
		case CGUI_BEACON_CRITICAL:
			return DATA->blink_on ? CONFIG->beacon_text_crit_on : CONFIG->beacon_text_crit_off;

		case CGUI_BEACON_ON:
			return CONFIG->beacon_text_on;

		case CGUI_BEACON_OFF:
		default:
			return CONFIG->beacon_text_off;
	}
}


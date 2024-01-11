/**
 * Copyright © 2024 Frawwlen <fraawlen@posteo.net>
 *
 * This file is part of the Derelict Graphics (DG) GUI library.
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

#include <math.h>
#include <stdbool.h>
#include <stdlib.h>

#include <dg/core/core.h>
#include <dg/core/config.h>
#include <dg/core/errno.h>

#include "public/base.h"
#include "public/config.h"
#include "public/draw.h"
#include "public/origin.h"
#include "public/string.h"
#include "public/zone.h"

#include "private/base.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define _PROPS ((_props_t*)dg_core_cell_get_props(c))
#define _STYLE (&dg_base_config_get()->spinner_style)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

typedef struct {
	dg_base_string_t label;
	dg_base_origin_t label_og;
	double angle;
	bool spinning;
} _props_t;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void _animate (dg_core_cell_t *c, dg_core_cell_drawing_context_t *dc);
static void _destroy (dg_core_cell_t *c);
static void _draw    (dg_core_cell_t *c, dg_core_cell_drawing_context_t *dc);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

dg_core_cell_t *
dg_base_spinner_create(void)
{
	DG_BASE_IS_INIT;

	const unsigned int serial = dg_base_get_type_serial(DG_BASE_SPINNER);

	_props_t *props = malloc(sizeof(_props_t));
	if (!props) {
		dg_core_errno_set(DG_CORE_ERRNO_MEMORY);
		return NULL;
	}

	dg_core_cell_t *c = dg_core_cell_create(serial, _draw, NULL, _destroy, props);
	if (!c) {
		free(props);
		return NULL;
	}

	props->label    = DG_BASE_STRING_EMPTY;
	props->label_og = DG_BASE_ORIGIN_LEFT;
	props->angle    = 0.0;
	props->spinning = true;

	return c;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_spinner_pause(dg_core_cell_t *c)
{
	DG_BASE_IS_INIT;
	DG_BASE_IS_CELL(c, DG_BASE_SPINNER);

	_PROPS->spinning = false;

	dg_core_cell_redraw(c);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_spinner_play(dg_core_cell_t *c)
{
	DG_BASE_IS_INIT;
	DG_BASE_IS_CELL(c, DG_BASE_SPINNER);

	_PROPS->spinning = false;

	dg_core_cell_redraw(c);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_spinner_set_label(dg_core_cell_t *c, const char *str)
{
	DG_BASE_IS_INIT;
	DG_BASE_IS_CELL(c, DG_BASE_SPINNER);

	dg_base_string_set(&_PROPS->label, str);

	dg_core_cell_redraw(c);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_spinner_set_label_origin(dg_core_cell_t *c, dg_base_origin_t og)
{
	DG_BASE_IS_INIT;
	DG_BASE_IS_CELL(c, DG_BASE_SPINNER);
	
	_PROPS->label_og = og;
	
	dg_core_cell_redraw(c);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_spinner_toggle(dg_core_cell_t *c)
{
	DG_BASE_IS_INIT;
	DG_BASE_IS_CELL(c, DG_BASE_SPINNER);

	_PROPS->spinning = !_PROPS->spinning;

	dg_core_cell_redraw(c);
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static void
_animate(dg_core_cell_t *c, dg_core_cell_drawing_context_t *dc)
{
	if (!_PROPS->spinning) {
		return;
	}
	
	_PROPS->angle += _STYLE->anim_speed / 1000000.0 * dc->delay;
	fmod(_PROPS->angle, DG_BASE_DRAW_PI * 2);

	dc->msg |= DG_CORE_CELL_DRAW_MSG_REQUEST_UPDATE;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_destroy(dg_core_cell_t *c)
{
	dg_base_string_clear(&_PROPS->label);
	free(_PROPS);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_draw(dg_core_cell_t *c, dg_core_cell_drawing_context_t *dc)
{
	/* update animation */

	_animate(c, dc);

	/* draw */

	const double x1 = 0.5 + 0.5 * cos(_PROPS->angle);
	const double x2 = 0.5 + 0.5 * cos(_PROPS->angle + DG_BASE_DRAW_PI);
	const double y1 = 0.5 + 0.5 * sin(_PROPS->angle);
	const double y2 = 0.5 + 0.5 * sin(_PROPS->angle + DG_BASE_DRAW_PI);

	dg_base_zone_t zb = dg_base_zone_get_body(dc,  _STYLE);
	dg_base_zone_t zl = dg_base_zone_get_label(dc, _STYLE, true);
	dg_base_zone_t zi = dg_base_zone_get_icon(dc,  _STYLE);

	dg_base_draw_body(&zb,  _STYLE);
	dg_base_draw_label(&zl, _STYLE, &_PROPS->label, _PROPS->label_og);

	dg_base_draw_circle(&zi,  _STYLE->cl_primary, 0.5, 0.5, 0.5,     _STYLE->thick_icon);
	dg_base_draw_segment(&zi, _STYLE->cl_primary, x1,  y1,  x2,  y2, _STYLE->thick_icon);
}

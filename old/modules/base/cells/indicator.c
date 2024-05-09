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

#include <stdbool.h>
#include <stdlib.h>

#include <dg/core/core.h>
#include <dg/core/config.h>
#include <dg/core/errno.h>

#include "../base.h"
#include "../base-private.h"
#include "../config.h"
#include "../draw.h"
#include "../origin.h"
#include "../string.h"
#include "../zone.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define _PROPS ((_props_t*)dg_core_cell_get_props(c))
#define _STYLE (&dg_base_config_get()->indicator_style[_PROPS->state])

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

typedef enum {
	_OFF       = 0,
	_ON        = 1,
	_CRIT_LOW  = 2,
	_CRIT_HIGH = 3,
} _state_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

typedef struct {
	_state_t state;
	dg_base_string_t label;
	dg_base_origin_t label_og;
	bool blink_on;
	void (*fn_blink)(dg_core_cell_t *c, bool crit_on);
	unsigned long anim_count;
	unsigned int  anim_multi;
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
dg_base_indicator_create(void)
{
	DG_BASE_IS_INIT;

	const unsigned int serial = dg_base_get_type_serial(DG_BASE_INDICATOR);

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

	props->state      = _OFF;
	props->label      = DG_BASE_STRING_EMPTY;
	props->label_og   = DG_BASE_ORIGIN_CENTER;
	props->blink_on   = false;
	props->fn_blink   = NULL; 
	props->anim_multi = 1;
	props->anim_count = 0;

	return c;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_indicator_set_callback_blink(dg_core_cell_t *c, void (*fn)(dg_core_cell_t *c, bool crit_on))
{
	DG_BASE_IS_INIT;
	DG_BASE_IS_CELL(c, DG_BASE_INDICATOR);
	
	_PROPS->fn_blink = fn;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_indicator_set_critical(dg_core_cell_t *c, unsigned int blink_multiplier)
{
	DG_BASE_IS_INIT;
	DG_BASE_IS_CELL(c, DG_BASE_INDICATOR);
	
	_PROPS->anim_multi = blink_multiplier;
	if (_PROPS->state != _CRIT_HIGH && _PROPS->state != _CRIT_LOW) {
		_PROPS->state = _CRIT_HIGH;
		_PROPS->anim_count = 0;
	}

	dg_core_cell_redraw(c);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_indicator_set_label(dg_core_cell_t *c, const char *str)
{
	DG_BASE_IS_INIT;
	DG_BASE_IS_CELL(c, DG_BASE_INDICATOR);
	
	dg_base_string_set(&_PROPS->label, str);
	
	dg_core_cell_redraw(c);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_indicator_set_label_origin(dg_core_cell_t *c, dg_base_origin_t og)
{
	DG_BASE_IS_INIT;
	DG_BASE_IS_CELL(c, DG_BASE_INDICATOR);
	
	_PROPS->label_og = og;
	
	dg_core_cell_redraw(c);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_indicator_set_off(dg_core_cell_t *c)
{
	DG_BASE_IS_INIT;
	DG_BASE_IS_CELL(c, DG_BASE_INDICATOR);
	
	_PROPS->state = _OFF;
	
	dg_core_cell_redraw(c);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_indicator_set_on(dg_core_cell_t *c)
{
	DG_BASE_IS_INIT;
	DG_BASE_IS_CELL(c, DG_BASE_INDICATOR);
	
	_PROPS->state = _ON;
	
	dg_core_cell_redraw(c);
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static void
_animate(dg_core_cell_t *c, dg_core_cell_drawing_context_t *dc)
{
	if (_PROPS->state == _ON || _PROPS->state == _OFF) {
		return;
	}

	const bool high = _PROPS->state == _CRIT_HIGH;

	_PROPS->anim_count += dc->delay;
	if (_PROPS->anim_count >= _STYLE->anim_speed * 1000 / _PROPS->anim_multi) {
		_PROPS->anim_count = 0;
		_PROPS->state = high ? _CRIT_LOW : _CRIT_HIGH;
		if (_PROPS->fn_blink) {
			_PROPS->fn_blink(c, high);
		}
	}
	
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
	
	dg_base_zone_t zb = dg_base_zone_get_body(dc, _STYLE);
	dg_base_zone_t zf = dg_base_zone_get_foreground(dc, _STYLE);
	dg_base_zone_t zl = dg_base_zone_get_label(dc, _STYLE, false);

	dg_base_draw_body(&zb,  _STYLE);
	dg_base_draw_fill(&zf,  _STYLE->cl_highlight);
	dg_base_draw_label(&zl, _STYLE, &_PROPS->label, _PROPS->label_og);
}

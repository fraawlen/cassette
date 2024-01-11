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
#include <stdint.h>
#include <stdlib.h>

#include <dg/core/core.h>
#include <dg/core/config.h>
#include <dg/core/errno.h>

#include "public/base.h"
#include "public/config.h"
#include "public/draw.h"
#include "public/origin.h"
#include "public/rotation.h"
#include "public/string.h"
#include "public/zone.h"

#include "private/base.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define _PROPS ((_props_t*)dg_core_cell_get_props(c))
#define _STYLE (&dg_base_config_get()->gauge_style[_PROPS->unknown ? 2 : (_val_ratio(c) >= 1.0 ? 1 : 0)])

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

typedef struct {
	dg_base_string_t units;
	dg_base_string_t label;
	bool show_label;
	bool unknown;
	bool horz;
	double val;
	double min;
	double max;
	int precision;
	double anim_pos;
	int anim_dir;
} _props_t;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void _animate (dg_core_cell_t *c, dg_core_cell_drawing_context_t *dc);
static void _destroy (dg_core_cell_t *c);
static void _draw    (dg_core_cell_t *c, dg_core_cell_drawing_context_t *dc);

static int16_t _bar_length   (dg_core_cell_t *c, dg_core_cell_drawing_context_t *dc);
static void    _update_label (dg_core_cell_t *c);
static double  _val_bound    (dg_core_cell_t *c);
static double  _val_ratio    (dg_core_cell_t *c);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

dg_core_cell_t *
dg_base_gauge_create(void)
{
	DG_BASE_IS_INIT;

	const unsigned int serial = dg_base_get_type_serial(DG_BASE_GAUGE);

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

	props->units      = DG_BASE_STRING_EMPTY;
	props->label      = DG_BASE_STRING_EMPTY;
	props->show_label = true;
	props->unknown    = false;
	props->horz       = true;
	props->val        = 0.0;
	props->min        = 0.0;
	props->max        = 100.0;
	props->precision  = 0;
	props->anim_pos   = 0.0;
	props->anim_dir   = 1;

	dg_base_string_set(&_PROPS->units,    "%");
	dg_base_string_set(&_PROPS->label, "__0%");

	return c;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_gauge_hide_label(dg_core_cell_t *c)
{
	DG_BASE_IS_INIT;
	DG_BASE_IS_CELL(c, DG_BASE_GAUGE);

	_PROPS->show_label = false;

	dg_core_cell_redraw(c);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_gauge_set_horizontal(dg_core_cell_t *c)
{
	DG_BASE_IS_INIT;
	DG_BASE_IS_CELL(c, DG_BASE_GAUGE);

	_PROPS->horz = true;

	dg_core_cell_redraw(c);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_gauge_set_label_style(dg_core_cell_t *c, int precision, const char *units)
{
	DG_BASE_IS_INIT;
	DG_BASE_IS_CELL(c, DG_BASE_GAUGE);

	dg_base_string_set(&_PROPS->units, units);
	_PROPS->precision = precision;

	_update_label(c);
	
	dg_core_cell_redraw(c);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_gauge_set_limits(dg_core_cell_t *c, double min, double max)
{
	DG_BASE_IS_INIT;
	DG_BASE_IS_CELL(c, DG_BASE_GAUGE);

	assert(max > min);

	_PROPS->min = min;
	_PROPS->max = max;

	_update_label(c);

	dg_core_cell_redraw(c);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_gauge_set_value(dg_core_cell_t *c, double value)
{
	DG_BASE_IS_INIT;
	DG_BASE_IS_CELL(c, DG_BASE_GAUGE);

	_PROPS->val     = value;
	_PROPS->unknown = false;

	_update_label(c);

	dg_core_cell_redraw(c);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_gauge_set_value_unknown(dg_core_cell_t *c)
{
	DG_BASE_IS_INIT;
	DG_BASE_IS_CELL(c, DG_BASE_GAUGE);

	_PROPS->unknown = true;

	dg_core_cell_redraw(c);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_gauge_set_vertical(dg_core_cell_t *c)
{
	DG_BASE_IS_INIT;
	DG_BASE_IS_CELL(c, DG_BASE_GAUGE);

	_PROPS->horz = false;

	dg_core_cell_redraw(c);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_gauge_show_label(dg_core_cell_t *c)
{
	DG_BASE_IS_INIT;
	DG_BASE_IS_CELL(c, DG_BASE_GAUGE);

	_PROPS->show_label = true;

	dg_core_cell_redraw(c);
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static void
_animate(dg_core_cell_t *c, dg_core_cell_drawing_context_t *dc)
{
	if (!_PROPS->unknown) {
		return;
	}
	
	const int16_t l = (_PROPS->horz ? dc->cell_pw : dc->cell_ph) - (_STYLE->margin + _STYLE->pad_fg) * 2
	                  - _bar_length(c, dc);
	
	dc->delay %= l * 2000000 / _STYLE->anim_speed;
	_PROPS->anim_pos += _STYLE->anim_speed * ((double)dc->delay / 1000000.0) * _PROPS->anim_dir;
	while (_PROPS->anim_pos < 0 || _PROPS->anim_pos > l) {
		_PROPS->anim_pos  = (_PROPS->anim_pos < 0 ? 0 : l * 2) - _PROPS->anim_pos;
		_PROPS->anim_dir *= -1;
	}
	
	dc->msg |= DG_CORE_CELL_DRAW_MSG_REQUEST_UPDATE;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static int16_t
_bar_length(dg_core_cell_t *c, dg_core_cell_drawing_context_t *dc)
{
	const int16_t a  = dg_core_config_get_cell_width(-1) * 2;
	const int16_t b1 = _STYLE->thick_sep;
	const int16_t b2 = _STYLE->thick_sep - _STYLE->thick_bd - _STYLE->pad_fg
	                   + DG_CORE_CONFIG->win_pad_cell * 2;
	int16_t l;
	int16_t ls;

	if (_PROPS->horz) {
		l  = dc->cell_pw - (_STYLE->margin + _STYLE->pad_fg) * 2;
		ls = dg_core_config_convert_str_width(_PROPS->label.n_cols);
	} else {
		l  = dc->cell_ph - (_STYLE->margin + _STYLE->pad_fg) * 2;
		ls = dg_core_config_convert_str_height(_PROPS->label.n_rows);
	}

	if (_PROPS->unknown) {
		return l < 3 * a ? l / 3 : a;
	} else {
		return (double)(l - (_PROPS->show_label ? ls + b2 : b1)) * _val_ratio(c) + b1;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_destroy(dg_core_cell_t *c)
{
	dg_base_string_clear(&_PROPS->units);
	dg_base_string_clear(&_PROPS->label);
	free(_PROPS);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_draw(dg_core_cell_t *c, dg_core_cell_drawing_context_t *dc)
{	
	dg_base_zone_t zb = dg_base_zone_get_body(dc, _STYLE);
	dg_base_zone_t zl = dg_base_zone_get_label(dc, _STYLE, false);
	dg_base_zone_t zf = dg_base_zone_get_foreground(dc, _STYLE);

	dg_base_rotation_t sep_rot = 0;
	dg_base_origin_t label_og  = 0;

	int16_t sep_py = 0;
	int16_t sep_l  = 0;

	/* update animation */

	_animate(c, dc);

	/* calc geometries */

	const int16_t l1 = _bar_length(c, dc);
	const int16_t l2 = _STYLE->thick_bd;
	const int16_t l3 = _STYLE->pad_fg;
	const int16_t l4 = DG_CORE_CONFIG->win_pad_cell;

	if (_PROPS->unknown) {
		if (_PROPS->horz) {
			zf.px += _PROPS->anim_pos;
			zf.pw  = l1;
		} else {
			zf.py += _PROPS->anim_pos;
			zf.ph  = l1;
		}
	} else {
		if (_PROPS->horz) {
			zf.pw    = l1;
			zl.px    = zf.px       + l1 - l2 + l4;
			zl.pw    = dc->cell_pw - l1 + l2 - l4 * 2 - l3;
			sep_py   = zf.ph;
			sep_l    = zf.ph;
			sep_rot  = DG_BASE_ROTATION_INVERTED;
			label_og = DG_BASE_ORIGIN_LEFT;
		} else {
			zf.py   += zf.ph - l1;
			zf.ph    = l1;
			zl.ph    = dc->cell_ph - l1 + l2 - l4 * 2 - l3;
			sep_py   = 0;
			sep_l    = zf.pw;
			sep_rot  = DG_BASE_ROTATION_LEFT;
			label_og = DG_BASE_ORIGIN_BOTTOM;
		}
	}

	/* draw */

	dg_base_draw_body(&zb, _STYLE);
	dg_base_draw_fill(&zf, _STYLE->cl_primary);
	if (!_PROPS->unknown) {
		dg_base_draw_separator(&zf, _STYLE, zf.pw, sep_py, sep_l, sep_rot);
		if (_PROPS->show_label) {
			dg_base_draw_label(&zl, _STYLE, &_PROPS->label, label_og);
		}
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_update_label(dg_core_cell_t *c)
{
	dg_base_string_t s_val = dg_base_string_convert_double(_val_bound(c), _PROPS->precision);
	dg_base_string_t s_max = dg_base_string_convert_double(_PROPS->max,   _PROPS->precision);
	dg_base_string_t s_min = dg_base_string_convert_double(_PROPS->min,   _PROPS->precision);

	dg_base_string_pad(&s_val, s_max.n_cols > s_min.n_cols ? s_max.n_cols : s_min.n_cols, false);
	dg_base_string_set(&_PROPS->label, s_val.chars);
	dg_base_string_append(&_PROPS->label, _PROPS->units.chars);

	dg_base_string_clear(&s_val);
	dg_base_string_clear(&s_max);
	dg_base_string_clear(&s_min);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static double
_val_bound(dg_core_cell_t *c)
{
	return _PROPS->val > _PROPS->min ? (_PROPS->val < _PROPS->max ? _PROPS->val : _PROPS->max) : _PROPS->min;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static double
_val_ratio(dg_core_cell_t *c)
{
	return (_val_bound(c) - _PROPS->min) / (_PROPS->max - _PROPS->min);
}

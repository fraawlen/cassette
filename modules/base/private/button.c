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
#include <xkbcommon/xkbcommon-keysyms.h>

#include <dg/core/core.h>
#include <dg/core/config.h>
#include <dg/core/errno.h>

#include "public/base.h"
#include "public/config.h"
#include "public/draw.h"
#include "public/origin.h"
#include "public/rotation.h"
#include "public/string.h"
#include "public/util.h"
#include "public/zone.h"

#include "private/base.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define _PROPS ((_props_t*)dg_core_cell_get_props(c))
#define _STYLE (&dg_base_config_get()->button_style[_PROPS->state])
#define _CL(X) dg_base_config_get()->common_cl[X]

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

typedef enum {
	_IDLE     = 0,
	_FOCUSED  = 1,
	_PRESSED  = 2,
	_DISABLED = 3,
} _state_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

typedef struct {
	_state_t state;
	dg_base_string_t label;
	dg_base_origin_t label_og;
	dg_base_button_icon_t icon;
	void (*fn_press)(dg_core_cell_t *c);
	void (*fn_icon)(dg_core_cell_t *c, dg_base_zone_t *z);
} _props_t;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void _destroy (dg_core_cell_t *c);
static void _events  (dg_core_cell_t *c, dg_core_cell_event_t *ev);
static void _draw    (dg_core_cell_t *c, dg_core_cell_drawing_context_t *dc);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const dg_base_draw_point_t link_box[5] = {
	{0.3, 0.0},
	{0.0, 0.0},
	{0.0, 1.0},
	{1.0, 1.0},
	{1.0, 0.7},
};

const dg_base_draw_point_t link_head_in[3] = {
	{0.3, 0.4},
	{0.3, 0.7},
	{0.6, 0.7},
};

const dg_base_draw_point_t link_head_out[3] = {
	{0.7, 0.0},
	{1.0, 0.0},
	{1.0, 0.3},
};

const dg_base_draw_point_t _left[3] = {
	{0.5, 0.0},
	{0.0, 0.5},
	{0.5, 1.0},
};

const dg_base_draw_point_t _right[3] = {
	{0.5, 0.0},
	{1.0, 0.5},
	{0.5, 1.0},
};

const dg_base_draw_point_t _up[3] = {
	{0.0, 0.5},
	{0.5, 0.0},
	{1.0, 0.5},
};

const dg_base_draw_point_t _down[3] = {
	{0.0, 0.5},
	{0.5, 1.0},
	{1.0, 0.5},
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

dg_core_cell_t *
dg_base_button_create(void)
{
	DG_BASE_IS_INIT;

	const unsigned int serial = dg_base_get_type_serial(DG_BASE_BUTTON);

	_props_t *props = malloc(sizeof(_props_t));
	if (!props) {
		dg_core_errno_set(DG_CORE_ERRNO_MEMORY);
		return NULL;
	}

	dg_core_cell_t *c = dg_core_cell_create(serial, _draw, _events, _destroy, props);
	if (!c) {
		free(props);
		return NULL;
	}

	props->state    = _IDLE;
	props->label    = DG_BASE_STRING_EMPTY;
	props->label_og = DG_BASE_ORIGIN_LEFT;
	props->icon     = DG_BASE_BUTTON_ICON_NONE;
	props->fn_press = NULL;
	props->fn_icon  = NULL;

	return c;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_button_set_callback_icon(dg_core_cell_t *c, void (*fn)(dg_core_cell_t *c, dg_base_zone_t *z))
{
	DG_BASE_IS_INIT;
	DG_BASE_IS_CELL(c, DG_BASE_BUTTON);

	_PROPS->fn_icon = fn;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_button_set_callback_pressed(dg_core_cell_t *c, void (*fn)(dg_core_cell_t *c))
{
	DG_BASE_IS_INIT;
	DG_BASE_IS_CELL(c, DG_BASE_BUTTON);

	_PROPS->fn_press = fn;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_button_set_icon(dg_core_cell_t *c, dg_base_button_icon_t icon)
{
	DG_BASE_IS_INIT;
	DG_BASE_IS_CELL(c, DG_BASE_BUTTON);

	_PROPS->icon = icon;
	
	dg_core_cell_redraw(c);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_button_set_label(dg_core_cell_t *c, const char *str)
{
	DG_BASE_IS_INIT;
	DG_BASE_IS_CELL(c, DG_BASE_BUTTON);
	
	dg_base_string_set(&_PROPS->label, str);
	
	dg_core_cell_redraw(c);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_button_set_label_origin(dg_core_cell_t *c, dg_base_origin_t og)
{
	DG_BASE_IS_INIT;
	DG_BASE_IS_CELL(c, DG_BASE_BUTTON);
	
	_PROPS->label_og = og;
	
	dg_core_cell_redraw(c);
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

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
	dg_base_zone_t zb = dg_base_zone_get_body(dc,  _STYLE);
	dg_base_zone_t zl = dg_base_zone_get_label(dc, _STYLE, _PROPS->icon != DG_BASE_BUTTON_ICON_NONE);
	dg_base_zone_t zi = dg_base_zone_get_icon(dc,  _STYLE);
	
	dg_base_draw_body(&zb,  _STYLE);
	dg_base_draw_focus(&zb, _STYLE, dc);
	dg_base_draw_label(&zl, _STYLE, &_PROPS->label, _PROPS->label_og);

	/* if there is no label, center the icon */

	if (_PROPS->label.n_chars == 0) {
		zi.px = dc->cell_px + (dc->cell_pw - zi.pw) / 2;
		zi.py = dc->cell_py + (dc->cell_ph - zi.ph) / 2;
	}

	/* icons */

	switch (_PROPS->icon) {

		case DG_BASE_BUTTON_ICON_NONE:
			break;

		case DG_BASE_BUTTON_ICON_YES:
			dg_base_draw_rectangle(&zi, _CL(DG_BASE_CONFIG_COLOR_GREEN), 0.25, 0.25, 0.75, 0.75, 0);
			dg_base_draw_rectangle(&zi, _STYLE->cl_secondary, 0.25, 0.25, 0.75, 0.75, _STYLE->thick_icon * 2);
			break;

		case DG_BASE_BUTTON_ICON_NO:
			dg_base_draw_rectangle(&zi, _CL(DG_BASE_CONFIG_COLOR_RED), 0.25, 0.25, 0.75, 0.75, 0);
			dg_base_draw_rectangle(&zi, _STYLE->cl_secondary, 0.25, 0.25, 0.75, 0.75, _STYLE->thick_icon * 2);
			break;

		case DG_BASE_BUTTON_ICON_NEUTRAL:
			dg_base_draw_rectangle(&zi, _CL(DG_BASE_CONFIG_COLOR_BRIGHT_BLACK), 0.25, 0.25, 0.75, 0.75, 0);
			dg_base_draw_rectangle(&zi, _STYLE->cl_secondary, 0.25, 0.25, 0.75, 0.75, _STYLE->thick_icon * 2);
			break;

		case DG_BASE_BUTTON_ICON_LINK_IN:
			dg_base_draw_lines(&zi,   _STYLE->cl_primary, link_box,     DG_BASE_DRAW_POINTS_LEN(link_box),      _STYLE->thick_icon);
			dg_base_draw_lines(&zi,   _STYLE->cl_primary, link_head_in, DG_BASE_DRAW_POINTS_LEN(link_head_out), _STYLE->thick_icon);
			dg_base_draw_segment(&zi, _STYLE->cl_primary, 0.3, 0.7, 1.0, 0.0, _STYLE->thick_icon);
			break;

		case DG_BASE_BUTTON_ICON_LINK_OUT:
			dg_base_draw_lines(&zi,   _STYLE->cl_primary, link_box,      DG_BASE_DRAW_POINTS_LEN(link_box),      _STYLE->thick_icon);
			dg_base_draw_lines(&zi,   _STYLE->cl_primary, link_head_out, DG_BASE_DRAW_POINTS_LEN(link_head_out), _STYLE->thick_icon);
			dg_base_draw_segment(&zi, _STYLE->cl_primary, 1.0, 0.0, 0.3, 0.7, _STYLE->thick_icon);
			break;

		case DG_BASE_BUTTON_ICON_CROSS:
			dg_base_draw_segment(&zi, _STYLE->cl_primary, 0.0, 0.0, 1.0, 1.0, _STYLE->thick_icon);
			dg_base_draw_segment(&zi, _STYLE->cl_primary, 0.0, 1.0, 1.0, 0.0, _STYLE->thick_icon);
			break;

		case DG_BASE_BUTTON_ICON_PLUS:
			dg_base_draw_segment(&zi, _STYLE->cl_primary, 0.0, 0.5, 1.0, 0.5, _STYLE->thick_icon);
			dg_base_draw_segment(&zi, _STYLE->cl_primary, 0.5, 0.0, 0.5, 1.0, _STYLE->thick_icon);
			break;

		case DG_BASE_BUTTON_ICON_MINUS:
			dg_base_draw_segment(&zi, _STYLE->cl_primary, 0.0, 0.5, 1.0, 0.5, _STYLE->thick_icon);
			break;

		case DG_BASE_BUTTON_ICON_LEFT:
			dg_base_draw_lines(&zi,   _STYLE->cl_primary, _left, DG_BASE_DRAW_POINTS_LEN(_left), _STYLE->thick_icon);
			dg_base_draw_segment(&zi, _STYLE->cl_primary, 0.0, 0.5, 1.0, 0.5, _STYLE->thick_icon);
			break;

		case DG_BASE_BUTTON_ICON_RIGHT:
			dg_base_draw_lines(&zi,   _STYLE->cl_primary, _right, DG_BASE_DRAW_POINTS_LEN(_right), _STYLE->thick_icon);
			dg_base_draw_segment(&zi, _STYLE->cl_primary, 0.0, 0.5, 1.0, 0.5, _STYLE->thick_icon);
			break;

		case DG_BASE_BUTTON_ICON_UP:
			dg_base_draw_lines(&zi,   _STYLE->cl_primary, _up,  DG_BASE_DRAW_POINTS_LEN(_up), _STYLE->thick_icon);
			dg_base_draw_segment(&zi, _STYLE->cl_primary, 0.5, 0.0, 0.5, 1.0, _STYLE->thick_icon);
			break;

		case DG_BASE_BUTTON_ICON_DOWN:
			dg_base_draw_lines(&zi,   _STYLE->cl_primary, _down,  DG_BASE_DRAW_POINTS_LEN(_down), _STYLE->thick_icon);
			dg_base_draw_segment(&zi, _STYLE->cl_primary, 0.5, 0.0, 0.5, 1.0, _STYLE->thick_icon);
			break;

		case DG_BASE_BUTTON_ICON_CUSTOM:
			if (_PROPS->fn_icon) {
				_PROPS->fn_icon(c, &zi);
			}
			break;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_events(dg_core_cell_t *c, dg_core_cell_event_t *ev)
{
	bool run_callback  = false;
	_state_t old_state = _PROPS->state;

	switch (ev->kind) {

		case DG_CORE_CELL_EVENT_KEY_PRESS:
			if (ev->key_sym == XKB_KEY_Return) {
				_PROPS->state = _PRESSED;
				run_callback = true;
			}
			break;

		case DG_CORE_CELL_EVENT_KEY_RELEASE:
			if (ev->key_sym == XKB_KEY_Return) {
				_PROPS->state = _FOCUSED;
			}
			break;

		case DG_CORE_CELL_EVENT_BUTTON_PRESS:
			if (ev->button_id == 1 && dg_base_util_test_event_bounds(ev, _STYLE)) {
				_PROPS->state = _PRESSED;
			}
			break;

		case DG_CORE_CELL_EVENT_BUTTON_RELEASE:
			if (ev->button_id == 1) {
				_PROPS->state = _FOCUSED;
				run_callback = old_state == _PRESSED && dg_base_util_test_event_bounds(ev, _STYLE);
			}
			break;

		case DG_CORE_CELL_EVENT_TOUCH_BEGIN:
			if (ev->touch_n == 1 && dg_base_util_test_event_bounds(ev, _STYLE)) {
				_PROPS->state = _PRESSED;
			}
			break;

		case DG_CORE_CELL_EVENT_TOUCH_END:
			if (ev->touch_n == 0) {
				_PROPS->state = _FOCUSED;
				run_callback = old_state == _PRESSED && dg_base_util_test_event_bounds(ev, _STYLE);
			}
			break;

		case DG_CORE_CELL_EVENT_FOCUS_GAIN_BY_TOUCH:
		case DG_CORE_CELL_EVENT_FOCUS_GAIN_BY_POINTER:
			_PROPS->state = dg_base_util_test_event_bounds(ev, _STYLE) ? _FOCUSED : _PROPS->state;
			break;

		case DG_CORE_CELL_EVENT_FOCUS_GAIN_BY_ACTION:
		case DG_CORE_CELL_EVENT_FOCUS_GAIN_BY_REFERENCE:
			_PROPS->state = _FOCUSED;
			break;

		case DG_CORE_CELL_EVENT_FOCUS_LOSE:
			_PROPS->state = _IDLE;
			break;

		case DG_CORE_CELL_EVENT_WINDOW_DISABLE:
		case DG_CORE_CELL_EVENT_STATE_DISABLE:
			_PROPS->state = _DISABLED;
			break;

		case DG_CORE_CELL_EVENT_WINDOW_ENABLE:
			_PROPS->state = ev->is_enabled ? _IDLE : _PROPS->state;
			break;

		case DG_CORE_CELL_EVENT_STATE_ENABLE:
			_PROPS->state = _PROPS->state == _DISABLED ? _IDLE : _PROPS->state;
			break;

		case DG_CORE_CELL_EVENT_CANCEL:
			_PROPS->state = _PROPS->state == _PRESSED ? _FOCUSED : _PROPS->state;
			break;

		case DG_CORE_CELL_EVENT_INFO_FOCUSED_CELL:
			dg_base_util_fill_focus_info_event(c, ev, _STYLE);
			break;

		case DG_CORE_CELL_EVENT_FOCUS_LOCK:
		case DG_CORE_CELL_EVENT_FOCUS_UNLOCK:
			ev->msg |= DG_CORE_CELL_EVENT_MSG_REQUEST_UPDATE;
			break;

		default:
			ev->msg |= DG_CORE_CELL_EVENT_MSG_REJECT;
			break;
	}

	if (old_state != _PROPS->state) {
		ev->msg |= DG_CORE_CELL_EVENT_MSG_REQUEST_UPDATE;
		if (run_callback && _PROPS->fn_press) {
			_PROPS->fn_press(c);
		}
	}
}

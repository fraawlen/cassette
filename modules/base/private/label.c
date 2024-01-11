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

#include <cairo/cairo.h>

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
#define _STYLE (&dg_base_config_get()->label_style)
#define _CL(X) dg_base_config_get()->common_cl[X]

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

typedef struct {
	dg_base_string_t label;
	dg_base_origin_t label_og;
	dg_base_rotation_t label_rot;
	dg_base_config_color_t label_cl;
} _props_t;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void _destroy (dg_core_cell_t *c);
static void _draw    (dg_core_cell_t *c, dg_core_cell_drawing_context_t *dc);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static const dg_base_origin_t _new_origins[9][4] = {
	/* NORMAL                       INVERTED                     LEFT ROT                     RIGHT ROT                 */
	{ DG_BASE_ORIGIN_CENTER,       DG_BASE_ORIGIN_CENTER,       DG_BASE_ORIGIN_CENTER,       DG_BASE_ORIGIN_CENTER       }, /* CENTER       */
	{ DG_BASE_ORIGIN_LEFT,         DG_BASE_ORIGIN_RIGHT,        DG_BASE_ORIGIN_BOTTOM,       DG_BASE_ORIGIN_TOP          }, /* LEFT         */
	{ DG_BASE_ORIGIN_RIGHT,        DG_BASE_ORIGIN_LEFT,         DG_BASE_ORIGIN_TOP,          DG_BASE_ORIGIN_BOTTOM       }, /* RIGHT        */
	{ DG_BASE_ORIGIN_TOP,          DG_BASE_ORIGIN_BOTTOM,       DG_BASE_ORIGIN_LEFT,         DG_BASE_ORIGIN_RIGHT        }, /* TOP          */
	{ DG_BASE_ORIGIN_TOP_LEFT,     DG_BASE_ORIGIN_BOTTOM_RIGHT, DG_BASE_ORIGIN_BOTTOM_LEFT,  DG_BASE_ORIGIN_TOP_RIGHT    }, /* TOP LEFT     */
	{ DG_BASE_ORIGIN_TOP_RIGHT,    DG_BASE_ORIGIN_BOTTOM_LEFT,  DG_BASE_ORIGIN_TOP_LEFT,     DG_BASE_ORIGIN_BOTTOM_RIGHT }, /* TOP_RIGHT    */
	{ DG_BASE_ORIGIN_BOTTOM,       DG_BASE_ORIGIN_TOP,          DG_BASE_ORIGIN_RIGHT,        DG_BASE_ORIGIN_LEFT         }, /* BOTTOM       */
	{ DG_BASE_ORIGIN_BOTTOM_LEFT,  DG_BASE_ORIGIN_TOP_RIGHT,    DG_BASE_ORIGIN_BOTTOM_RIGHT, DG_BASE_ORIGIN_TOP_LEFT     }, /* BOTTOM LEFT  */
	{ DG_BASE_ORIGIN_BOTTOM_RIGHT, DG_BASE_ORIGIN_TOP_LEFT,     DG_BASE_ORIGIN_TOP_RIGHT,    DG_BASE_ORIGIN_BOTTOM_LEFT  }, /* BOTTOM RIGHT */
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

dg_core_cell_t *
dg_base_label_create(void)
{
	DG_BASE_IS_INIT;

	const unsigned int serial = dg_base_get_type_serial(DG_BASE_LABEL);

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

	props->label     = DG_BASE_STRING_EMPTY;
	props->label_og  = DG_BASE_ORIGIN_LEFT;
	props->label_rot = DG_BASE_ROTATION_NORMAL;
	props->label_cl  = DG_BASE_CONFIG_COLOR_DEFAULT;

	return c;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_label_set_color(dg_core_cell_t *c, dg_base_config_color_t cl)
{
	DG_BASE_IS_INIT;
	DG_BASE_IS_CELL(c, DG_BASE_LABEL);

	_PROPS->label_cl = cl;
	
	dg_core_cell_redraw(c);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_label_set_label(dg_core_cell_t *c, const char *str)
{
	DG_BASE_IS_INIT;
	DG_BASE_IS_CELL(c, DG_BASE_LABEL);

	dg_base_string_set(&_PROPS->label, str);

	dg_core_cell_redraw(c);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_label_set_origin(dg_core_cell_t *c, dg_base_origin_t og)
{
	DG_BASE_IS_INIT;
	DG_BASE_IS_CELL(c, DG_BASE_LABEL);

	_PROPS->label_og = og;

	dg_core_cell_redraw(c);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_label_set_rotation(dg_core_cell_t *c, dg_base_rotation_t rot)
{
	DG_BASE_IS_INIT;
	DG_BASE_IS_CELL(c, DG_BASE_LABEL);

	_PROPS->label_rot = rot;
	
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
	dg_base_config_style_t style = *_STYLE;

	dg_base_zone_t zb = dg_base_zone_get_body(dc, &style);
	dg_base_zone_t zl = dg_base_zone_get_label(dc, &style, false);

	cairo_matrix_t c_mat;
	double a = 0;

	/* modify style for custom colors */

	if (_PROPS->label_cl != DG_BASE_CONFIG_COLOR_DEFAULT) {
		style.cl_ft = _CL(_PROPS->label_cl);
		style.cl_bg =
			dg_core_color_interpolate(
				_STYLE->cl_bg, 
				_CL(_PROPS->label_cl),
				DG_BASE_CONFIG->label_bg_cl_ratio);
	}
	
	/* transform and rotate label zone */

	const int16_t pw_tmp = zl.pw;

	switch (_PROPS->label_rot) {
		
		case DG_BASE_ROTATION_NORMAL:
			break;

		case DG_BASE_ROTATION_INVERTED:
			a = DG_BASE_DRAW_PI;
			zl.px += zl.pw;
			zl.py += zl.ph;
			break;
			
		case DG_BASE_ROTATION_LEFT:
			a = DG_BASE_DRAW_PI / 2;
			zl.px += zl.pw;
			zl.pw  = zl.ph;
			zl.ph  = pw_tmp;
			break;

		case DG_BASE_ROTATION_RIGHT:
			a = -DG_BASE_DRAW_PI / 2;
			zl.py += zl.ph;
			zl.pw  = zl.ph;
			zl.ph  = pw_tmp;
			break;
	}

	/* draw body then label inside modified zone and matrix */

	dg_base_draw_body(&zb, &style);

	cairo_get_matrix(zl.c_ctx, &c_mat);

	cairo_translate(zl.c_ctx, zl.px, zl.py);
	cairo_rotate(zl.c_ctx, a);
	cairo_translate(zl.c_ctx, -zl.px, -zl.py);
		dg_base_draw_label(&zl, &style, &_PROPS->label, _new_origins[_PROPS->label_og][_PROPS->label_rot]);
	cairo_set_matrix(zl.c_ctx, &c_mat);
}

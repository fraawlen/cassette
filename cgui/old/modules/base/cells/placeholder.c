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

#include <stdlib.h>

#include <dg/core/core.h>

#include "../base.h"
#include "../base-private.h"
#include "../config.h"
#include "../draw.h"
#include "../zone.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define _STYLE (&dg_base_config_get()->placeholder_style)

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void _draw (dg_core_cell_t *c, dg_core_cell_drawing_context_t *dc);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

dg_core_cell_t *
dg_base_placeholder_create(void)
{
	DG_BASE_IS_INIT;

	const unsigned int serial = dg_base_get_type_serial(DG_BASE_PLACEHOLDER);

	return dg_core_cell_create(serial, _draw, NULL, NULL, NULL);
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static void
_draw(dg_core_cell_t *c, dg_core_cell_drawing_context_t *dc)
{
	dg_base_zone_t zb = dg_base_zone_get_body(dc, _STYLE);
	dg_base_zone_t zf = dg_base_zone_get_foreground(dc, _STYLE);

	dg_base_draw_body(&zb, _STYLE);

	dg_base_zone_clip(&zf);
	dg_base_draw_segment(&zf, _STYLE->cl_sep, 0.0, 0.0, 1.0, 1.0, _STYLE->thick_sep);
	dg_base_draw_segment(&zf, _STYLE->cl_sep, 1.0, 0.0, 0.0, 1.0, _STYLE->thick_sep);
	dg_base_zone_unclip(&zf);
}


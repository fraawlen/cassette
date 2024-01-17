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

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#include <dg/core/core.h>
#include <dg/core/util.h>

#include "config.h"
#include "util.h"

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
dg_base_util_fill_focus_info_event(dg_core_cell_t *c, dg_core_cell_event_t *ev,
                                   const dg_base_config_style_t *style)
{
	assert(c && ev && style);
	assert(ev->kind == DG_CORE_CELL_EVENT_INFO_FOCUSED_CELL);

	ev->info_focus_cell = c;
	ev->info_focus_px = ev->cell_px + style->margin;
	ev->info_focus_py = ev->cell_py + style->margin;
	ev->info_focus_pw = ev->cell_pw - style->margin * 2;
	ev->info_focus_ph = ev->cell_ph - style->margin * 2;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
dg_base_util_test_event_bounds(const dg_core_cell_event_t *ev, const dg_base_config_style_t *style)
{
	assert(ev && style);

	const int16_t cell_px = ev->cell_px + style->margin;
	const int16_t cell_py = ev->cell_py + style->margin;
	const int16_t cell_pw = ev->cell_pw - style->margin * 2;
	const int16_t cell_ph = ev->cell_ph - style->margin * 2;

	int16_t px = 0;
	int16_t py = 0;

	switch (ev->kind) {
		
		case DG_CORE_CELL_EVENT_BUTTON_PRESS:
		case DG_CORE_CELL_EVENT_BUTTON_RELEASE:
			px = ev->button_px;
			py = ev->button_py;
			break;

		case DG_CORE_CELL_EVENT_POINTER_HOVER:
		case DG_CORE_CELL_EVENT_POINTER_DRAG:
			px = ev->pointer_px;
			py = ev->pointer_py;
			break;

		case DG_CORE_CELL_EVENT_TOUCH_BEGIN:
		case DG_CORE_CELL_EVENT_TOUCH_END:
		case DG_CORE_CELL_EVENT_TOUCH_UPDATE:
			px = ev->touch_px;
			py = ev->touch_py;
			break;

		case DG_CORE_CELL_EVENT_FOCUS_GAIN_BY_POINTER:
		case DG_CORE_CELL_EVENT_FOCUS_GAIN_BY_TOUCH:
			px = ev->focus_px;
			py = ev->focus_py;
			break;

		default:
			return false;
	}

	return dg_core_util_test_bounds(px, py, cell_px, cell_py, cell_pw, cell_ph);
}

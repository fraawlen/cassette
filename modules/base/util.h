/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
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

#ifndef DG_BASE_UTIL_H
#define DG_BASE_UTIL_H

#include <stdbool.h>

#include <dg/core/core.h>

#include "config.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/**
 * Automatically fill the fields of a DG_CORE_CELL_EVENT_INFO_FOCUSED_CELL by taking in account the margin of
 * the given cell style.
 * Only uselful for non meta-cells.
 *
 * @param c     : cell on which the event happens
 * @param ev    : event to process
 * @param style : style to use
 */
void dg_base_util_fill_focus_info_event(dg_core_cell_t *c, dg_core_cell_event_t *ev,
                                        const dg_base_config_style_t *style);

/**
 * Test wether a cell input event is within the bounds of the cell. Only events that carry coordinate data
 * like pointer, button, touch and focus events are valid. Takes the margin from the given style into account.
 * Only uselful for non meta-cells.
 *
 * @param ev    : event to test
 * @param style : style to use
 *
 * @return : true if the input is whitin bounds, false otherwhise. Also false if the event is not of a valid
 *           kind.
 */
bool dg_base_util_test_event_bounds(const dg_core_cell_event_t *ev, const dg_base_config_style_t *style);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DG_BASE_UTIL_H */

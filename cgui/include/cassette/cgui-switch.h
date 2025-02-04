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

#pragma once

#include "cgui-attributes.h"
#include "cgui-cell.h"
#include "cgui-types.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/* CONSTRUCTORS / DESTRUCTORS *******************************************************************************/
/************************************************************************************************************/

/**
 *
 */
cgui_cell *
cgui_switch_create(void)
CGUI_NONNULL_RETURN;

/************************************************************************************************************/
/* IMPURE METHODS *******************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
void
cgui_switch_align_label(cgui_cell *cell, enum cgui_align alignment)
CGUI_NONNULL(1);

/**
 *
 */
void
cgui_switch_disable(cgui_cell *cell)
CGUI_NONNULL(1);

/**
 *
 */
void
cgui_switch_enable(cgui_cell *cell)
CGUI_NONNULL(1);

/**
 *
 */
void
cgui_switch_on_toggle(cgui_cell *cell, void (*fn)(cgui_cell *cell))
CGUI_NONNULL(1);

/**
 *
 */
void
cgui_switch_rotate_label(cgui_cell *cell, enum cgui_rotation rotation)
CGUI_NONNULL(1);

/**
 *
 */
void
cgui_switch_set_label(cgui_cell *cell, const char *label)
CGUI_NONNULL(1, 2);

/**
 *
 */
void
cgui_switch_toggle(cgui_cell *cell)
CGUI_NONNULL(1);

/**
 *
 */
void
cgui_switch_toggle_state(cgui_cell *cell)
CGUI_NONNULL(1);

/**
 *
 */
void
cgui_switch_turn_off(cgui_cell *cell)
CGUI_NONNULL(1);

/**
 *
 */
void
cgui_switch_turn_on(cgui_cell *cell)
CGUI_NONNULL(1);

/************************************************************************************************************/
/* PURE METHODS *********************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
bool
cgui_switch_on(const cgui_cell *cell)
CGUI_NONNULL(1);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif


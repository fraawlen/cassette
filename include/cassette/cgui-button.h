/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Graphics (CGUI) library.
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

#pragma once

#include "cgui-attributes.h"
#include "cgui-cell.h"

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
cgui_button_create(void)
CGUI_NONNULL_RETURN;

/************************************************************************************************************/
/* IMPURE METHODS *******************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
void
cgui_button_disable(cgui_cell *cell)
CGUI_NONNULL(1);

/**
 *
 */
void
cgui_button_enable(cgui_cell *cell)
CGUI_NONNULL(1);

/**
 *
 */
void
cgui_button_on_click(cgui_cell *cell, void (*fn)(cgui_cell *cell))
CGUI_NONNULL(1);


/**
 *
 */
void
cgui_button_set_label(cgui_cell *cell, const char *label)
CGUI_NONNULL(1, 2);

/**
 *
 */
void
cgui_button_toggle(cgui_cell *cell)
CGUI_NONNULL(1);

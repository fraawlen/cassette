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

#include <stdbool.h>

#include "cgui-attributes.h"
#include "cgui-cell.h"

/************************************************************************************************************/
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

#define CGUI_CLIPBOARDS 3

/************************************************************************************************************/
/* IMPURE METHODS *******************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
void
cgui_clipboard_copy(int id, const char *str)
CGUI_NONNULL(2);

/**
 *
 */
void
cgui_clipboard_clear(int id);

/**
 *
 */
void
cgui_clipboard_on_copy(int id, void (*fn)(int id));

/**
 *
 */
void
cgui_clipboard_on_lose(int id, void (*fn)(int id));

/**
 *
 */
void
cgui_clipboard_pair_cell(int id, cgui_cell *cell)
CGUI_NONNULL(2);

/**
 *
 */
const char *
cgui_clipboard_paste(int id, size_t *length)
CGUI_NONNULL_RETURN;

/************************************************************************************************************/
/* PURE METHODS *********************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
bool
cgui_clipboard_owned(int id)
CGUI_PURE;

/**
 *
 */
cgui_cell *
cgui_clipboard_paired_cell(int id)
CGUI_NONNULL_RETURN
CGUI_PURE;


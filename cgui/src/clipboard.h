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

#include <cassette/cgui.h>
#include <stdbool.h>
#include <stdlib.h>
#include <xcb/xcb.h>

/************************************************************************************************************/
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

struct clipboard
{
	xcb_timestamp_t time;
	bool owned;
	char *data;
	size_t data_n;
	void (*fn_copy)(int);
	void (*fn_lose)(int);
	cgui_cell *cell;
};

/************************************************************************************************************/
/* IMPURE METHODS *******************************************************************************************/
/************************************************************************************************************/

void
clipboard_clear(int id)
CGUI_HIDDEN;

/************************************************************************************************************/
/* PURE METHODS *********************************************************************************************/
/************************************************************************************************************/

struct clipboard
clipboard_get(int id)
CGUI_PURE
CGUI_HIDDEN;

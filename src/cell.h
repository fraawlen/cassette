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

/************************************************************************************************************/
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

struct cgui_cell
{
	/* data */

	void *data;

	/* callbacks */

	void (*fn_destroy) (cgui_cell *cell);
	void (*fn_draw)    (cgui_cell *cell, struct cgui_cell_context *context);
	void (*fn_event)   (cgui_cell *cell, struct cgui_cell_event *event);

	/* states */

	bool valid;
};

/************************************************************************************************************/
/* CONSTRUCTORS / DESTRUCTORS *******************************************************************************/
/************************************************************************************************************/

void
cell_destroy(cgui_cell *cell)
CGUI_NONNULL(1);


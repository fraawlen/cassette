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

struct grid_line
{
	int16_t size;
	double flex;
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct cgui_grid
{
	/* data */

	size_t n_cols;
	size_t n_rows;
	double total_col_flex;
	double total_row_flex;
	int16_t total_width;
	int16_t total_width_inv;
	int16_t total_height;
	int16_t total_height_inv;
	struct grid_line *cols;
	struct grid_line *rows;
	cref *areas;
	cgui_grid *ref;

	/* states */

	bool to_destroy;
	enum cgui_grid_err err;
};

/************************************************************************************************************/
/* CONSTRUCTORS / DESTRUCTORS *******************************************************************************/
/************************************************************************************************************/

void
grid_destroy(cgui_grid *grid)
CGUI_NONNULL(1);


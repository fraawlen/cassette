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

#include <cairo/cairo.h>
#include <cassette/cgui.h>
#include <stdbool.h>

/************************************************************************************************************/
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

struct grid_area
{
	cgui_cell *cell; /* hosted cell          */
	size_t col;      /* start col            */
	size_t row;      /* start row            */
	size_t n_cols;   /* width  in cols       */
	size_t n_rows;   /* height in rows       */
	double x;        /* pixel x offset cache */
	double y;        /* pixel y offset cache */
	double width;    /* pixel width  cache   */
	double height;   /* pixel height cache   */
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct grid_line
{
	double offset; /* pixel offset cache */
	double size;   /* pixel length cache */
	double flex;   /* flex factor        */
	ssize_t units; /* amount of chars    */
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct cgui_grid
{
	/* data */

	size_t n_cols;
	size_t n_rows;
	size_t width_units;
	size_t width_units_inv;
	size_t height_units;
	size_t height_units_inv;
	double col_flex;
	double row_flex;
	struct grid_line *cols;
	struct grid_line *rows;
	cref *areas;
	cgui_grid *ref;

	/* states */

	bool valid;
	bool used;
};

/************************************************************************************************************/
/* GLOBALS **************************************************************************************************/
/************************************************************************************************************/

#define GRID_AREA_PLACEHOLDER \
{ \
	.cell   = CGUI_CELL_PLACEHOLDER, \
	.col    = 0,   \
	.row    = 0,   \
	.n_cols = 0,   \
	.n_rows = 0,   \
	.x      = 0.0, \
	.y      = 0.0, \
	.width  = 0.0, \
	.height = 0.0, \
}

/************************************************************************************************************/
/* CONSTRUCTORS / DESTRUCTORS *******************************************************************************/
/************************************************************************************************************/

void
grid_destroy(cgui_grid *grid)
CGUI_NONNULL(1);

/************************************************************************************************************/
/* IMPURE METHODS *******************************************************************************************/
/************************************************************************************************************/

void
grid_repair(cgui_grid *grid)
CGUI_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
grid_update_geometry(cgui_grid *grid, double width, double height)
CGUI_NONNULL(1);

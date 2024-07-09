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

#include <cassette/cgui.h>
#include <cassette/cobj.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "area.h"
#include "cell.h"
#include "main.h"
#include "grid.h"
#include "safe.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

cgui_grid cgui_grid_placeholder_instance =
{
	.to_destroy = false,
	.err        = CGUI_GRID_INVALID,
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cgui_grid_assign_cell(cgui_grid *grid, cgui_cell *cell, size_t x, size_t y, size_t width, size_t height)
{
	struct area *area;

	if (grid->err
	 || cell->err
	 || width  == 0
	 || height == 0
	 || grid->n_cols < width
	 || grid->n_rows < height
	 || grid->n_cols - width  > x
	 || grid->n_rows - height > y
	 || !(area = malloc(sizeof(struct area))))
	{
		return;
	}

	area->x      = x;
	area->y      = y;
	area->width  = width;
	area->height = height;
	area->cell   = cell;

	cref_push(grid->areas, area);

	grid->err |= cref_error(grid->areas) & CREF_OVERFLOW ? CGUI_GRID_OVERFLOW : CGUI_GRID_OK;
	grid->err |= cref_error(grid->areas) & CREF_MEMORY   ? CGUI_GRID_MEMORY   : CGUI_GRID_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgui_grid *
cgui_grid_clone(const cgui_grid *grid)
{
	cgui_grid *grid_new;
	const struct area *area;

	if (grid->err || !(grid_new = malloc(sizeof(cgui_grid))))
	{
		goto fail_grid;
	}

	if (!(grid_new->cols = malloc(grid->n_cols * sizeof(struct grid_line))))
	{
		goto fail_cols;
	}

	if (!(grid_new->rows = malloc(grid->n_rows * sizeof(struct grid_line))))
	{
		goto fail_rows;
	}

	if ((grid_new->areas = cref_create()) == CREF_PLACEHOLDER)
	{
		goto fail_areas;
	}

	memcpy(grid_new->cols, grid->cols, grid->n_cols * sizeof(struct grid_line));
	memcpy(grid_new->rows, grid->rows, grid->n_rows * sizeof(struct grid_line));

	grid_new->n_cols           = grid->n_cols;
	grid_new->n_rows           = grid->n_rows;
	grid_new->total_col_flex   = grid->total_col_flex;
	grid_new->total_row_flex   = grid->total_row_flex;
	grid_new->total_width      = grid->total_width;
	grid_new->total_width_inv  = grid->total_width_inv;
	grid_new->total_height     = grid->total_height;
	grid_new->total_height_inv = grid->total_height_inv; 
	grid_new->ref              = grid->ref;
	grid_new->to_destroy       = false;
	grid_new->err              = CGUI_GRID_OK;
	
	cref_push(main_grids(), grid_new);

	CREF_FOR_EACH(grid->areas, i)
	{
		area = (const struct area*)cref_ptr(grid->areas, i);
		cgui_grid_assign_cell(grid_new, area->cell, area->x, area->y, area->width, area->height);
	}

	if (grid->err)
	{
		grid_new->to_destroy = true;
		grid_destroy(grid_new);
	}

	return grid_new;

	/* errors */

fail_areas:
	free(grid_new->rows);
fail_rows:
	free(grid_new->cols);
fail_cols:
	free(grid_new);
fail_grid:
	return CGUI_GRID_PLACEHOLDER;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cgui_grid_relative_flex
cgui_grid_compare_flex(const cgui_grid *grid_1, const cgui_grid *grid_2)
{
	bool x1;
	bool x2;
	bool y1;
	bool y2;

	if (grid_1->err || grid_2->err)
	{
		return CGUI_GRID_FLEX_INVALID;
	}

	x1 = grid_1->total_col_flex > 0.0;
	x2 = grid_2->total_col_flex > 0.0;
	y1 = grid_1->total_row_flex > 0.0;
	y2 = grid_2->total_row_flex > 0.0;

	if ((x1 == x2) && (y1 == y2))
	{
		return CGUI_GRID_FLEX_SAME;
	}
	else
	{
		return CGUI_GRID_FLEX_DIFFERENT;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cgui_grid_relative_size
cgui_grid_compare_size(const cgui_grid *grid_1, const cgui_grid *grid_2)
{
	enum cgui_grid_relative_size x;
	enum cgui_grid_relative_size y;

	if (grid_1->err || grid_2->err)
	{
		return CGUI_GRID_SIZE_INVALID;
	}

	/* compare x axis */

	if (grid_1->n_cols          == grid_2->n_cols
	 && grid_1->total_width     == grid_2->total_width
	 && grid_1->total_width_inv == grid_2->total_width_inv)
	{
		x = CGUI_GRID_SIZE_EQUAL;
	}
	else if (
	    grid_1->n_cols          >= grid_2->n_cols
	 && grid_1->total_width     >= grid_2->total_width
	 && grid_1->total_width_inv >= grid_2->total_width_inv)
	{
		x = CGUI_GRID_SIZE_BIGGER;
	}
	else if (
	    grid_1->n_cols          <= grid_2->n_cols
	 && grid_1->total_width     <= grid_2->total_width
	 && grid_1->total_width_inv <= grid_2->total_width_inv)
	{
		x = CGUI_GRID_SIZE_SMALLER;
	}
	else
	{
		x = CGUI_GRID_SIZE_UNDEFINED;
	}

	/* compare y axis */

	if (grid_1->n_rows           == grid_2->n_rows
	 && grid_1->total_height     == grid_2->total_height
	 && grid_1->total_height_inv == grid_2->total_height_inv)
	{
		y = CGUI_GRID_SIZE_EQUAL;
	}
	else if (
	    grid_1->n_rows           >= grid_2->n_rows
	 && grid_1->total_height     >= grid_2->total_height
	 && grid_1->total_height_inv >= grid_2->total_height_inv)
	{
		y = CGUI_GRID_SIZE_BIGGER;
	}
	else if (
	    grid_1->n_rows           <= grid_2->n_rows
	 && grid_1->total_height     <= grid_2->total_height
	 && grid_1->total_height_inv <= grid_2->total_height_inv)
	{
		y = CGUI_GRID_SIZE_SMALLER;
	}
	else
	{
		y = CGUI_GRID_SIZE_UNDEFINED;
	}

	/* returns */

	if (x == CGUI_GRID_SIZE_EQUAL && y == CGUI_GRID_SIZE_EQUAL)
	{
		return CGUI_GRID_SIZE_EQUAL;
	}
	
	if ((x == CGUI_GRID_SIZE_BIGGER && y == CGUI_GRID_SIZE_BIGGER)
	 || (x == CGUI_GRID_SIZE_BIGGER && y == CGUI_GRID_SIZE_EQUAL)
	 || (x == CGUI_GRID_SIZE_EQUAL  && y == CGUI_GRID_SIZE_BIGGER))
	{
		return CGUI_GRID_SIZE_BIGGER;
	}

	if ((x == CGUI_GRID_SIZE_SMALLER && y == CGUI_GRID_SIZE_SMALLER)
	 || (x == CGUI_GRID_SIZE_SMALLER && y == CGUI_GRID_SIZE_EQUAL)
	 || (x == CGUI_GRID_SIZE_EQUAL   && y == CGUI_GRID_SIZE_SMALLER))
	{
		return CGUI_GRID_SIZE_SMALLER;
	}

	return CGUI_GRID_SIZE_UNDEFINED;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgui_grid *
cgui_grid_create(size_t cols, size_t rows)
{
	cgui_grid *grid;

	if (!cgui_is_init()
	 || cgui_error()
	 || cols == 0
	 || rows == 0
	 || cols > INT16_MAX
	 || rows > INT16_MAX
	 || !safe_mul(NULL, cols, sizeof(struct grid_line))
	 || !safe_mul(NULL, rows, sizeof(struct grid_line))
	 || !(grid = malloc(sizeof(cgui_grid))))
	{
		goto fail_grid;
	}

	if (!(grid->cols = malloc(cols * sizeof(struct grid_line))))
	{
		goto fail_cols;
	}

	if (!(grid->rows = malloc(rows * sizeof(struct grid_line))))
	{
		goto fail_rows;
	}

	if ((grid->areas = cref_create()) == CREF_PLACEHOLDER)
	{
		goto fail_areas;
	}

	for (size_t i = 0; i < cols; i++)
	{
		grid->cols[i].size = -1;
		grid->cols[i].flex = 0.0;
	}

	for (size_t i = 0; i < rows; i++)
	{
		grid->rows[i].size = 1;
		grid->rows[i].flex = 0.0;
	}

	grid->n_cols           = cols;
	grid->n_rows           = rows;
	grid->total_col_flex   = 0.0;
	grid->total_row_flex   = 0.0;
	grid->total_width      = 0;
	grid->total_width_inv  = cols;
	grid->total_height     = rows;
	grid->total_height_inv = 0; 
	grid->ref              = CGUI_GRID_PLACEHOLDER;
	grid->to_destroy       = false;
	grid->err              = CGUI_GRID_OK;

	cref_push(main_grids(), grid);

	return grid;

	/* errors */

fail_areas:
	free(grid->rows);
fail_rows:
	free(grid->cols);
fail_cols:
	free(grid);
fail_grid:
	return CGUI_GRID_PLACEHOLDER;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_grid_destroy(cgui_grid *grid)
{
	if (grid == CGUI_GRID_PLACEHOLDER)
	{
		return;
	}

	grid->to_destroy = true;
	if (!cgui_is_running())
	{
		grid_destroy(grid);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cgui_grid_err
cgui_grid_error(const cgui_grid *grid)
{
	return grid->err;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

double
cgui_grid_flex_horizontal(const cgui_grid *grid)
{
	if (grid->err)
	{
		return 0.0;
	}

	return grid->total_col_flex;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

double
cgui_grid_flex_vertical(const cgui_grid *grid)
{
	if (grid->err)
	{
		return 0.0;
	}

	return grid->total_row_flex;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

uint16_t
cgui_grid_min_height(const cgui_grid *grid)
{
	struct cline h = { .origin = 0, .length = 0, .min = INT16_MIN, .max = INT16_MAX};

	if (grid->err)
	{
		return 0;
	}

	// TODO

	return h.length;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

uint16_t
cgui_grid_min_width(const cgui_grid *grid)
{
	struct cline w = { .origin = 0, .length = 0, .min = INT16_MIN, .max = INT16_MAX};

	if (grid->err)
	{
		return 0;
	}

	// TODO

	return w.length;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_grid_repair(cgui_grid *grid)
{
	grid->err &= CGUI_GRID_INVALID;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_grid_resize_col(cgui_grid *grid, size_t col, int16_t width)
{
	struct cline w = { .origin = 0, .length = 0, .min = INT16_MIN, .max = INT16_MAX};

	if (grid->err || col >= grid->n_cols)
	{
		return;
	}

	if (grid->cols[col].size > 0)
	{
		grid->total_width -= grid->cols[col].size;
	}
	else
	{
		grid->total_width_inv += grid->cols[col].size;
	}

	if (width > 0)
	{
		w.length = grid->total_width;
		cline_grow(&w, width);
		grid->total_width = w.length;
	}
	else
	{
		w.length = grid->total_width_inv;
		cline_grow(&w, -width);
		grid->total_width_inv = w.length;
	}

	grid->cols[col].size = width;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_grid_resize_row(cgui_grid *grid, size_t row, int16_t height)
{
	struct cline h = { .origin = 0, .length = 0, .min = INT16_MIN, .max = INT16_MAX};

	if (grid->err || row >= grid->n_rows)
	{
		return;
	}

	if (grid->rows[row].size > 0)
	{
		grid->total_height -= grid->rows[row].size;
	}
	else
	{
		grid->total_height_inv += grid->rows[row].size;
	}

	if (height > 0)
	{
		h.length = grid->total_height;
		cline_grow(&h, height);
		grid->total_height = h.length;
	}
	else
	{
		h.length = grid->total_height_inv;
		cline_grow(&h, -height);
		grid->total_height_inv = h.length;
	}

	grid->rows[row].size = height;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_grid_set_col_flex(cgui_grid *grid, size_t col, double flex)
{
	if (grid->err || col >= grid->n_cols || flex < 0.0)
	{
		return;
	}

	grid->total_col_flex += flex - grid->cols[col].flex;
	grid->cols[col].flex  = flex;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_grid_set_reference(cgui_grid *grid, cgui_grid *grid_ref)
{
	if (grid->err
	 || grid_ref->err
	 || cgui_grid_compare_size(grid, grid_ref) != CGUI_GRID_SIZE_EQUAL
	 || cgui_grid_compare_flex(grid, grid_ref) != CGUI_GRID_FLEX_SAME)
	{
		return;
	}

	grid->ref = grid_ref;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_grid_set_row_flex(cgui_grid *grid, size_t row, double flex)
{
	if (grid->err || row >= grid->n_rows || flex < 0.0)
	{
		return;
	}

	grid->total_row_flex += flex - grid->rows[row].flex;
	grid->rows[row].flex  = flex;
}

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

void
grid_destroy(cgui_grid *grid)
{
	if (!grid->to_destroy)
	{
		return;
	}

	CREF_FOR_EACH(grid->areas, i)
	{
		free((void*)cref_ptr(grid->areas, i));
	}

	cref_pull(main_grids(), grid);
	cref_destroy(grid->areas);

	free(grid->cols);
	free(grid->rows);
	free(grid);
}

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

#include <assert.h>
#include <limits.h>
#include <stdbool.h>
#include <stdlib.h>

#include <cassette/cgui.h>
#include <cassette/cobj.h>

#include "area.h"
#include "cell.h"
#include "grid.h"
#include "main.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static cgui_grid_t _err_grid =
{
	.id               = 0,
	.to_destroy       = false,
	.failed           = true,
	.n_cols           = SIZE_MAX,
	.n_rows           = SIZE_MAX,
	.total_col_flex   = 0.0,
	.total_row_flex   = 0.0,
	.total_width      = 0,
	.total_width_inv  = 0,
	.total_height     = 0,
	.total_height_inv = 0,
	.rows             = NULL,
	.cols             = NULL,
	.areas            = NULL,
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cgui_grid_assign_cell(cgui_grid_t *grid, cgui_cell_t *cell, size_t x, size_t y, size_t width, size_t height)
{
	area_t *area;

	assert(grid);
	assert(cell);
	assert(width > 0 && height > 0);
	assert(grid->n_cols >= width  && grid->n_cols - width  >= x);
	assert(grid->n_rows >= height && grid->n_rows - height >= y);

	if (grid->failed || cell->failed)
	{
		return;
	}

	if (!(area = malloc(sizeof(area_t))))
	{
		grid->failed = true;
		return;
	}

	area->x      = x;
	area->y      = y;
	area->width  = width;
	area->height = height;
	area->cell   = cell;

	cobj_tracker_push(grid->areas, area, NULL);
	grid->failed = cobj_tracker_has_failed(grid->areas);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgui_grid_t *
cgui_grid_clone(cgui_grid_t *grid)
{
	cgui_grid_t *clone;
	area_t *area;

	assert(grid);

	if (grid->failed)
	{
		return &_err_grid;
	}

	clone = cgui_grid_create(grid->n_cols, grid->n_rows);

	for (size_t i = 0; i < grid->n_cols; i++)
	{
		cgui_grid_set_col_width(clone, i, grid->cols[i].size);
		cgui_grid_set_col_flex (clone, i, grid->cols[i].flex);
	}

	for (size_t i = 0; i < grid->n_rows; i++)
	{
		cgui_grid_set_row_height(clone, i, grid->rows[i].size);
		cgui_grid_set_row_flex  (clone, i, grid->rows[i].flex);
	}

	cobj_tracker_reset_iterator(grid->areas);
	while (cobj_tracker_increment_iterator(grid->areas))
	{
		area = (area_t*)cobj_tracker_get_iteration(grid->areas);
		cgui_grid_assign_cell(clone, area->cell, area->x, area->y, area->width, area->height);
	}

	return clone;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgui_grid_t *
cgui_grid_create(size_t n_cols, size_t n_rows)
{
	cgui_grid_t *grid;

	assert(n_cols > 0 && n_rows > 0);

	if (cgui_has_failed())
	{
		return &_err_grid;
	}

	if (n_cols > INT_MAX || n_rows > INT_MAX)
	{
		return &_err_grid;
	}

	if (n_cols > SIZE_MAX / sizeof(grid_line_t) || n_rows > SIZE_MAX / sizeof(grid_line_t))
	{
		return &_err_grid;
	}

	if (!(grid = malloc(sizeof(cgui_grid_t))))
	{
		return &_err_grid;
	}

	grid->id               = 0;
	grid->to_destroy       = false;
	grid->failed           = false;
	grid->n_cols           = n_cols;
	grid->n_rows           = n_rows;
	grid->total_col_flex   = 0.0;
	grid->total_row_flex   = 0.0;
	grid->total_width      = 0;
	grid->total_width_inv  = n_cols;
	grid->total_height     = n_rows;
	grid->total_height_inv = 0;
	grid->cols             = malloc(n_cols * sizeof(grid_line_t));
	grid->rows             = malloc(n_rows * sizeof(grid_line_t));
	grid->areas            = cobj_tracker_create(1);

	cobj_tracker_push(main_get_grids(), grid, &grid->id);

	grid->failed |= cobj_tracker_has_failed(grid->areas);
	grid->failed |= !grid->cols;
	grid->failed |= !grid->rows;	

	if (!grid->failed)
	{
		for (size_t i = 0; i < n_cols; i++)
		{
			grid->cols[i].size = -1;
			grid->cols[i].flex = 0.0;
		}

		for (size_t i = 0; i < n_rows; i++)
		{
			grid->rows[i].size = 1;
			grid->rows[i].flex = 0.0;
		}
	}

	return grid;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_grid_destroy(cgui_grid_t **grid)
{
	assert(grid && *grid);

	if (*grid == &_err_grid)
	{
		return;
	}

	(*grid)->to_destroy = true;
	if (!cgui_is_running())
	{
		grid_destroy(*grid);
	}

	*grid = &_err_grid;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgui_grid_t *
cgui_grid_get_placeholder(void)
{
	return &_err_grid;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgui_grid_has_failed(const cgui_grid_t *grid)
{
	assert(grid);

	return grid->failed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_grid_set_col_flex(cgui_grid_t *grid, size_t col, double flex)
{
	assert(grid);
	assert(grid->n_cols > col);
	assert(flex >= 0.0);

	if (grid->failed)
	{
		return;
	}

	grid->total_col_flex += flex - grid->cols[col].flex;
	grid->cols[col].flex  = flex;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_grid_set_col_width(cgui_grid_t *grid, size_t col, int width)
{
	assert(grid);
	assert(grid->n_cols > col);

	if (grid->failed)
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
		grid->total_width += width;
	}
	else
	{
		grid->total_width_inv -= width;
	}

	grid->cols[col].size = width;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_grid_set_row_flex(cgui_grid_t *grid, size_t row, double flex)
{
	assert(grid);
	assert(grid->n_rows > row);
	assert(flex >= 0.0);

	if (grid->failed)
	{
		return;
	}

	grid->total_row_flex += flex - grid->rows[row].flex;
	grid->rows[row].flex  = flex;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_grid_set_row_height(cgui_grid_t *grid, size_t row, int height)
{
	assert(grid);
	assert(grid->n_rows > row);

	if (grid->failed)
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
		grid->total_height += height;
	}
	else
	{
		grid->total_height_inv -= height;
	}

	grid->rows[row].size = height;
}

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

void
grid_destroy(cgui_grid_t *grid)
{
	if (!grid->to_destroy)
	{
		return;
	}

	cobj_tracker_reset_iterator(grid->areas);
	while (cobj_tracker_increment_iterator(grid->areas))
	{
		free((void*)cobj_tracker_get_iteration(grid->areas));
	}
	
	cobj_tracker_pull_pointer(main_get_grids(), grid, grid->id);
	cobj_tracker_destroy(&grid->areas);

	free(grid->cols);
	free(grid->rows);
	free(grid);
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/


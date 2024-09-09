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

#include <cairo/cairo.h>
#include <cassette/cgui.h>
#include <cassette/cobj.h>
#include <float.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "area.h"
#include "cell.h"
#include "config.h"
#include "main.h"
#include "grid.h"
#include "safe.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static double col_width  (struct grid_line);
static double row_height (struct grid_line);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

cgui_grid cgui_grid_placeholder_instance =
{
	.n_cols           = 0,
	.n_rows           = 0,
	.col_flex         = 0,
	.row_flex         = 0,
	.width_units      = 0,
	.width_units_inv  = 0,
	.height_units     = 0,
	.height_units_inv = 0,
	.cols             = NULL,
	.rows             = NULL,
	.areas            = CREF_PLACEHOLDER,
	.ref              = NULL,
	.valid            = false,
	.used             = true,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const enum cgui_grid_relative_size compare_size[4][4] =
{
	/* EQUAL                     BIGGER                    SMALLER                   UNDEFINED  */
	{CGUI_GRID_SIZE_EQUAL,     CGUI_GRID_SIZE_BIGGER,    CGUI_GRID_SIZE_SMALLER,   CGUI_GRID_SIZE_UNDEFINED}, /* EQUAL    */
	{CGUI_GRID_SIZE_BIGGER,    CGUI_GRID_SIZE_BIGGER,    CGUI_GRID_SIZE_UNDEFINED, CGUI_GRID_SIZE_UNDEFINED}, /* BIGGER   */
	{CGUI_GRID_SIZE_SMALLER,   CGUI_GRID_SIZE_UNDEFINED, CGUI_GRID_SIZE_SMALLER,   CGUI_GRID_SIZE_UNDEFINED}, /* SMALLER  */
	{CGUI_GRID_SIZE_UNDEFINED, CGUI_GRID_SIZE_UNDEFINED, CGUI_GRID_SIZE_UNDEFINED, CGUI_GRID_SIZE_UNDEFINED}, /* UNDEFINED*/
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cgui_grid_assign_cell(cgui_grid *grid, cgui_cell *cell, size_t x, size_t y, size_t width, size_t height)
{
	struct area *area;

	if (cgui_error()
	 || !grid->valid)
	{
		return;
	}

	if (width  == 0
	 || height == 0
	 || grid->n_cols < width
	 || grid->n_rows < height
	 || grid->n_cols - width  < x
	 || grid->n_rows - height < y)
	{
		main_set_error(CERR_PARAM);
		return;
	}

	if (!(area = malloc(sizeof(struct area))))
	{
		main_set_error(CERR_MEMORY);
		return;
	}

	cref_push(grid->areas, area);
	if (cref_error(grid->areas))
	{
		main_set_error(cref_error(grid->areas));
		free(area);
		return;
	}

	area->x      = x;
	area->y      = y;
	area->width  = width;
	area->height = height;
	area->cell   = cell;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgui_grid *
cgui_grid_clone(const cgui_grid *grid)
{
	cgui_grid *grid_new;
	const struct area *area;

	if (cgui_error() || !grid->valid)
	{
		goto fail_main;
	}

	if (!(grid_new = malloc(sizeof(cgui_grid))))
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

	if (!main_push_instance(main_grids(), grid_new))
	{
		goto fail_push;
	}

	CREF_FOR_EACH(grid->areas, i)
	{
		area = (const struct area*)cref_ptr(grid->areas, i);
		cgui_grid_assign_cell(grid_new, area->cell, area->x, area->y, area->width, area->height);
		if (cgui_error())
		{
			goto fail_copy;
		}
	}

	memcpy(grid_new->cols, grid->cols, grid->n_cols * sizeof(struct grid_line));
	memcpy(grid_new->rows, grid->rows, grid->n_rows * sizeof(struct grid_line));

	grid_new->n_cols           = grid->n_cols;
	grid_new->n_rows           = grid->n_rows;
	grid_new->col_flex         = grid->col_flex;
	grid_new->row_flex         = grid->row_flex;
	grid_new->width_units      = grid->width_units;
	grid_new->width_units_inv  = grid->width_units_inv;
	grid_new->height_units     = grid->height_units;
	grid_new->height_units_inv = grid->height_units_inv; 
	grid_new->ref              = grid->ref;
	grid_new->valid            = true;
	grid_new->used             = false;

	return grid_new;

	/* errors */

fail_copy:
	cref_repair(grid->areas);
	CREF_FOR_EACH(grid->areas, i)
	{
		free(cref_ptr(grid->areas, i));
	}
fail_push:
	cref_destroy(grid_new->areas);
fail_areas:
	free(grid_new->rows);
fail_rows:
	free(grid_new->cols);
fail_cols:
	free(grid_new);
fail_grid:
	main_set_error(CERR_INSTANCE);
fail_main:
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

	if (cgui_error() || !grid_1->valid || !grid_2->valid)
	{
		return CGUI_GRID_FLEX_INVALID;
	}

	x1 = grid_1->col_flex > 0.0;
	x2 = grid_2->col_flex > 0.0;
	y1 = grid_1->row_flex > 0.0;
	y2 = grid_2->row_flex > 0.0;

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
	enum cgui_grid_relative_size x = CGUI_GRID_SIZE_UNDEFINED;
	enum cgui_grid_relative_size y = CGUI_GRID_SIZE_UNDEFINED;

	if (cgui_error() || !grid_1->valid || !grid_2->valid)
	{
		return CGUI_GRID_SIZE_INVALID;
	}

	/* compare x axis */

	if (grid_1->n_cols          == grid_2->n_cols
	 && grid_1->width_units     == grid_2->width_units
	 && grid_1->width_units_inv == grid_2->width_units_inv)
	{
		x = CGUI_GRID_SIZE_EQUAL;
	}
	else if (
	    grid_1->n_cols          >= grid_2->n_cols
	 && grid_1->width_units     >= grid_2->width_units
	 && grid_1->width_units_inv >= grid_2->width_units_inv)
	{
		x = CGUI_GRID_SIZE_BIGGER;
	}
	else if (
	    grid_1->n_cols          <= grid_2->n_cols
	 && grid_1->width_units     <= grid_2->width_units
	 && grid_1->width_units_inv <= grid_2->width_units_inv)
	{
		x = CGUI_GRID_SIZE_SMALLER;
	}

	/* compare y axis */

	if (grid_1->n_rows           == grid_2->n_rows
	 && grid_1->height_units     == grid_2->height_units
	 && grid_1->height_units_inv == grid_2->height_units_inv)
	{
		y = CGUI_GRID_SIZE_EQUAL;
	}
	else if (
	    grid_1->n_rows           >= grid_2->n_rows
	 && grid_1->height_units     >= grid_2->height_units
	 && grid_1->height_units_inv >= grid_2->height_units_inv)
	{
		y = CGUI_GRID_SIZE_BIGGER;
	}
	else if (
	    grid_1->n_rows           <= grid_2->n_rows
	 && grid_1->height_units     <= grid_2->height_units
	 && grid_1->height_units_inv <= grid_2->height_units_inv)
	{
		y = CGUI_GRID_SIZE_SMALLER;
	}

	/* result */

	return compare_size[x][y];
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgui_grid *
cgui_grid_create(size_t cols, size_t rows)
{
	cgui_grid *grid;

	if (cgui_error())
	{
		goto fail_main;
	}

	if (cols == 0
	 || rows == 0
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

	if (!main_push_instance(main_grids(), grid))
	{
		goto fail_push;
	}

	for (size_t i = 0; i < cols; i++)
	{
		grid->cols[i].units  = -1;
		grid->cols[i].offset = 0;
		grid->cols[i].size   = 0;
		grid->cols[i].flex   = 0.0;
	}

	for (size_t i = 0; i < rows; i++)
	{
		grid->rows[i].units  = 1;
		grid->rows[i].offset = 0;
		grid->rows[i].size   = 0;
		grid->rows[i].flex   = 0.0;
	}

	grid->n_cols           = cols;
	grid->n_rows           = rows;
	grid->col_flex         = 0.0;
	grid->row_flex         = 0.0;
	grid->width_units      = 0;
	grid->width_units_inv  = cols;
	grid->height_units     = rows;
	grid->height_units_inv = 0; 
	grid->ref              = CGUI_GRID_PLACEHOLDER;
	grid->valid            = true;
	grid->used             = false;

	return grid;

	/* errors */

fail_push:
	cref_destroy(grid->areas);	
fail_areas:
	free(grid->rows);
fail_rows:
	free(grid->cols);
fail_cols:
	free(grid);
fail_grid:
	main_set_error(CERR_INSTANCE);
fail_main:
	return CGUI_GRID_PLACEHOLDER;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_grid_destroy(cgui_grid *grid)
{
	grid->valid = false;
	if (!cgui_is_running())
	{
		grid_destroy(grid);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

double
cgui_grid_flex_horizontal(const cgui_grid *grid)
{
	if (cgui_error() || !grid->valid)
	{
		return 0.0;
	}

	return grid->col_flex;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

double
cgui_grid_flex_vertical(const cgui_grid *grid)
{
	if (cgui_error() || !grid->valid)
	{
		return 0.0;
	}

	return grid->row_flex;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

double
cgui_grid_height(const cgui_grid *grid)
{
	double h = 0;

	if (cgui_error() || !grid->valid)
	{
		return 0;
	}

	for (size_t i = 0; i < grid->n_rows; i++)
	{
		h += row_height(grid->rows[i]);
	}

	return h + CONFIG->grid_spacing * (grid->n_rows - 1);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_grid_resize_col(cgui_grid *grid, size_t col, ssize_t width)
{
	if (cgui_error() || !grid->valid || col >= grid->n_cols)
	{
		return;
	}

	if (width == 0)
	{
		main_set_error(CERR_PARAM);
		return;
	}

	if (grid->cols[col].units > 0)
	{
		grid->width_units -= grid->cols[col].units;
	}
	else
	{
		grid->width_units_inv += grid->cols[col].units;
	}

	if (width > 0)
	{
		grid->width_units += width;
	}
	else
	{
		grid->width_units_inv -= width;
	}

	grid->cols[col].units = width;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_grid_resize_row(cgui_grid *grid, size_t row, ssize_t height)
{
	if (cgui_error() || !grid->valid || row >= grid->n_rows)
	{
		return;
	}

	if (height == 0)
	{
		main_set_error(CERR_PARAM);
		return;
	}

	if (grid->rows[row].units > 0)
	{
		grid->height_units -= grid->rows[row].units;
	}
	else
	{
		grid->height_units_inv += grid->rows[row].units;
	}

	if (height > 0)
	{
		grid->height_units += height;
	}
	else
	{
		grid->height_units_inv -= height;
	}

	grid->rows[row].units = height;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_grid_set_col_flex(cgui_grid *grid, size_t col, double flex)
{
	if (cgui_error() || !grid->valid || col >= grid->n_cols)
	{
		return;
	}

	if (flex < 0.0)
	{
		main_set_error(CERR_PARAM);
		return;
	}	

	grid->col_flex += flex - grid->cols[col].flex;
	grid->cols[col].flex = flex;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_grid_set_reference(cgui_grid *grid, cgui_grid *grid_ref)
{
	if (cgui_error()
	 || !grid->valid
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
	if (cgui_error() || !grid->valid || row >= grid->n_rows)
	{
		return;
	}

	if (flex < 0.0)
	{
		main_set_error(CERR_PARAM);
		return;
	}	
 
	grid->row_flex += flex - grid->rows[row].flex;
	grid->rows[row].flex  = flex;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

double
cgui_grid_width(const cgui_grid *grid)
{
	double w = 0;

	if (cgui_error() || !grid->valid)
	{
		return 0;
	}

	for (size_t i = 0; i < grid->n_cols; i++)
	{
		w += col_width(grid->cols[i]);
	}

	return w + CONFIG->grid_spacing * (grid->n_cols - 1);
}

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

struct cgui_zone
grid_area_zone(const cgui_grid *grid, const struct area *area, cairo_t *drawable)
{
	struct cgui_zone zone =
	{
		.drawable = drawable,
		.x        = grid->cols[area->x].offset,
		.y        = grid->rows[area->y].offset,
		.width    = CONFIG->grid_spacing * (area->width  - 1),
		.height   = CONFIG->grid_spacing * (area->height - 1),
	};

	for (size_t i = area->x; i < area->x + area->width; i++)
	{
		zone.width += grid->cols[i].size;	
	}

	for (size_t i = area->y; i < area->y + area->height; i++)
	{
		zone.height += grid->rows[i].size;	
	}

	return zone;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
grid_destroy(cgui_grid *grid)
{
	if (grid == CGUI_GRID_PLACEHOLDER || grid->valid)
	{
		return;
	}

	CREF_FOR_EACH(grid->areas, i)
	{
		free(cref_ptr(grid->areas, i));
	}

	main_pull_instance(main_grids(), grid);
	cref_destroy(grid->areas);
	free(grid->cols);
	free(grid->rows);
	free(grid);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
grid_repair(cgui_grid *grid)
{
	if (!grid->valid)
	{
		return;
	}

	cref_repair(grid->areas);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
grid_update_geometry(cgui_grid *grid, double width, double height)
{
	double o = 0.0;
	double l;
	double n;
	double f;

	/* cols */

	f = grid->col_flex;
	n = width - cgui_grid_width(grid);

	for (size_t i = 0; i < grid->n_cols; i++)
	{
		l = f < DBL_EPSILON ? 0.0 : n * grid->cols[i].flex / f;

		grid->cols[i].offset = o;
		grid->cols[i].size   = col_width(grid->cols[i]) + l;

		o += grid->cols[i].size + CONFIG->grid_spacing;
		f -= grid->cols[i].flex;
		n -= l;
	}

	/* rows */

	o = 0.0;
	f = grid->row_flex;
	n = height - cgui_grid_height(grid);

	for (size_t i = 0; i < grid->n_rows; i++)
	{
		l = f < DBL_EPSILON ? 0.0 : n * grid->rows[i].flex / f;

		grid->rows[i].offset = o;
		grid->rows[i].size   = row_height(grid->rows[i]) + l;

		o += grid->rows[i].size + CONFIG->grid_spacing;
		f -= grid->rows[i].flex;
		n -= l;
	}
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static double
col_width(struct grid_line col)
{
	if (col.units > 0)
	{
		return cgui_config_str_width(col.units) + CONFIG->grid_padding * 2;
	}
	else
	{
		return cgui_config_str_height(-col.units) + CONFIG->grid_padding * 2;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static double
row_height(struct grid_line row)
{
	if (row.units > 0)
	{
		return cgui_config_str_height(row.units) + CONFIG->grid_padding * 2;
	}
	else
	{
		return cgui_config_str_width(-row.units) + CONFIG->grid_padding * 2;
	}
}


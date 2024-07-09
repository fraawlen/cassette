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
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
typedef struct cgui_grid cgui_grid;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
enum cgui_grid_err
{
	CGUI_GRID_OK       = 0,
	CGUI_GRID_INVALID  = 1,
	CGUI_GRID_OVERFLOW = 1 << 1,
	CGUI_GRID_MEMORY   = 1 << 2,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
enum cgui_grid_relative_size
{
	CGUI_GRID_SIZE_EQUAL,
	CGUI_GRID_SIZE_BIGGER,
	CGUI_GRID_SIZE_SMALLER,
	CGUI_GRID_SIZE_UNDEFINED,
	CGUI_GRID_SIZE_INVALID,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
enum cgui_grid_relative_flex
{
	CGUI_GRID_FLEX_SAME,
	CGUI_GRID_FLEX_DIFFERENT,
	CGUI_GRID_FLEX_INVALID,
};

/************************************************************************************************************/
/* GLOBALS **************************************************************************************************/
/************************************************************************************************************/

/**
 * A macro that gives uninitialized grid a non-NULL value that is safe to use with the grid's realted
 * functions. However, any function called with a handle set to this value will return early and without any
 * side effects.
 */
#define CGUI_GRID_PLACEHOLDER &cgui_grid_placeholder_instance

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Global grid instance with the error state set to CGUI_GRID_INVALID. This instance is only made available to
 * allow the static initialization of grid pointers with the macro CGUI_GRID_PLACEHOLDER.
 */
extern cgui_grid cgui_grid_placeholder_instance;

/************************************************************************************************************/
/* CONSTRUCTORS / DESTRUCTORS *******************************************************************************/
/************************************************************************************************************/

/**
 *
 */
cgui_grid *
cgui_grid_clone(const cgui_grid *grid)
CGUI_NONNULL_RETURN
CGUI_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
cgui_grid *
cgui_grid_create(size_t cols, size_t rows)
CGUI_NONNULL_RETURN;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
void
cgui_grid_destroy(cgui_grid *grid)
CGUI_NONNULL(1);

/************************************************************************************************************/
/* PROCEDURES ***********************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
void
cgui_grid_assign_cell(cgui_grid *grid, cgui_cell *cell, size_t x, size_t y, size_t width, size_t height)
CGUI_NONNULL(1, 2);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
void
cgui_grid_repair(cgui_grid *grid)
CGUI_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
void
cgui_grid_resize_col(cgui_grid *grid, size_t col, int16_t width)
CGUI_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
void
cgui_grid_resize_row(cgui_grid *grid, size_t row, int16_t height)
CGUI_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
void
cgui_grid_set_col_flex(cgui_grid *grid, size_t col, double flex)
CGUI_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
void
cgui_grid_set_reference(cgui_grid *grid, cgui_grid *grid_ref)
CGUI_NONNULL(1, 2);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
void
cgui_grid_set_row_flex(cgui_grid *grid, size_t row, double flex)
CGUI_NONNULL(1);

/************************************************************************************************************/
/* FUNCTIONS ************************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
enum cgui_grid_relative_flex
cgui_grid_compare_flex(const cgui_grid *grid_1, const cgui_grid *grid_2)
CGUI_NONNULL(1, 2)
CGUI_PURE;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
enum cgui_grid_relative_size
cgui_grid_compare_size(const cgui_grid *grid_1, const cgui_grid *grid_2)
CGUI_NONNULL(1, 2)
CGUI_PURE;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
enum cgui_grid_err
cgui_grid_error(const cgui_grid *grid)
CGUI_NONNULL(1)
CGUI_PURE;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
double
cgui_grid_flex_horizontal(const cgui_grid *grid)
CGUI_NONNULL(1)
CGUI_PURE;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
double
cgui_grid_flex_vertical(const cgui_grid *grid)
CGUI_NONNULL(1)
CGUI_PURE;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
uint16_t
cgui_grid_min_height(const cgui_grid *grid)
CGUI_NONNULL(1)
CGUI_PURE;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
uint16_t
cgui_grid_min_width(const cgui_grid *grid)
CGUI_NONNULL(1)
CGUI_PURE;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

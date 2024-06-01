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

#ifndef CGUI_GRID_H
#define CGUI_GRID_H

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

typedef struct grid_t cgui_grid_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgui_grid_t *cgui_grid_clone(cgui_grid_t *grid);

cgui_grid_t *cgui_grid_create(size_t n_cols, size_t n_rows);

cgui_grid_t *cgui_grid_get_placeholder(void);

void cgui_grid_destroy(cgui_grid_t **grid);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void cgui_grid_assign_cell(cgui_grid_t *grid, cgui_cell_t *cell, size_t x, size_t y, size_t width, size_t height);

void cgui_grid_set_col_width(cgui_grid_t *grid, size_t col, unsigned int width);

void cgui_grid_set_col_flex(cgui_grid_t *grid, size_t col, double flex);

void cgui_grid_set_row_height(cgui_grid_t *grid, size_t row, unsigned int height);

void cgui_grid_set_row_flex(cgui_grid_t *grid, size_t row, double flex);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* CGUI_GRID_H */

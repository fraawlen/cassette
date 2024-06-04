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

#ifndef GRID_H
#define GRID_H

#include <stdbool.h>
#include <stdlib.h>

#include <cassette/cobj.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

struct grid_line_t
{
	unsigned int size;
	double flex;
};

typedef struct grid_line_t grid_line_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct grid_t
{
	size_t id;

	/* states */

	bool to_destroy;
	bool failed;

	/* data */

	size_t n_cols;
	size_t n_rows;
	grid_line_t *cols;
	grid_line_t *rows;
	cobj_tracker_t *areas;
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void grid_destroy(cgui_grid_t *grid);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#endif /* GRID_H */

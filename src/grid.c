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
#include <stdbool.h>
#include <stdlib.h>

#include <cassette/cgui.h>
#include <cassette/cobj.h>

#include "grid.h"
#include "main.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static cgui_grid_t _err_grid =
{
	.id         = 0,
	.to_destroy = false,
	.failed     = true,
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

cgui_grid_t *
cgui_grid_create(void)
{
	cgui_grid_t *grid;

	assert(cgui_is_init());

	if (!(grid = malloc(sizeof(cgui_grid_t))))
	{
		return &_err_grid;
	}

	grid->id         = 0;
	grid->to_destroy = false;
	grid->failed     = false;

	cobj_tracker_push(main_get_grids(), grid, &grid->id);
	main_update_status();

	return grid;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_grid_destroy(cgui_grid_t **grid)
{
	assert(cgui_is_init());
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
	
	cobj_tracker_pull_pointer(main_get_grids(), grid, grid->id);
	free(grid);
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/


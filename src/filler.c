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

#include "cell.h"
#include "config.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void draw  (cgui_cell *, struct cgui_cell_context) CGUI_NONNULL(1);
static void frame (cgui_cell *, struct cgui_box        *) CGUI_NONNULL(1, 2);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

cgui_cell *
cgui_filler_create(void)
{
	cgui_cell *cell;

	cell = cgui_cell_create();

	cgui_cell_on_draw (cell, draw);
	cgui_cell_on_frame(cell, frame);
	cgui_cell_set_serial(cell, CELL_FILLER);

	return cell;
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
draw(cgui_cell *cell, struct cgui_cell_context context)
{
	(void)cell;

	cgui_box_draw(context.frame, context.zone);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
frame(cgui_cell *cell, struct cgui_box *box)
{
	(void)cell;

	*box = CONFIG->filler_frame;
}


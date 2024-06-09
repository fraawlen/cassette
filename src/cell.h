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

#include <stdbool.h>
#include <stdlib.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

struct cell_t
{
	size_t id;

	/* states */

	bool to_destroy;
	bool enabled;
	bool failed;

	/* data */

	void *data;

	/* callbacks */

	cgui_cell_callback_destroy_t fn_destroy;
	cgui_cell_callback_draw_t    fn_draw;
	cgui_cell_callback_event_t   fn_event;
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void cell_destroy(cgui_cell_t *cell);

bool cell_send_windowless_event(cgui_cell_t *cell, cgui_cell_event_t *event);

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

#ifndef WINDOW_H
#define WINDOW_H

#include <stdbool.h>
#include <stdlib.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

struct window_t
{
	size_t id;
	bool to_destroy;
	bool failed;
	cgui_window_state_t state;
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void window_destroy(cgui_window_t *window);

void window_present(cgui_window_t *window);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#endif /* WINDOW_H */

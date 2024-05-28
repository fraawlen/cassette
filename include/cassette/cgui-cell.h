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

#ifndef CGUI_CELL_H
#define CGUI_CELL_H

#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

typedef struct cell_t cgui_cell_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct cgui_cell_event_t
{
	int dummy;

	// TODO
};

typedef struct cgui_cell_event_t cgui_cell_event_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct cgui_cell_drawing_context_t
{
	int dummy;
	
	// TODO
};

typedef struct cgui_cell_drawing_context_t cgui_cell_drawing_context_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgui_cell_t *cgui_cell_create(void);

cgui_cell_t *cgui_cell_get_placeholder(void);

void cgui_cell_destroy(cgui_cell_t **cell);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void cgui_cell_disable(cgui_cell_t *cell);

void cgui_cell_enable(cgui_cell_t *cell);

void cgui_cell_redraw(cgui_cell_t *cell);

bool cgui_cell_send_custom_event(cgui_cell_t *cell, unsigned int id, void *data, size_t data_n);

void cgui_cell_toggle(cgui_cell_t *cell);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void cgui_cell_set_callback_destroy(cgui_cell_t *cell, void (*fn)(cgui_cell_t *cell));

void cgui_cell_set_callback_draw(cgui_cell_t *cell, void (*fn)(cgui_cell_t *cell, cgui_cell_drawing_context_t *context));

void cgui_cell_set_callback_event(cgui_cell_t *cell, void (*fn)(cgui_cell_t *cell, cgui_cell_event_t *event));

void cgui_cell_set_data(cgui_cell_t *cell, void *data);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void (*cgui_cell_get_callback_destroy(cgui_cell_t *cell))(cgui_cell_t *cell);

void (*cgui_cell_get_callback_draw(cgui_cell_t *cell))(cgui_cell_t *cell, cgui_cell_drawing_context_t *context);

void (*cgui_cell_get_callback_event(cgui_cell_t *cell))(cgui_cell_t *cell, cgui_cell_event_t *event);

void *cgui_cell_get_data(cgui_cell_t *cell);

bool cgui_cell_is_enabled(cgui_cell_t *cell);

bool cgui_cell_has_failed(cgui_cell_t *cell);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* CGUI_CELL_H */

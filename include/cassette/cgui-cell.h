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

#include <cassette/cobj.h>
#include <stdint.h>

#include "cgui-attributes.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
typedef struct cgui_cell cgui_cell;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
enum cgui_cell_event_type
{
	CGUI_CELL_EVENT_NONE = 0,

	// TODO
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
struct cgui_cell_event
{
	enum cgui_cell_event_type type;

	// TODO
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
struct cgui_cell_context
{
	int dummy;
	
	// TODO
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
struct cgui_cell_style
{
	/* geometry */

	uint16_t thickness_border;
	uint16_t thickness_outline;
	 int16_t margin;

	/* colors */

	struct ccolor color_background;
	struct ccolor color_border;
	struct ccolor color_outline;
};

/************************************************************************************************************/
/* GLOBALS **************************************************************************************************/
/************************************************************************************************************/

/**
 * A macro that gives uninitialized cell a non-NULL value that is safe to use with the cell's realted
 * functions. However, any function called with a handle set to this value will return early and without any
 * side effects.
 */
#define CGUI_CELL_PLACEHOLDER &cgui_cell_placeholder_instance

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Global cell instance with the error state set to CGUI_CELL_INVALID. This instance is only made available to
 * allow the static initialization of cell pointers with the macro CGUI_CELL_PLACEHOLDER.
 */
extern cgui_cell cgui_cell_placeholder_instance;

/************************************************************************************************************/
/* CONSTRUCTORS / DESTRUCTORS *******************************************************************************/
/************************************************************************************************************/

/**
 *
 */
cgui_cell *
cgui_cell_create(void)
CGUI_NONNULL_RETURN;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
void
cgui_cell_destroy(cgui_cell *cell)
CGUI_NONNULL(1);

/************************************************************************************************************/
/* PROCEDURES ***********************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
void
cgui_cell_on_destroy(cgui_cell *cell, void (*fn)(cgui_cell *cell))
CGUI_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
void
cgui_cell_on_draw(cgui_cell *cell, void (*fn)(cgui_cell *cell, struct cgui_cell_context *context))
CGUI_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
void
cgui_cell_on_event(cgui_cell *cell, void (*fn)(cgui_cell *cell, struct cgui_cell_event *event))
CGUI_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
void
cgui_cell_redraw(cgui_cell *cell)
CGUI_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
void
cgui_cell_repair(cgui_cell *cell)
CGUI_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
bool
cgui_cell_send_custom_event(cgui_cell *cell, int id, void *data, size_t length)
CGUI_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
void
cgui_cell_set_data(cgui_cell *cell, void *data)
CGUI_NONNULL(1);

/************************************************************************************************************/
/* FUNCTIONS ************************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
void *
cgui_cell_data(const cgui_cell *cell)
CGUI_NONNULL(1)
CGUI_PURE;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
enum cerr
cgui_cell_error(const cgui_cell *cell)
CGUI_NONNULL(1)
CGUI_PURE;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
void
(*cgui_cell_fn_destroy(cgui_cell *cell))(cgui_cell *cell)
CGUI_NONNULL_RETURN
CGUI_NONNULL(1)
CGUI_PURE;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
void
(*cgui_cell_fn_draw(cgui_cell *cell))(cgui_cell *cell, struct cgui_cell_context *context)
CGUI_NONNULL_RETURN
CGUI_NONNULL(1)
CGUI_PURE;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
void
(*cgui_cell_fn_event(cgui_cell *cell))(cgui_cell *cell, struct cgui_cell_event *event)
CGUI_NONNULL_RETURN
CGUI_NONNULL(1)
CGUI_PURE;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

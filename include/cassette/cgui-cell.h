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

#include <cairo/cairo.h>
#include <cassette/cobj.h>
#include <stdbool.h>
#include <stdint.h>

#include "cgui-attributes.h"
#include "cgui-box.h"

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

/**
 *
 */
enum cgui_cell_msg
{
	CGUI_CELL_MSG_NONE = 0,
	CGUI_CELL_MSG_REJECT,
	CGUI_CELL_MSG_LOCK,
	CGUI_CELL_MSG_UNLOCK,
};

/**
 *
 */
enum cgui_cell_event_type
{
	CGUI_CELL_EVENT_NONE = 0,
	CGUI_CELL_EVENT_POINTER_MOTION,
	CGUI_CELL_EVENT_FOCUS_GAIN_BY_ACTION,
	CGUI_CELL_EVENT_FOCUS_GAIN_BY_POINTER,
	CGUI_CELL_EVENT_FOCUS_GAIN_BY_TOUCH,
	CGUI_CELL_EVENT_FOCUS_GAIN_BY_REFERENCE,
	CGUI_CELL_EVENT_FOCUS_LOSE,
	CGUI_CELL_EVENT_FOCUS_LOCK,
	CGUI_CELL_EVENT_FOCUS_UNLOCK,
	CGUI_CELL_EVENT_FOCUS_INFO,
	CGUI_CELL_EVENT_FOCUS_SEEK,
};

/**
 *
 */
struct cgui_cell_event
{
	enum cgui_cell_msg msg;
	enum cgui_cell_event_type type;
	double x;
	double y;
	double width;
	double height;
	union
	{
		/* CGUI_CELL_EVENT_POINTER_MOTION */
		struct
		{
			double pointer_x;
			double pointer_y;
		};
		/* CGUI_CELL_EVENT_FOCUS_GAIN_BY_POINTER */
		/* CGUI_CELL_EVENT_FOCUS_GAIN_BY_TOUCH   */
		struct
		{
			double focus_x;
			double focus_y;
		};
		/* CGUI_CELL_EVENT_FOCUS_GAIN_BY_ACTION    */
		// TODO
		/* CGUI_CELL_EVENT_FOCUS_GAIN_BY_REFERENCE */
		cgui_cell *focus_cell;
		/* CGUI_CELL_EVENT_FOCUS_INFO */
		struct
		{
			cgui_cell *focus_info_cell;
			double focus_info_x;
			double focus_info_y;
			double focus_info_width;
			double focus_info_height;
		};
		/* CGUI_CELL_EVENT_FOCUS_SEEK */
		cgui_cell *seek_cell;
		/* CGUI_CELL_EVENT_FOCUS_LOSE   */
		/* CGUI_CELL_EVENT_FOCUS_LOCK   */
		/* CGUI_CELL_EVENT_FOCUS_UNLOCK */
		/* CGUI_CELL_EVENT_NONE         */
		/* no extra fields for these events */
	};
};

/**
 *
 */
struct cgui_cell_context
{
	cairo_t *drawable;
	unsigned long delay;
	struct cgui_box frame;
	double x;
	double y;
	double width;
	double height;
};

/************************************************************************************************************/
/* GLOBALS **************************************************************************************************/
/************************************************************************************************************/

/**
 * A macro that gives uninitialized cell a non-NULL value that is safe to use with the cell's realted
 * functions. However, any function called with a handle set to this value will return early and without any
 * side effects.
 */
#define CGUI_CELL_PLACEHOLDER (&cgui_cell_placeholder_instance)

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

/**
 *
 */
void
cgui_cell_destroy(cgui_cell *cell)
CGUI_NONNULL(1);

/************************************************************************************************************/
/* IMPURE METHODS *******************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
void
cgui_cell_on_destroy(cgui_cell *cell, void (*fn)(cgui_cell *cell))
CGUI_NONNULL(1);

/**
 *
 */
void
cgui_cell_on_draw(cgui_cell *cell, void (*fn)(cgui_cell *cell, struct cgui_cell_context context))
CGUI_NONNULL(1);

/**
 *
 */
void
cgui_cell_on_event(cgui_cell *cell, void (*fn)(cgui_cell *cell, struct cgui_cell_event *event))
CGUI_NONNULL(1);

/**
 *
 */
void
cgui_cell_on_frame(cgui_cell *cell, void (fn)(cgui_cell *cell, struct cgui_box *box))
CGUI_NONNULL(1);

/**
 *
 */
void
cgui_cell_redraw(cgui_cell *cell)
CGUI_NONNULL(1);

/**
 *
 */
void
cgui_cell_set_data(cgui_cell *cell, void *data)
CGUI_NONNULL(1);

/**
 *
 */
void
cgui_cell_set_serial(cgui_cell *cell, int serial)
CGUI_NONNULL(1);

/************************************************************************************************************/
/* PURE METHODS *********************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
void *
cgui_cell_data(const cgui_cell *cell)
CGUI_NONNULL(1)
CGUI_PURE;

/**
 *
 */
void
(*cgui_cell_fn_destroy(cgui_cell *cell))(cgui_cell *cell)
CGUI_NONNULL_RETURN
CGUI_NONNULL(1)
CGUI_PURE;

/**
 *
 */
void
(*cgui_cell_fn_draw(cgui_cell *cell))(cgui_cell *cell, struct cgui_cell_context context)
CGUI_NONNULL_RETURN
CGUI_NONNULL(1)
CGUI_PURE;

/**
 *
 */
void
(*cgui_cell_fn_event(cgui_cell *cell))(cgui_cell *cell, struct cgui_cell_event *event)
CGUI_NONNULL_RETURN
CGUI_NONNULL(1)
CGUI_PURE;

/**
 *
 */
void
(*cgui_cell_fn_frame(cgui_cell *cell))(cgui_cell *cell, struct cgui_box *box)
CGUI_NONNULL_RETURN
CGUI_NONNULL(1)
CGUI_PURE;

/**
 *
 */
bool
cgui_cell_is_valid(const cgui_cell *cell)
CGUI_NONNULL(1)
CGUI_PURE;

/**
 *
 */
int
cgui_cell_serial(const cgui_cell *cell)
CGUI_NONNULL(1)
CGUI_PURE;

/************************************************************************************************************/
/* HELPERS **************************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
void
cgui_cell_draw_frame(struct cgui_cell_context context);

/**
 *
 */
void
cgui_cell_clip_frame(struct cgui_cell_context context);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

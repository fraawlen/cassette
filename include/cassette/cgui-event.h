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

#include "cgui-window.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
enum cgui_event_type
{
	CGUI_EVENT_NONE = 0,
	CGUI_EVENT_UNKNOWN_XCB,
	CGUI_EVENT_RECONFIG,
	CGUI_EVENT_CLOSE,
	CGUI_EVENT_ACCELERATOR,
	CGUI_EVENT_TRANSFORM,
	CGUI_EVENT_MAP,
	CGUI_EVENT_UNMAP,
	CGUI_EVENT_FOCUS,
	CGUI_EVENT_UNFOCUS,
	CGUI_EVENT_TOUCH_BEGIN,
	CGUI_EVENT_TOUCH_UPDATE,
	CGUI_EVENT_TOUCH_END,
};

/**
 *
 */
struct cgui_event
{
	cgui_window *window;
	enum cgui_event_type type;
	union
	{
		/* CGUI_EVENT_UNKNOWN_XCB */
		xcb_generic_event_t *xcb_event;
		/* CGUI_EVENT_ACCELERATOR */
		int accelerator;
		/* CGUI_EVENT_TRANSFORM */
		struct
		{
			 int16_t transform_x;
			 int16_t transform_y;
			uint16_t transform_width;
			uint16_t transform_height;
		};
		/* CGUI_EVENT_TOUCH_BEGIN  */
		/* CGUI_EVENT_TOUCH_UPDATE */
		/* CGUI_EVENT_TOUCH_END    */
		struct
		{
			 int16_t touch_x;
			 int16_t touch_y;
			uint32_t touch_id;
		};
		/* CGUI_EVENT_CLOSE    */
		/* CGUI_EVENT_RECONFIG */
		/* CGUI_EVENT_MAP      */
		/* CGUI_EVENT_UNMAP    */
		/* CGUI_EVENT_FOCUS    */
		/* CGUI_EVENT_UNFOCUS  */
		/* CGUI_EVENT_NONE     */
		/* no extra fields for these events */
	};
};

/************************************************************************************************************/
/* IMPURE METHODS *******************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
void
cgui_event_on_event(void (*fn)(struct cgui_event *event));

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif


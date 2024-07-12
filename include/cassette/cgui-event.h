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
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
struct cgui_event
{
	enum cgui_event_type type;
	union
	{
		/* CGUI_EVENT_UNKNOWN_XCB */
		xcb_generic_event_t *xcb_event;
		/* CGUI_EVENT_NONE */
		/* no fields for these events */
	};
};

/************************************************************************************************************/
/* PROCEDURES ***********************************************************************************************/
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


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

#ifndef CGUI_EVENT_H
#define CGUI_EVENT_H

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

enum cgui_event_kind_t
{
	CGUI_EVENT_NONE = 0,
	CGUI_EVENT_UNKNOWN_XCB,
};

typedef enum cgui_event_kind_t cgui_event_kind_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct cgui_event_t
{
	cgui_event_kind_t kind;
	union
	{
		/* CGUI_EVENT_UNKNOWN_XCB */
		xcb_generic_event_t *xcb_event;
		/* CGUI_EVENT_NONE */
		/* no fields for these events */
	};
};

typedef struct cgui_event_t cgui_event_t;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* CGUI_EVENT_H */

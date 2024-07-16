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
#include <xcb/xcb.h>

#include "event.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void _dummy_callback_event (struct cgui_event *) CGUI_NONNULL(1);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void (*_fn_event) (struct cgui_event *) = _dummy_callback_event;

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cgui_event_on_event(void (*fn)(struct cgui_event *event))
{
	if (cgui_error())
	{
		return;
	}

	_fn_event = fn ? fn : _dummy_callback_event;
}

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

void
event_process(struct cgui_event *event)
{
	_fn_event(event);

	switch (event->type)
	{
		// TODO

		case CGUI_EVENT_NONE:
		default:
			break;
	}
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static void
_dummy_callback_event(struct cgui_event *event)
{
	(void)event;
}


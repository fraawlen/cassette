/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#include <cairo/cairo.h>
#include <cassette/cgui.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>

#include "menu.h"

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

enum menu_action
menu_send_event(struct menu *mn, struct cevent ev)
{
	switch (ev.type)
	{
		case CEVENT_BUTTON_RELEASE:
			break;

		case CEVENT_REDRAW:
			cairo_set_operator(ev.redraw_ctx, CAIRO_OPERATOR_SOURCE);
			cairo_set_source_rgba(ev.redraw_ctx, 0.0, 0.0, 0.0, 1.0);
			cairo_paint(ev.redraw_ctx);
			cairo_set_source_rgba(ev.redraw_ctx, 0.0, 1.0, 0.0, 0.5);
			cairo_rectangle(ev.redraw_ctx, 20, 20, mn->w - 40, mn->h - 40);
			cairo_fill(ev.redraw_ctx);
			break;

		case CEVENT_TRANSFORM:
			mn->w = ev.transform_w;
			mn->h = ev.transform_h;
			break;

		case CEVENT_OPEN:
			break;

		case CEVENT_BUTTON_PRESS:
		case CEVENT_CLOSE:
		case CEVENT_FAIL:
			return MENU_HIDE;

		default:
			break;
	}

	return MENU_IDLE;
}


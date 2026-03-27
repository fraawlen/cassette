/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#include <cairo/cairo.h>
#include <cassette/cgui.h>
#include <cassette/ccfg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>

#include "event.h"
#include "menu.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void ev_conf  (struct menu *mn, struct cevent ev);
static void ev_draw  (struct menu *mn, struct cevent ev);
static void ev_shape (struct menu *mn, struct cevent ev);

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

enum menu_action
menu_send_event(struct menu *mn, struct cevent ev)
{
	event_print(ev, "menu");
	switch (ev.type)
	{
		case CEVENT_DRAW:
			ev_draw(mn, ev);
			break;

		case CEVENT_SHAPE:
			ev_shape(mn, ev);
			break;

		case CEVENT_CONFIG:
			ev_conf(mn, ev);
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

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void
ev_conf(struct menu *mn, struct cevent ev)
{
	(void)mn;
	(void)ev;

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_draw(struct menu *mn, struct cevent ev)
{
	if (!mn->damaged)
	{
		return;
	}

	cairo_set_operator(ev.drawable, CAIRO_OPERATOR_SOURCE);
	cairo_set_source_rgba(ev.drawable, 0.0, 0.0, 0.0, 1.0);
	cairo_paint(ev.drawable);
	
	cairo_set_source_rgba(ev.drawable, 0.0, 1.0, 0.0, 0.5);
	cairo_rectangle(ev.drawable, 20, 20, mn->w - 40, mn->h - 40);
	cairo_fill(ev.drawable);

	mn->damaged = false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_shape(struct menu *mn, struct cevent ev)
{
	mn->w       = ev.shape_w;
	mn->h       = ev.shape_h;
	mn->damaged = true;
}

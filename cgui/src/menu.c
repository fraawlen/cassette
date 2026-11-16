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
/************************************************************************************************************/
/************************************************************************************************************/

static constexpr struct cbox2_style default_style =
{
	.striped     = true,
	.pad         = 20,
	.border      = 20,
	.clr_border  = ccolor_black,
	.clr_bg      = {0.0, 0.5, 0.0, 0.5},
};

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

bool
menu_init(struct menu *mn)
{
	*mn = (struct menu){0};

	if (!(mn->frame = cbox2_create("menu", 1)))
	{
		return false;
	}

	cbox2_setup(mn->frame, 0, default_style, "");

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
menu_kill(struct menu *mn)
{
	cbox2_destroy(mn->frame);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum menu_action
menu_send_event(struct menu *mn, struct cevent ev)
{
	event_print(ev, "menu");
	switch (ev.type)
	{
		case CEVENT_OPEN:
			break;

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

	cbox2_config(mn->frame, ev.config);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_draw(struct menu *mn, struct cevent ev)
{
	if (!mn->damaged)
	{
		return;
	}

	cbox2_draw(mn->frame, ev.drawable);
	mn->damaged = false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_shape(struct menu *mn, struct cevent ev)
{
	cbox2_shape(mn->frame, ev.shape);
	mn->damaged = true;
}

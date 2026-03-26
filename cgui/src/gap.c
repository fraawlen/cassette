/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#include <cairo/cairo.h>
#include <cassette/ccfg.h>
#include <cassette/cgui.h>
#include <cassette/cobj.h>
#include <stddef.h>
#include <stdlib.h>

#include "event.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void event (ccell *, void *, struct cevent);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

ccell *
cgap_create(void)
{
	cbox *frame = cbox_create();

	cbox_default_outline(frame, ccolor_blue, 1);
	cbox_default_border(frame, ccolor_green, 10);
	cbox_default_background(frame, ccolor_white);

	return frame ? ccell_create(event, frame) : nullptr;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

nullptr_t
cgap_destroy(ccell *cl)
{
	free(ccell_data(cl));

	return ccell_destroy(cl);
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
event(ccell *cl, void *data, struct cevent ev)
{
	(void)cl;

	cbox *frame = data;

	switch (ev.type)
	{
		case CEVENT_REDRAW:
			cbox_draw(frame, ev.redraw_ctx);
			break;

		case CEVENT_CONFIG:
			cbox_config(frame, ev.config, "gap");
			break;

		case CEVENT_TRANSFORM:
			cbox_transform(
				frame,
				ev.transform_x,
				ev.transform_y,
				ev.transform_w,
				ev.transform_h);
			break;
		
		default:
			break;
	}
}

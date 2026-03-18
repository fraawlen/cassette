/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#include <cassette/cgui.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

#include "event.h"

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

void
event_print(struct cevent ev)
{
	(void)ev;

	#ifdef ENV_SRV_DEBUG

	switch(ev.type)
	{
		case CEVENT_CLOSE:
			printf("CEVENT_CLOSE [-]\n");
			break;

		case CEVENT_OPEN:
			printf("CEVENT_OPEN [-]\n");
			break;

		case CEVENT_FAIL:
			printf("CEVENT_FAIL [-]\n");
			break;

		case CEVENT_NONE:
			printf("CEVENT_NONE [-]\n");
			break;

		case CEVENT_BUTTON_PRESS:
			printf("CEVENT_BUTTON_PRESS [id = %i]\n", ev.button);
			break;

		case CEVENT_BUTTON_RELEASE:
			printf("CEVENT_BUTTON_RELEASE [id = %i]\n", ev.button);
			break;

		case CEVENT_REDRAW:
			printf("CEVENT_REDRAW [ctx = %p]\n", (void *)ev.redraw_ctx);
			break;

		case CEVENT_TRANSFORM:
			printf(
				"CEVENT_TRANSFORM "
				"[x = %"PRIu32", y = %"PRIu32", w = %"PRIu32", h = %"PRIu32"]\n",
				ev.transform_x,
				ev.transform_y,
				ev.transform_w,
				ev.transform_h);
			break;

		default:
			break;
	}

	#endif
}

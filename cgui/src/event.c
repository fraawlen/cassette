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
/************************************************************************************************************/
/************************************************************************************************************/

#define LOG(EV, ARGS, ...) printf("%s: \tCEVENT_" EV " [" ARGS "]\n", prefix __VA_OPT__(, __VA_ARGS__));

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

void
event_print(struct cevent ev, const char *prefix)
{
	(void)ev;
	(void)prefix;

	#ifdef ENV_SRV_DEBUG

	switch (ev.type)
	{
		case CEVENT_CLOSE:
			LOG("CLOSE", "-");
			break;

		case CEVENT_OPEN:
			LOG("OPEN", "-");
			break;

		case CEVENT_FAIL:
			LOG("FAIL", "-");
			break;

		case CEVENT_NONE:
			LOG("NONE", "-");
			break;

		case CEVENT_BUTTON_PRESS:
			LOG("BUTTON_PRESS", "id = %i", ev.button);
			break;

		case CEVENT_BUTTON_RELEASE:
			LOG("BUTTON_RELEASE", "id = %i", ev.button);
			break;

		case CEVENT_DRAW:
			LOG("DRAW", "ctx = %p", (void *)ev.drawable);
			break;

		case CEVENT_CONFIG:
			LOG("CONFIG", "cfg = %p", (void *)ev.config);
			break;

		case CEVENT_SHAPE:
			LOG("SHAPE",
			    "x = %"PRIu32", y = %"PRIu32", w = %"PRIu32", h = %"PRIu32"",
			    ev.shape_x,
			    ev.shape_y,
			    ev.shape_w,
			    ev.shape_h);
			break;

		default:
			break;
	}

	#endif
}

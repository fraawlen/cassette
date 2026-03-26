/************************************************************************************************************/
/* PRELUDE **************************************************************************************************/
/************************************************************************************************************/

#pragma once

#include <cairo/cairo.h>
#include <cassette/ccfg.h>
#include <cassette/cobj.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

enum cevent_type
{
	CEVENT_NONE = 0,
	CEVENT_FAIL,
	CEVENT_OPEN,
	CEVENT_CLOSE,
	CEVENT_SHAPE,
	CEVENT_CONFIG,
	CEVENT_REDRAW,
	CEVENT_BUTTON_PRESS,
	CEVENT_BUTTON_RELEASE,
};

struct cevent
{
	enum cevent_type type;
	union
	{
		/* CEVENT_BUTTON_PRESS   */
		/* CEVENT_BUTTON_RELEASE */

		int button;

		/* CEVENT_SHAPE */

		struct
		{
			int32_t  shape_x;
			int32_t  shape_y;
			uint32_t shape_w;
			uint32_t shape_h;
		};

		/* CEVENT_REDRAW */

		cairo_t *redraw_ctx;

		/* CEVENT_CONFIG */

		ccfg *config;

		/* CEVENT_CLOSE */
		/* CEVENT_OPEN  */
		/* CEVENT_FAIL  */
		/* CEVENT_NONE  */

		/* no fields for these events */
	};
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

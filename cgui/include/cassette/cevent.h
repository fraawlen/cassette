/************************************************************************************************************/
/* PRELUDE **************************************************************************************************/
/************************************************************************************************************/

#pragma once

#include <cairo/cairo.h>
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
	CEVENT_CLOSE,
	CEVENT_REDRAW,
	CEVENT_TRANSFORM,
	CEVENT_BUTTON_PRESS,
	CEVENT_BUTTON_RELEASE,
	CEVENT_UNKNOWN,
};

struct cevent
{
	enum cevent_type type;
	union
	{
		/* CEVENT_BUTTON_PRESS   */
		/* CEVENT_BUTTON_RELEASE */

		struct
		{
			int32_t button_x;
			int32_t button_y;
			int     button_id;
		};

		/* CEVENT_TRANSFORM */

		struct
		{
			int32_t  transform_x;
			int32_t  transform_y;
			uint32_t transform_w;
			uint32_t transform_h;
		};

		/* CEVENT_REDRAW  */

		struct
		{
			cairo_t *redraw_ctx;
		};

		/* CEVENT_NONE    */
		/* CEVENT_FAIL    */
		/* CEVENT_CLOSE   */
		/* CEVENT_UNKNOWN */

		/* no fields for these events */
	};
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

constexpr struct cevent cevent_blank   = { .type = CEVENT_NONE    };
constexpr struct cevent cevent_error   = { .type = CEVENT_FAIL    };
constexpr struct cevent cevent_unknown = { .type = CEVENT_UNKNOWN };

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

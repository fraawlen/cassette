/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#include <cassette/cgui.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <xcb/xcb.h>

#include "x11.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static bool xfail (struct cx11 *, xcb_void_cookie_t);

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

struct cevent
x11_event(struct cx11 *x)
{
	struct cevent cev = cevent_blank;
	xcb_generic_event_t *xev; 

	if (!(xev = xcb_poll_for_event(x->connection)))
	{
		cev.type = CEVENT_FAIL;
		return cev;
	}

	switch (xev->response_type & ~0x80)
	{
		case XCB_BUTTON_PRESS:
			cev.type      = CEVENT_BUTTON_PRESS;
			cev.button_x  = ((xcb_button_press_event_t*)xev)->event_x;
			cev.button_y  = ((xcb_button_press_event_t*)xev)->event_y;
			cev.button_id = ((xcb_button_press_event_t*)xev)->detail;
			break;

		case XCB_BUTTON_RELEASE:
			cev.type      = CEVENT_BUTTON_RELEASE;
			cev.button_x  = ((xcb_button_press_event_t*)xev)->event_x;
			cev.button_y  = ((xcb_button_press_event_t*)xev)->event_y;
			cev.button_id = ((xcb_button_press_event_t*)xev)->detail;
			break;

		case XCB_EXPOSE:
			cev.type = CEVENT_REDRAW;
			break;

		default:
			cev.type = CEVENT_UNKNOWN;
			break;
	}

	xcb_flush(x->connection);
	free(xev);

	return cev;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
x11_init(struct cx11 *x, int *fd)
{
	xcb_void_cookie_t ck;

	const uint32_t mask_opt = 
		  XCB_CW_BACK_PIXEL
		| XCB_CW_BORDER_PIXEL
		| XCB_CW_BIT_GRAVITY
		| XCB_CW_EVENT_MASK;

	const uint32_t mask_val[] =
	{
		  0x00000000,
		  0x00000000,
		  XCB_GRAVITY_NORTH_WEST,
		  XCB_EVENT_MASK_EXPOSURE
		| XCB_EVENT_MASK_BUTTON_PRESS
		| XCB_EVENT_MASK_BUTTON_RELEASE,
	};

	/* setup */

	if (!(x->connection = xcb_connect(nullptr, nullptr)))
	{
		goto fail_con;
	}

	if (!(x->screen = xcb_setup_roots_iterator(xcb_get_setup(x->connection)).data))
	{
		goto fail_win;
	}

	x->window = xcb_generate_id(x->connection),
	ck = xcb_create_window_checked(
		x->connection,
		XCB_COPY_FROM_PARENT,
		x->window,
		x->screen->root,
		0, 0, 500, 300, 0,
		XCB_WINDOW_CLASS_INPUT_OUTPUT,
		x->screen->root_visual,
		mask_opt,
		mask_val);

	if (xfail(x, ck))
	{
		goto fail_win;
	}

	ck = xcb_map_window_checked(x->connection, x->window);
	if (xfail(x, ck))
	{
		goto fail_map;
	}

	/* end */

	xcb_flush(x->connection);

	*fd = xcb_get_file_descriptor(x->connection);

	return true;

	/* errors */

fail_map:
	xcb_destroy_window(x->connection, x->window);
fail_win:
	xcb_disconnect(x->connection);
fail_con:
	return false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_kill(struct cx11 *x)
{
	xcb_unmap_window(x->connection, x->window);
	xcb_destroy_window(x->connection, x->window);
	xcb_disconnect(x->connection);
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static bool
xfail(struct cx11 *x, xcb_void_cookie_t ck)
{
	xcb_generic_error_t *err;

	if ((err = xcb_request_check(x->connection, ck)))
	{
		free(err);
		return true;
	}

	return false;
}

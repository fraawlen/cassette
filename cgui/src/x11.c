/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#include <cassette/cgui.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <xcb/xcb.h>

#include "shell.h"
#include "x11.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static struct cevent ev_button  (xcb_button_press_event_t   *, bool);
static struct cevent ev_expose  (xcb_expose_event_t         *);
static struct cevent ev_message (xcb_client_message_event_t *, struct x11 *);
static struct cevent ev_unknown (xcb_generic_event_t        *);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static xcb_atom_t atom     (struct x11 *, const char *);
static bool       fail     (struct x11 *, xcb_void_cookie_t);
static bool       prop_set (struct x11 *, xcb_atom_t, xcb_atom_t, uint32_t, const void *, bool);

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

void
x11_dispatch(struct x11 *x, cshell *sh)
{
	xcb_generic_event_t *xev; 
	struct cevent cev;

	/* grab new event */

	if (!(xev = xcb_poll_for_event(x->connection)))
	{
		cev = xcb_connection_has_error(x->connection) ? cevent_error : cevent_blank;
		goto skip;
	}

	/* dispatch event */

	switch (xev->response_type & ~0x80)
	{
		case XCB_BUTTON_PRESS:
			cev = ev_button((xcb_button_press_event_t*)xev, true);
			break;

		case XCB_BUTTON_RELEASE:
			cev = ev_button((xcb_button_press_event_t*)xev, false);
			break;

		case XCB_CLIENT_MESSAGE:
			cev = ev_message((xcb_client_message_event_t*)xev, x);
			break;

		case XCB_EXPOSE:
			cev = ev_expose((xcb_expose_event_t*)xev);
			break;

		default:
			cev = ev_unknown(xev);
			break;
	}

	/* end */

skip:

	shell_dispatch_event(sh, cev);
	xcb_flush(x->connection);
	free(xev);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
x11_init(struct x11 *x, int *fd)
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

	/* base setup */

	*x = (struct x11){0};

	if (xcb_connection_has_error(x->connection = xcb_connect(nullptr, nullptr)))
	{
		goto fail_con;
	}

	if (!(x->screen = xcb_setup_roots_iterator(xcb_get_setup(x->connection)).data))
	{
		goto fail_win;
	}

	/* window setup */

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

	if (fail(x, ck))
	{
		goto fail_win;
	}

	/* ICCCM setup */

	x->atom_utf8     = atom(x, "UTF8_STRING");
	x->atom_time     = atom(x, "TIMESTAMP");
	x->atom_protocol = atom(x, "WM_PROTOCOLS");
	x->atom_close    = atom(x, "WM_DELETE_WINDOW");
	x->atom_focus    = atom(x, "WM_TAKE_FOCUS");
	x->atom_ping     = atom(x, "_NET_WM_PING");

	prop_set(x, x->atom_protocol, XCB_ATOM_ATOM, 1, &x->atom_close, true);
	prop_set(x, x->atom_protocol, XCB_ATOM_ATOM, 1, &x->atom_focus, false);
	prop_set(x, x->atom_protocol, XCB_ATOM_ATOM, 1, &x->atom_ping,  false);

	/* end */

	if (fail(x, xcb_map_window_checked(x->connection, x->window)))
	{
		goto fail_map;
	}

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
x11_kill(struct x11 *x)
{
	xcb_unmap_window(x->connection, x->window);
	xcb_destroy_window(x->connection, x->window);
	xcb_disconnect(x->connection);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_redraw(struct x11 *x)
{
	(void)x;

	// TODO
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static xcb_atom_t
atom(struct x11 *x, const char *name)
{
	xcb_intern_atom_cookie_t ck;
	xcb_intern_atom_reply_t *rp;
	xcb_atom_t at;

	ck = xcb_intern_atom(x->connection, 0, strlen(name), name);
	rp = xcb_intern_atom_reply(x->connection, ck, nullptr);
	if (!rp)
	{
		return 0;
	}

	at = rp->atom;
	free(rp);

	return at;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static struct cevent
ev_button(xcb_button_press_event_t *xev, bool press)
{
	struct cevent cev =
	{
		.type      = press ? CEVENT_BUTTON_PRESS : CEVENT_BUTTON_RELEASE,
		.button_x  = xev->event_x,
		.button_y  = xev->event_y,
		.button_id = xev->detail,
	};

	return cev;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static struct cevent
ev_expose(xcb_expose_event_t *xev)
{
	(void)xev;

	struct cevent cev = { .type = CEVENT_REDRAW };

	return cev;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static struct cevent
ev_message(xcb_client_message_event_t *xev, struct x11 *x)
{
	xcb_atom_t msg = xev->data.data32[0];
	struct cevent cev = cevent_unknown;

	if (xev->type != x->atom_protocol)
	{
		return cev;
	}
	
	if (msg == x->atom_close)
	{
		cev.type = CEVENT_CLOSE;
	}
	else if (msg == x->atom_focus)
	{
		xcb_set_input_focus(x->connection, XCB_INPUT_FOCUS_PARENT, xev->window, XCB_CURRENT_TIME);
	}
	else if (msg == x->atom_ping)
	{
		xev->window = x->screen->root;
		xcb_send_event(x->connection, 0, x->screen->root, XCB_EVENT_MASK_NO_EVENT, (char*)xev);
	}

	return cev;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static struct cevent
ev_unknown(xcb_generic_event_t *xev)
{
	(void)xev;

	return cevent_unknown;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
fail(struct x11 *x, xcb_void_cookie_t ck)
{
	xcb_generic_error_t *err;

	if ((err = xcb_request_check(x->connection, ck)))
	{
		free(err);
		return true;
	}

	return false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
prop_set(struct x11 *x, xcb_atom_t prop, xcb_atom_t type, uint32_t n, const void *data, bool head)
{
	return fail(x,
		xcb_change_property_checked(
			x->connection,
			head ? XCB_PROP_MODE_REPLACE : XCB_PROP_MODE_APPEND,
			x->window,
			prop,
			type,
			type == x->atom_utf8 || type == x->atom_time || type == XCB_ATOM_STRING ? 8 : 32,
			n,
			data));
}

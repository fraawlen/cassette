/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#include <cairo/cairo.h>
#include <cairo/cairo-xcb.h>
#include <cassette/cgui.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <xcb/present.h>
#include <xcb/render.h>
#include <xcb/xcb.h>
#include <xcb/xcb_aux.h>
#include <xcb/xcb_renderutil.h>
#include <xcb/xfixes.h>

#include "shell.h"
#include "x11.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static struct cevent ev_button    (xcb_button_press_event_t     *, bool);
static struct cevent ev_conf      (xcb_configure_notify_event_t *, struct x11 *, uint32_t, uint32_t);
static struct cevent ev_expose    (xcb_expose_event_t           *, struct x11 *);
static struct cevent ev_extension (xcb_ge_generic_event_t       *, struct x11 *);
static struct cevent ev_message   (xcb_client_message_event_t   *, struct x11 *);
static struct cevent ev_present   (xcb_present_generic_event_t  *, struct x11 *);
static struct cevent ev_unknown   (xcb_generic_event_t          *);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static xcb_atom_t atom        (struct x11 *, const char *);
static bool       fail        (struct x11 *, xcb_void_cookie_t);
static uint8_t    opcode      (struct x11 *, const char *);
static bool       prop_set    (struct x11 *, xcb_atom_t, xcb_atom_t, uint32_t, const void *, bool);
static bool       setup_image (struct x11 *);

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

void
x11_commit(struct x11 *x, cshell *sh)
{
	uint32_t w = shell_w(sh);
	uint32_t h = shell_h(sh);

	struct cevent ev =
	{
		.type       = CEVENT_REDRAW,
		.redraw_ctx = x->cairo,
	};

	if (x->busy)
	{
		goto end;
	}

	/* update buffer size */

	if (x->buffer_w < w || x->buffer_h < h)
	{	
		xcb_free_pixmap(x->connection, x->buffer);
		x->buffer_w = w;
		x->buffer_h = h;
		x->buffer = xcb_generate_id(x->connection);
		xcb_create_pixmap(x->connection, x->depth, x->buffer, x->window, w, h);
		cairo_surface_flush(x->surface);
		cairo_xcb_surface_set_drawable(x->surface, x->buffer, w, h);
	}

	/* rendering */

	if (x->redraw)
	{
		x->redraw = false;
		shell_dispatch_event(sh, ev);
		cairo_surface_flush(x->surface);
	}

	if (x->present && !x->wait)
	{
		if (x->opcode_present != 0)
		{
			x->busy    = true;
			x->wait    = true;
			x->present = false;
			xcb_present_pixmap(
				x->connection,
				x->window,
				x->buffer,
				++x->serial,
				XCB_XFIXES_REGION_NONE,
				XCB_XFIXES_REGION_NONE,
				0, 0, 0, 0, 0,
				XCB_PRESENT_OPTION_COPY,
				0, 1, 0, 0,
				nullptr);
		}
		else /* fallback */
		{
			x->present = false;
			xcb_copy_area(x->connection, x->buffer, x->window, x->gc, 0, 0, 0, 0, w, h);
		}
	}

	/* end */

end:

	xcb_flush(x->connection);
	if (xcb_connection_has_error(x->connection))
	{
		shell_dispatch_event(sh, cevent_error);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_dispatch(struct x11 *x, cshell *sh)
{
	xcb_generic_event_t *xev; 
	struct cevent cev;

	while((xev = xcb_poll_for_event(x->connection)))
	{
		switch (xev->response_type & ~0x80)
		{
			case XCB_BUTTON_PRESS:
				cev = ev_button((xcb_button_press_event_t *)xev, true);
				break;

			case XCB_BUTTON_RELEASE:
				cev = ev_button((xcb_button_press_event_t *)xev, false);
				break;

			case XCB_CLIENT_MESSAGE:
				cev = ev_message((xcb_client_message_event_t *)xev, x);
				break;
	
			case XCB_CONFIGURE_NOTIFY:
				cev = ev_conf((xcb_configure_notify_event_t *)xev, x, shell_w(sh), shell_h(sh));
				break;
	
			case XCB_EXPOSE:
				cev = ev_expose((xcb_expose_event_t *)xev, x);
				break;
	
			case XCB_GE_GENERIC:
				cev = ev_extension((xcb_ge_generic_event_t *)xev, x);
				break;
	
			default:
				cev = ev_unknown(xev);
				break;
		}

		shell_dispatch_event(sh, cev);
		free(xev);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
x11_init(struct x11 *x, int *fd, uint32_t w, uint32_t h)
{
	xcb_void_cookie_t ck;

	/* base setup */

	*x = (struct x11){0};

	if (xcb_connection_has_error(x->connection = xcb_connect(nullptr, nullptr)))
	{
		goto fail_con;
	}

	if (!(x->screen = xcb_setup_roots_iterator(xcb_get_setup(x->connection)).data))
	{
		goto fail_screen;
	}

	/* extension check */

	x->opcode_present = opcode(x, "Present");
	x->opcode_render  = opcode(x, "RENDER");
	x->opcode_xinput  = opcode(x, "XInputExtension");

	/* select format, visual and depth (xrender default, screen root fallback) */

	if (!setup_image(x))
	{
		goto fail_screen;
	}

	/* colormap setup */

	x->colormap = xcb_generate_id(x->connection);
	ck = xcb_create_colormap_checked(
		x->connection,
		XCB_COLORMAP_ALLOC_NONE,
		x->colormap,
		x->screen->root,
		x->visual->visual_id);

	if (fail(x, ck))
	{
		goto fail_screen;
	}

	/* window setup */

	const uint32_t win_opt = 
		  XCB_CW_BACK_PIXMAP
		| XCB_CW_BORDER_PIXEL
		| XCB_CW_BIT_GRAVITY
		| XCB_CW_EVENT_MASK
		| XCB_CW_COLORMAP;

	const uint32_t win_val[] =
	{
		  XCB_BACK_PIXMAP_NONE,
		  0x00000000,
		  XCB_GRAVITY_NORTH_WEST,
		  XCB_EVENT_MASK_EXPOSURE
		| XCB_EVENT_MASK_STRUCTURE_NOTIFY
		| XCB_EVENT_MASK_BUTTON_PRESS
		| XCB_EVENT_MASK_BUTTON_RELEASE,
		  x->colormap,
	};

	x->window = xcb_generate_id(x->connection);
	ck = xcb_create_window_checked(
		x->connection,
		x->depth,
		x->window,
		x->screen->root,
		0, 0, w, h, 0,
		XCB_WINDOW_CLASS_INPUT_OUTPUT,
		x->visual->visual_id,
		win_opt,
		win_val);

	if (fail(x, ck))
	{
		goto fail_win;
	}

	/* buffer setup */

	x->buffer = xcb_generate_id(x->connection);
	ck = xcb_create_pixmap_checked(
		x->connection,
		x->depth,
		x->buffer,
		x->window,
		w, h);

	if (fail(x, ck))
	{
		goto fail_buf;
	}

	/* gc setup for present extension fallback */

	const uint32_t gc_opt   = XCB_GC_FOREGROUND;
	const uint32_t gc_val[] = {0x00000000};

	x->gc = xcb_generate_id(x->connection);
	ck = xcb_create_gc_checked(x->connection, x->gc, x->buffer, gc_opt, gc_val);

	if (fail(x, ck))
	{
		goto fail_gc;
	}

	/* cairo setup */

	x->surface = cairo_xcb_surface_create(x->connection, x->buffer, x->visual, w, h);
	if (cairo_surface_status(x->surface) != CAIRO_STATUS_SUCCESS)
	{
		goto fail_sfc;
	}

	x->cairo = cairo_create(x->surface);
	if (cairo_status(x->cairo) != CAIRO_STATUS_SUCCESS)
	{
		goto fail_ctx;
	}

	/* register window to extension events */

	if (x->opcode_present != 0)
	{
		ck = xcb_present_select_input_checked(
			  x->connection,
			  xcb_generate_id(x->connection),
			  x->window, 
			  XCB_PRESENT_EVENT_MASK_IDLE_NOTIFY
			| XCB_PRESENT_EVENT_MASK_COMPLETE_NOTIFY);

		if (fail(x, ck))
		{
			goto fail_ev;
		}
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
		goto fail_ev;
	}

	x->buffer_w = w;
	x->buffer_h = h;
	x->serial   = 0;
	x->redraw   = true;
	x->present  = false;
	x->busy     = false;
	x->wait     = false;

	xcb_flush(x->connection);

	*fd = xcb_get_file_descriptor(x->connection);

	return true;

	/* errors */

fail_ev:
	cairo_destroy(x->cairo);
fail_ctx:
	cairo_surface_destroy(x->surface);
fail_sfc:
	xcb_free_gc(x->connection, x->gc);
fail_gc:
	xcb_free_pixmap(x->connection, x->buffer);
fail_buf:
	xcb_destroy_window(x->connection, x->window);
fail_win:
	xcb_free_colormap(x->connection, x->colormap);
fail_screen:
	xcb_disconnect(x->connection);
fail_con:
	return false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_kill(struct x11 *x)
{
	cairo_destroy(x->cairo);
	cairo_surface_finish(x->surface);
	cairo_surface_destroy(x->surface);
	xcb_free_gc(x->connection, x->gc);
	xcb_free_colormap(x->connection, x->colormap);
	xcb_free_pixmap(x->connection, x->buffer);
	xcb_unmap_window(x->connection, x->window);
	xcb_destroy_window(x->connection, x->window);
	xcb_disconnect(x->connection);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_redraw(struct x11 *x)
{
	x->redraw = true;
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
		.type   = press ? CEVENT_BUTTON_PRESS : CEVENT_BUTTON_RELEASE,
		.button = xev->detail,
	};

	return cev;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static struct cevent
ev_conf(xcb_configure_notify_event_t *xev, struct x11 *x, uint32_t w, uint32_t h)
{
	struct cevent cev =
	{
		.type = CEVENT_TRANSFORM,
		.transform_w = xev->width,
		.transform_h = xev->height,
		.transform_x = xev->x,
		.transform_y = xev->y,
	};

	x->present |= xev->width < w || xev->height < h;

	return cev;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static struct cevent
ev_expose(xcb_expose_event_t *xev, struct x11 *x)
{
	x->present |= xev->count == 0;

	return cevent_blank;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static struct cevent
ev_extension(xcb_ge_generic_event_t *xev, struct x11 *x)
{
	if (xev->extension == x->opcode_present)
	{
		return ev_present((xcb_present_generic_event_t *)xev, x);
	}

	return cevent_blank;
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
	else if (msg == x->atom_close)
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
ev_present(xcb_present_generic_event_t *xev, struct x11 *x)
{
	xcb_present_complete_notify_event_t *cev = (xcb_present_complete_notify_event_t *)xev;
	xcb_present_idle_notify_event_t     *iev = (xcb_present_idle_notify_event_t     *)xev;

	switch (xev->evtype)
	{
		case XCB_PRESENT_EVENT_COMPLETE_NOTIFY:
			x->wait &= cev->serial != x->serial || cev->kind != XCB_PRESENT_COMPLETE_KIND_PIXMAP;
			break;

		case XCB_PRESENT_EVENT_IDLE_NOTIFY:
			x->busy &= iev->pixmap != x->buffer;
			break;

		default:
			break;
	}

	return cevent_blank;
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

static uint8_t
opcode(struct x11 *x, const char *name)
{
	xcb_query_extension_cookie_t ck;
	xcb_query_extension_reply_t *rp;
	uint8_t opcode = 0;

	ck = xcb_query_extension(x->connection, strlen(name), name);
	if ((rp = xcb_query_extension_reply(x->connection, ck, nullptr)))
	{
		opcode = rp->major_opcode;
	}

	free(rp);
	return opcode;
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
setup_image(struct x11 *x)
{
	xcb_render_query_pict_formats_reply_t *rp = nullptr;
	xcb_visualid_t id = x->screen->root_visual;

	/* fallback visual */

	if (x->opcode_render == 0)
	{
		goto done;
	}

	/* xrender visual with alpha */

	xcb_render_query_pict_formats_cookie_t ck;
	xcb_render_pictscreen_iterator_t it_screen;
	xcb_render_pictdepth_iterator_t it_depth;
	xcb_render_pictvisual_iterator_t it_visual;
	xcb_render_pictforminfo_t *format;

	ck = xcb_render_query_pict_formats(x->connection);
	if (!(rp = xcb_render_query_pict_formats_reply(x->connection, ck, nullptr))
	 || !(format = xcb_render_util_find_standard_format(rp, XCB_PICT_STANDARD_ARGB_32)))
	{
		goto done;
	}

	it_screen = xcb_render_query_pict_formats_screens_iterator(rp);
	for (; it_screen.rem; xcb_render_pictscreen_next(&it_screen))
	{
		it_depth = xcb_render_pictscreen_depths_iterator(it_screen.data);
		for (; it_depth.rem;  xcb_render_pictdepth_next(&it_depth))
		{
			it_visual = xcb_render_pictdepth_visuals_iterator(it_depth.data);
			for (; it_visual.rem; xcb_render_pictvisual_next(&it_visual))
			{
				if (it_visual.data->format == format->id)
				{
					id = it_visual.data->visual;
					goto done;
				}
			}
		}
	}

	/* end */

done:
	free(rp);
	return (x->depth  = xcb_aux_get_depth_of_visual(x->screen, id)) != 0
	    && (x->visual = xcb_aux_find_visual_by_id(x->screen, id));
}

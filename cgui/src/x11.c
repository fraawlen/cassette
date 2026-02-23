/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#include <cairo/cairo.h>
#include <cairo/cairo-xcb.h>
#include <cassette/cgui.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <xcb/present.h>
#include <xcb/render.h>
#include <xcb/sync.h>
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
static struct cevent ev_conf      (xcb_configure_notify_event_t *, struct x11 *);
static struct cevent ev_expose    (xcb_expose_event_t           *, struct x11 *);
static struct cevent ev_extension (xcb_ge_generic_event_t       *, struct x11 *);
static struct cevent ev_message   (xcb_client_message_event_t   *, struct x11 *);
static struct cevent ev_present   (xcb_present_generic_event_t  *, struct x11 *);
static struct cevent ev_unknown   (xcb_generic_event_t          *);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static xcb_atom_t atom           (struct x11 *, const char *);
static bool       fail           (struct x11 *, xcb_void_cookie_t);
static bool       inputs_grab    (struct x11 *);
static void       inputs_ungrab  (struct x11 *);
static uint8_t    opcode         (struct x11 *, const char *);
static void       position_popup (struct x11 *, uint32_t, uint32_t, int32_t *, int32_t *);
static bool       prop_set       (struct x11 *, struct x11_window *, xcb_atom_t, xcb_atom_t, uint32_t, const void *, bool);
static bool       setup_image    (struct x11 *);
static void       setup_sync     (struct x11 *);
static struct x11_window *window (struct x11 *, xcb_window_t);
static void       window_commit  (struct x11_window *, struct x11 *, cshell *);
static void       window_destroy (struct x11_window *, struct x11 *);
static bool       window_init    (struct x11_window *, struct x11 *, uint32_t, uint32_t, bool);

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

void
x11_menu_close(struct x11 *x)
{
	window_destroy(&x->menu, x);
	inputs_ungrab(x);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
x11_menu_open(struct x11 *x, uint32_t w, uint32_t h)
{
	return inputs_grab(x) && window_init(&x->menu, x, w, h, true);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_menu_redraw(struct x11 *x)
{
	x->menu.redraw = true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_server_commit(struct x11 *x, cshell *sh)
{
	window_commit(&x->shell, x, sh);
	window_commit(&x->menu,  x, sh);

	xcb_flush(x->connection);
	if (xcb_connection_has_error(x->connection))
	{
		shell_dispatch_event(sh, cevent_error);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_server_dispatch(struct x11 *x, cshell *sh)
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
				cev = ev_conf((xcb_configure_notify_event_t *)xev, x);
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
x11_server_init(struct x11 *x, int *fd)
{
	xcb_void_cookie_t ck;

	*x = (struct x11){0};

	/* base setup */

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
	x->opcode_randr   = opcode(x, "RANDR");
	x->opcode_sync    = opcode(x, "SYNC");

	setup_sync(x);

	/* select format, visual and depth (xrender default, screen root fallback) */

	if (!setup_image(x))
	{
		goto fail_image;
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
		goto fail_color;
	}

	/* setup atoms */

	x->atom_clip     = atom(x, "CLIPBOARD");
	x->atom_multiple = atom(x, "MULTIPLE");
	x->atom_target   = atom(x, "TARGETS");
	x->atom_utf8     = atom(x, "UTF8_STRING");
	x->atom_time     = atom(x, "TIMESTAMP");
	x->atom_protocol = atom(x, "WM_PROTOCOLS");
	x->atom_close    = atom(x, "WM_DELETE_WINDOW");
	x->atom_focus    = atom(x, "WM_TAKE_FOCUS");
	x->atom_name     = atom(x, "WM_NAME");
	x->atom_icon     = atom(x, "WM_ICON_NAME");
	x->atom_class    = atom(x, "WM_CLASS");
	x->atom_cmd      = atom(x, "WM_COMMAND");
	x->atom_host     = atom(x, "WM_CLIENT_MACHINE");
	x->atom_lead     = atom(x, "WM_CLIENT_LEADER");
	x->atom_ping     = atom(x, "_NET_WM_PING");
	x->atom_pid      = atom(x, "_NET_WM_PID");
	x->atom_name2    = atom(x, "_NET_WM_NAME");
	x->atom_icon2    = atom(x, "_NET_WM_ICON_NAME");
	x->atom_type     = atom(x, "_NET_WM_WINDOW_TYPE");
	x->atom_shell    = atom(x, "_NET_WM_WINDOW_TYPE_NORMAL");
	x->atom_dock     = atom(x, "_NET_WM_WINDOW_TYPE_DOCK");
	x->atom_menu     = atom(x, "_NET_WM_WINDOW_TYPE_POPUP_MENU");
	x->atom_sync     = atom(x, "_NET_WM_SYNC_REQUEST");
	x->atom_sync2    = atom(x, "_NET_WM_SYNC_REQUEST_COUNTER");

	/* end */

	*fd = xcb_get_file_descriptor(x->connection);
	xcb_flush(x->connection);

	return true;

	/* errors */

fail_color:
fail_image:
fail_screen:
	xcb_disconnect(x->connection);
fail_con:
	return false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_server_kill(struct x11 *x)
{
	xcb_free_gc(x->connection, x->shell.gc);
	xcb_disconnect(x->connection);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_shell_close(struct x11 *x)
{
	window_destroy(&x->shell, x);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
x11_shell_open(struct x11 *x, uint32_t w, uint32_t h)
{
	return window_init(&x->shell, x, w, h, false);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_shell_redraw(struct x11 *x)
{
	x->shell.redraw = true;
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
ev_conf(xcb_configure_notify_event_t *xev, struct x11 *x)
{
	struct x11_window *win = window(x, xev->window);
	struct cevent cev =
	{
		.type = CEVENT_TRANSFORM,
		.transform_w = xev->width,
		.transform_h = xev->height,
		.transform_x = xev->x,
		.transform_y = xev->y,
	};

	win->present |= xev->width  < win->buffer_w || xev->height  < win->buffer_h;
	win->resized |= xev->width != win->buffer_w || xev->height != win->buffer_h;
	win->buffer_w = xev->width;
	win->buffer_h = xev->height;

	return win == &x->shell ? cev : cevent_blank;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static struct cevent
ev_expose(xcb_expose_event_t *xev, struct x11 *x)
{
	window(x, xev->window)->present |= xev->count == 0;

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
	uint32_t ev_mask = XCB_EVENT_MASK_SUBSTRUCTURE_REDIRECT | XCB_EVENT_MASK_SUBSTRUCTURE_NOTIFY;
	struct x11_window *win = window(x, xev->window);
	struct cevent cev = cevent_blank;
	xcb_atom_t msg = xev->data.data32[0];

	if (xev->format != 32 || xev->type != x->atom_protocol)
	{
		cev.type = CEVENT_UNKNOWN;
	}
	else if (msg == x->atom_close)
	{
		cev.type = CEVENT_CLOSE;
	}
	else if (msg == x->atom_focus)
	{
		xcb_set_input_focus(x->connection, XCB_INPUT_FOCUS_PARENT, xev->window, xev->data.data32[1]);
	}
	else if (msg == x->atom_ping)
	{
		xev->window = x->screen->root;
		xcb_send_event(x->connection, 0, x->screen->root, ev_mask, (char*)xev);
	}
	else if (msg == x->atom_sync)
	{
		win->sync_val.lo = xev->data.data32[2];
		win->sync_val.hi = xev->data.data32[3];
		win->sync = true;
	}
	else
	{
		cev.type = CEVENT_UNKNOWN;
	}

	return cev;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static struct cevent
ev_present(xcb_present_generic_event_t *xev, struct x11 *x)
{
	xcb_present_complete_notify_event_t *cev = (xcb_present_complete_notify_event_t *)xev;
	xcb_present_idle_notify_event_t     *iev = (xcb_present_idle_notify_event_t     *)xev;
	struct x11_window *win;

	switch (xev->evtype)
	{
		case XCB_PRESENT_EVENT_COMPLETE_NOTIFY:
			win = window(x, cev->window);
			win->wait &= cev->serial != win->serial || cev->kind != XCB_PRESENT_COMPLETE_KIND_PIXMAP;
			break;

		case XCB_PRESENT_EVENT_IDLE_NOTIFY:
			win = window(x, iev->window);
			win->busy &= iev->pixmap != win->buffer;
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

bool
inputs_grab(struct x11 *x)
{
	xcb_grab_keyboard_reply_t *rp1 = nullptr;
	xcb_grab_pointer_reply_t  *rp2 = nullptr;
	xcb_grab_keyboard_cookie_t ck1;
	xcb_grab_pointer_cookie_t  ck2;
	bool fail;

	ck1 = xcb_grab_keyboard(
		x->connection,
		0,
		x->screen->root,
		XCB_CURRENT_TIME,
		XCB_GRAB_MODE_ASYNC,
		XCB_GRAB_MODE_ASYNC);

	ck2 = xcb_grab_pointer(
		x->connection,
		0,
		x->screen->root,
		XCB_EVENT_MASK_BUTTON_PRESS | XCB_EVENT_MASK_BUTTON_RELEASE | XCB_EVENT_MASK_POINTER_MOTION,
		XCB_GRAB_MODE_ASYNC,
		XCB_GRAB_MODE_ASYNC,
		XCB_NONE,
		XCB_NONE,
		XCB_CURRENT_TIME);

	rp1 = xcb_grab_keyboard_reply(x->connection, ck1, nullptr);
	rp2 = xcb_grab_pointer_reply(x->connection,  ck2, nullptr);

	if ((fail = !rp1 || !rp2))
	{
		inputs_ungrab(x);
	}

	free(rp1);
	free(rp2);

	return !fail;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
inputs_ungrab(struct x11 *x)
{
	xcb_ungrab_keyboard(x->connection, XCB_CURRENT_TIME);
	xcb_ungrab_pointer (x->connection, XCB_CURRENT_TIME);
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
		free(rp);
	}

	return opcode;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
position_popup(struct x11 *x, uint32_t w, uint32_t h, int32_t *px, int32_t *py)
{
	uint32_t dw1 = w;
	uint32_t dw2 = w;
	uint32_t dh1 = h;
	uint32_t dh2 = h;

	/* get pointer position */

	xcb_query_pointer_cookie_t ck1;
	xcb_query_pointer_reply_t *rp1;

	ck1 = xcb_query_pointer(x->connection, x->screen->root);
	if (!(rp1 = xcb_query_pointer_reply(x->connection, ck1, nullptr)))
	{
		return;
	}

	*px = rp1->root_x;
	*py = rp1->root_y;
	free(rp1);

	/* stop here if extension is missing */

	if (x->opcode_randr == 0)
	{
		return;
	}

	/* locate the monitor the pointer is on */

	xcb_randr_get_monitors_cookie_t ck2;
	xcb_randr_get_monitors_reply_t *rp2;
	xcb_randr_monitor_info_iterator_t it;

	ck2 = xcb_randr_get_monitors(x->connection, x->screen->root, 1);
	if (!(rp2 = xcb_randr_get_monitors_reply(x->connection, ck2, nullptr)))
	{
		return;
	}

	it = xcb_randr_get_monitors_monitors_iterator(rp2);
	for (; it.rem; xcb_randr_monitor_info_next(&it))
	{
		if (*px > it.data->x && *px < it.data->x + it.data->width
		 && *py > it.data->y && *py < it.data->y + it.data->height)
		{
			dw1 = it.data->x + it.data->width  - *px;
			dh1 = it.data->y + it.data->height - *py;
			dw2 = *px - it.data->x;
			dh2 = *py - it.data->y;
			break;
		}
	}

	free(rp2);

	/* adjust popup position to fit on monitor */

	if (dw1 < w)
	{
		if (dw2 < w)
		{
			*px -= w - dw1;
		}
		else
		{
			*px -= w;
		}
	}

	*px -= dw1 >= w ? 0 : w - (dw2 >= w ? 0 : dw1);
	*py -= dh1 >= h ? 0 : h - (dh2 >= h ? 0 : dh1);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
prop_set(struct x11 *x, struct x11_window *win, xcb_atom_t prop, xcb_atom_t type, uint32_t n, const void *data, bool head)
{
	return fail(x,
		xcb_change_property_checked(
			x->connection,
			head ? XCB_PROP_MODE_REPLACE : XCB_PROP_MODE_APPEND,
			win->window,
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
setup_sync(struct x11 *x)
{
	xcb_sync_initialize_cookie_t ck;
	xcb_sync_initialize_reply_t *rp;

	if (x->opcode_sync == 0)
	{
		return;
	}

	ck = xcb_sync_initialize(x->connection, 3, 1);
	if (!(rp = xcb_sync_initialize_reply(x->connection, ck, nullptr)))
	{
		x->opcode_sync = 0;
	}

	free(rp);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static struct x11_window *
window(struct x11 *x, xcb_window_t xwin)
{
	return xwin == x->shell.window ? &x->shell : &x->menu;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
window_commit(struct x11_window *win, struct x11 *x, cshell *sh)
{
	uint32_t w = win->buffer_w;
	uint32_t h = win->buffer_h;
	struct cevent ev =
	{
		.type = CEVENT_REDRAW,
		.redraw_ctx = win->cairo,
		.redraw_shell = win == &x->shell,
	};

	if (!win->active || win->busy)
	{
		return;
	}

	/* update buffer size */

	if (win->resized)
	{	
		xcb_free_pixmap(x->connection, win->buffer);
		win->buffer = xcb_generate_id(x->connection);
		xcb_create_pixmap(x->connection, x->depth, win->buffer, win->window, w, h);
		cairo_surface_flush(win->surface);
		cairo_xcb_surface_set_drawable(win->surface, win->buffer, w, h);
		win->resized = false;
	}

	/* rendering */

	if (win->redraw)
	{
		win->redraw = false;
		shell_dispatch_event(sh, ev);
		cairo_surface_flush(win->surface);
	}

	if (win->present && !win->wait)
	{
		if (x->opcode_present != 0)
		{
			win->busy    = true;
			win->wait    = true;
			win->present = false;
			xcb_present_pixmap(
				x->connection,
				win->window,
				win->buffer,
				++win->serial,
				XCB_XFIXES_REGION_NONE,
				XCB_XFIXES_REGION_NONE,
				0, 0, 0, 0, 0,
				XCB_PRESENT_OPTION_COPY,
				0, 1, 0, 0,
				nullptr);
		}
		else /* fallback */
		{
			win->present = false;
			xcb_copy_area(x->connection, win->buffer, win->window, win->gc, 0, 0, 0, 0, w, h);
		}

		if (x->opcode_sync != 0 && win->sync)
		{
			win->sync = false;
			xcb_sync_set_counter(x->connection, win->sync_count, win->sync_val);
		}
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
window_destroy(struct x11_window *win, struct x11 *x)
{
	if (win->active)
	{
		cairo_destroy(win->cairo);
		cairo_surface_finish(win->surface);
		cairo_surface_destroy(win->surface);
		xcb_free_gc(x->connection, win->gc);
		xcb_free_pixmap(x->connection, win->buffer);
		xcb_unmap_window(x->connection, win->window);
		xcb_destroy_window(x->connection, win->window);
		xcb_sync_destroy_counter(x->connection, win->sync_count);
		win->active = false;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
window_init(struct x11_window *win, struct x11 *x, uint32_t w, uint32_t h, bool popup)
{
	xcb_void_cookie_t ck;

	if (win->active)
	{
		return false;
	}

	/* window setup */

	int32_t px = 0;
	int32_t py = 0;

	const uint32_t win_opt = 
		  XCB_CW_BACK_PIXMAP
		| XCB_CW_BORDER_PIXEL
		| XCB_CW_BIT_GRAVITY
		| XCB_CW_OVERRIDE_REDIRECT
		| XCB_CW_EVENT_MASK
		| XCB_CW_COLORMAP;

	const uint32_t win_val[] =
	{
		  XCB_BACK_PIXMAP_NONE,
		  0x00000000,
		  XCB_GRAVITY_NORTH_WEST,
		  popup,
		  XCB_EVENT_MASK_EXPOSURE
		| XCB_EVENT_MASK_STRUCTURE_NOTIFY
		| XCB_EVENT_MASK_BUTTON_PRESS
		| XCB_EVENT_MASK_BUTTON_RELEASE,
		  x->colormap,
	};

	if (popup)
	{
		position_popup(x, w, h, &px, &py);
	}

	win->window = xcb_generate_id(x->connection);
	ck = xcb_create_window_checked(
		x->connection,
		x->depth,
		win->window,
		x->screen->root,
		px, py, w, h, 0,
		XCB_WINDOW_CLASS_INPUT_OUTPUT,
		x->visual->visual_id,
		win_opt,
		win_val);

	if (fail(x, ck))
	{
		goto fail_win;
	}

	/* sync setup */

	win->sync_count = xcb_generate_id(x->connection);
	ck = xcb_sync_create_counter_checked(x->connection, win->sync_count, win->sync_val);

	if (fail(x, ck))
	{
		goto fail_sync;
	}

	/* buffer setup */

	win->buffer = xcb_generate_id(x->connection);
	ck = xcb_create_pixmap_checked(
		x->connection,
		x->depth,
		win->buffer,
		win->window,
		w, h);

	if (fail(x, ck))
	{
		goto fail_buf;
	}

	/* gc setup for present extension fallback */

	const uint32_t gc_opt   = XCB_GC_FOREGROUND;
	const uint32_t gc_val[] = {0x00000000};

	win->gc = xcb_generate_id(x->connection);
	ck = xcb_create_gc_checked(x->connection, win->gc, win->buffer, gc_opt, gc_val);

	if (fail(x, ck))
	{
		goto fail_gc;
	}

	/* cairo setup */

	win->surface = cairo_xcb_surface_create(x->connection, win->buffer, x->visual, w, h);
	if (cairo_surface_status(win->surface) != CAIRO_STATUS_SUCCESS)
	{
		goto fail_sfc;
	}

	win->cairo = cairo_create(win->surface);
	if (cairo_status(win->cairo) != CAIRO_STATUS_SUCCESS)
	{
		goto fail_ctx;
	}

	/* register window to extension events */

	if (x->opcode_present != 0)
	{
		ck = xcb_present_select_input_checked(
			  x->connection,
			  xcb_generate_id(x->connection),
			  win->window, 
			  XCB_PRESENT_EVENT_MASK_IDLE_NOTIFY
			| XCB_PRESENT_EVENT_MASK_COMPLETE_NOTIFY);

		if (fail(x, ck))
		{
			goto fail_ev;
		}
	}
	
	/* ICCCM and EWMH properties setup */

	const char *name  = "shell"; // TODO set as arg
	const char *class = "cgui";  // TODO set as arg
	const char *tag   = "tag";   // TODO set as arg

	xcb_atom_t atom_type = popup ? x->atom_shell : x->atom_menu;
	char host[256] = "";
	uint32_t pid;
	size_t tag_n;
	size_t name_n;
	size_t host_n;
	size_t class_n;

	gethostname(host, 256);

	class_n = strlen(class) + 1;
	tag_n   = strlen(tag)   + 1;
	host_n  = strlen(host);
	name_n  = strlen(name);
	pid     = getpid();

	prop_set(x, win, x->atom_protocol, XCB_ATOM_ATOM,     1,       &x->atom_close, true);
	prop_set(x, win, x->atom_protocol, XCB_ATOM_ATOM,     1,       &x->atom_focus, false);
	prop_set(x, win, x->atom_protocol, XCB_ATOM_ATOM,     1,       &x->atom_ping,  false);
	prop_set(x, win, x->atom_name2,    x->atom_utf8,      name_n,  name,           true);
	prop_set(x, win, x->atom_icon2,    x->atom_utf8,      name_n,  name,           true);
	prop_set(x, win, x->atom_name,     XCB_ATOM_STRING,   name_n,  name,           true);
	prop_set(x, win, x->atom_icon,     XCB_ATOM_STRING,   name_n,  name,           true);
	prop_set(x, win, x->atom_class,    XCB_ATOM_STRING,   tag_n,   tag,            true);
	prop_set(x, win, x->atom_class,    XCB_ATOM_STRING,   class_n, class,          false);
	prop_set(x, win, x->atom_type,     XCB_ATOM_ATOM,     1,       &atom_type,     true);
	prop_set(x, win, x->atom_lead,     XCB_ATOM_WINDOW,   1,       &win->window,   true);
	prop_set(x, win, x->atom_pid,      XCB_ATOM_CARDINAL, 1,       &pid,           true);
	prop_set(x, win, x->atom_host,     XCB_ATOM_STRING,   host_n,  host,           true);

	if (x->opcode_sync != 0)
	{
		prop_set(x, win, x->atom_protocol, XCB_ATOM_ATOM,     1, &x->atom_sync,    false);
		prop_set(x, win, x->atom_sync2,    XCB_ATOM_CARDINAL, 1, &win->sync_count, true);
	}

	/* end */

	if (fail(x, xcb_map_window_checked(x->connection, win->window)))
	{
		goto fail_ev;
	}

	win->sync_val.hi = 0;
	win->sync_val.lo = 0;
	win->buffer_w    = w;
	win->buffer_h    = h;
	win->serial      = 0;
	win->redraw      = true;
	win->resized     = false;
	win->present     = false;
	win->busy        = false;
	win->wait        = false;
	win->sync        = false;
	win->active      = true;

	xcb_flush(x->connection);

	return true;

	/* errors */

fail_ev:
	cairo_destroy(win->cairo);
fail_ctx:
	cairo_surface_destroy(win->surface);
fail_sfc:
	xcb_free_gc(x->connection, win->gc);
fail_gc:
	xcb_free_pixmap(x->connection, win->buffer);
fail_buf:
	xcb_sync_destroy_counter(x->connection, win->sync_count);
fail_sync:
	xcb_destroy_window(x->connection, win->window);
fail_win:
	return false;
}

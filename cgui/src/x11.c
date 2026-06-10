/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#include <cairo/cairo.h>
#include <cairo/cairo-xcb.h>
#include <cassette/cgui.h>
#include <cassette/ccfg.h>
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
#include <xcb/xcb_icccm.h>
#include <xcb/xcb_renderutil.h>
#include <xcb/xfixes.h>

#include "event.h"
#include "shell.h"
#include "x11.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void ev_button  (struct x11 *, xcb_button_press_event_t     *, bool);
static void ev_conf    (struct x11 *, xcb_configure_notify_event_t *);
static void ev_expose  (struct x11 *, xcb_expose_event_t           *);
static void ev_ext     (struct x11 *, xcb_ge_generic_event_t       *);
static void ev_map     (struct x11 *, xcb_map_notify_event_t       *);
static void ev_msg     (struct x11 *, xcb_client_message_event_t   *);
static void ev_present (struct x11 *, xcb_present_generic_event_t  *);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool inputs_grab   (struct x11 *);
static void inputs_ungrab (struct x11 *);
static void move_popup    (struct x11 *, uint32_t, uint32_t, int32_t *, int32_t *);
static void prop_set      (struct x11 *, struct x11_window *, xcb_atom_t, xcb_atom_t, uint32_t, const void *, bool);
static void setup_render  (struct x11 *);
static void setup_sync    (struct x11 *);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static xcb_atom_t         atom   (struct x11 *, const char *);
static uint8_t            opcode (struct x11 *, const char *);
static enum shell_target  target (struct x11 *, xcb_window_t);
static struct x11_window *window (struct x11 *, xcb_window_t);

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

void
x11_commit(struct x11 *x11, enum shell_target target)
{
	struct x11_window *win = target == SHELL_MAIN ? &x11->main : &x11->menu;

	if (!win->active || !win->mapped || win->busy)
	{
		return;
	}

	/* update buffer size */

	if (win->resized && win->w > 0 && win->h > 0)
	{
		xcb_free_pixmap(x11->connection, win->buffer);
		win->buffer = xcb_generate_id(x11->connection);
		xcb_create_pixmap(x11->connection, x11->depth, win->buffer, win->window, win->w, win->h);
		cairo_xcb_surface_set_drawable(win->surface, win->buffer, win->w, win->h);
		win->resized = false;
		win->damaged = true;
	}

	/* redraw buffer */

	struct cevent ev =
	{
		.type     = CEVENT_DRAW,
		.drawable = win->cairo,
	};

	if (win->damaged)
	{
		win->damaged = false;
		shell_send_event(ev, target);
		cairo_surface_flush(win->surface);
	}

	/* present buffer */

	if (win->present && !win->wait)
	{
		win->present = false;
		if (x11->opcode_present != 0)
		{
			win->busy = true;
			win->wait = true;
			xcb_present_pixmap(
				x11->connection,
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
			xcb_copy_area(
				x11->connection,
				win->buffer,
				win->window,
				win->gc,
				0, 0, 0, 0,
				win->w, 
				win->h);
		}
	}

	/* ICCCM sync protocol handling */

	if (x11->opcode_sync != 0 && win->sync && !win->present && !win->resized)
	{
		xcb_sync_set_counter(x11->connection, win->sync_count, win->sync_val);
		win->sync = false;
	}

	/* done */

	xcb_flush(x11->connection);
	if (xcb_connection_has_error(x11->connection))
	{
		shell_send_event(event_error, SHELL_MAIN);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_config(struct x11 *x11, ccfg *cfg)
{
	(void)x11;
	(void)cfg;

	/* no backend-specific options */
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_damage(struct x11 *x11, enum shell_target target)
{
	struct x11_window *win = target == SHELL_MAIN ? &x11->main : &x11->menu;

	win->damaged = true;
	win->present = true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_hide(struct x11 *x11, enum shell_target target)
{
	struct x11_window *win = target == SHELL_MAIN ? &x11->main : &x11->menu;

	if (!win->active)
	{
		return;
	}

	if (x11->opcode_sync != 0)
	{
		xcb_sync_destroy_counter(x11->connection, win->sync_count);
	}

	cairo_destroy(win->cairo);
	cairo_surface_finish(win->surface);
	cairo_surface_destroy(win->surface);

	xcb_free_gc(x11->connection, win->gc);
	xcb_free_pixmap(x11->connection, win->buffer);
	xcb_unmap_window(x11->connection, win->window);
	xcb_destroy_window(x11->connection, win->window);

	inputs_ungrab(x11);

	win->active = false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_hint(struct x11 *x11, uint32_t w, uint32_t h)
{
	xcb_size_hints_t hints =
	{
		.min_width  = w,
		.min_height = h,
		.flags      = XCB_ICCCM_SIZE_HINT_P_MIN_SIZE,
	};

	if (x11->main.active)
	{
		prop_set(
			 x11,
			&x11->main,
			XCB_ATOM_WM_NORMAL_HINTS,
			XCB_ATOM_WM_SIZE_HINTS,
			sizeof(hints),
			&hints,
			true);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

int
x11_init(struct x11 *x11)
{
	*x11 = (struct x11){0};

	/* base setup */

	if (xcb_connection_has_error(x11->connection = xcb_connect(nullptr, nullptr)))
	{
		goto fail_con;
	}

	if (!(x11->screen = xcb_setup_roots_iterator(xcb_get_setup(x11->connection)).data))
	{
		goto fail_screen;
	}

	/* atoms setup */

	x11->atom_clip     = atom(x11, "CLIPBOARD");
	x11->atom_multiple = atom(x11, "MULTIPLE");
	x11->atom_target   = atom(x11, "TARGETS");
	x11->atom_utf8     = atom(x11, "UTF8_STRING");
	x11->atom_time     = atom(x11, "TIMESTAMP");
	x11->atom_protocol = atom(x11, "WM_PROTOCOLS");
	x11->atom_close    = atom(x11, "WM_DELETE_WINDOW");
	x11->atom_focus    = atom(x11, "WM_TAKE_FOCUS");
	x11->atom_name     = atom(x11, "WM_NAME");
	x11->atom_icon     = atom(x11, "WM_ICON_NAME");
	x11->atom_class    = atom(x11, "WM_CLASS");
	x11->atom_cmd      = atom(x11, "WM_COMMAND");
	x11->atom_host     = atom(x11, "WM_CLIENT_MACHINE");
	x11->atom_lead     = atom(x11, "WM_CLIENT_LEADER");
	x11->atom_ping     = atom(x11, "_NET_WM_PING");
	x11->atom_pid      = atom(x11, "_NET_WM_PID");
	x11->atom_name2    = atom(x11, "_NET_WM_NAME");
	x11->atom_icon2    = atom(x11, "_NET_WM_ICON_NAME");
	x11->atom_type     = atom(x11, "_NET_WM_WINDOW_TYPE");
	x11->atom_shell    = atom(x11, "_NET_WM_WINDOW_TYPE_NORMAL");
	x11->atom_dock     = atom(x11, "_NET_WM_WINDOW_TYPE_DOCK");
	x11->atom_menu     = atom(x11, "_NET_WM_WINDOW_TYPE_POPUP_MENU");
	x11->atom_sync     = atom(x11, "_NET_WM_SYNC_REQUEST");
	x11->atom_sync2    = atom(x11, "_NET_WM_SYNC_REQUEST_COUNTER");

	/* extensions check */

	x11->opcode_present = opcode(x11, "Present");
	x11->opcode_render  = opcode(x11, "RENDER");
	x11->opcode_xinput  = opcode(x11, "XInputExtension");
	x11->opcode_randr   = opcode(x11, "RANDR");
	x11->opcode_sync    = opcode(x11, "SYNC");

	/* extensions init */

	setup_sync(x11);
	setup_render(x11);

	/* colormap setup for custom visuals */

	x11->colormap = xcb_generate_id(x11->connection);
	xcb_create_colormap(
		x11->connection,
		XCB_COLORMAP_ALLOC_NONE,
		x11->colormap,
		x11->screen->root,
		x11->visual->visual_id);

	/* end */

	xcb_flush(x11->connection);

	return xcb_get_file_descriptor(x11->connection);

	/* errors */

fail_screen:
	xcb_disconnect(x11->connection);
fail_con:
	return -1;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_kill(struct x11 *x11)
{
	xcb_disconnect(x11->connection);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_read(struct x11 *x11)
{
	xcb_generic_event_t *ev;

	while ((ev = xcb_poll_for_event(x11->connection)))
	{
		switch (ev->response_type & ~0x80)
		{
			case XCB_BUTTON_PRESS:
				ev_button(x11, (xcb_button_press_event_t *)ev, true);
				break;

			case XCB_BUTTON_RELEASE:
				ev_button(x11, (xcb_button_press_event_t *)ev, false);
				break;

			case XCB_CLIENT_MESSAGE:
				ev_msg(x11, (xcb_client_message_event_t *)ev);
				break;

			case XCB_CONFIGURE_NOTIFY:
				ev_conf(x11, (xcb_configure_notify_event_t *)ev);
				break;

			case XCB_MAP_NOTIFY:
				ev_map(x11, (xcb_map_notify_event_t *)ev);
				break;

			case XCB_GE_GENERIC:
				ev_ext(x11, (xcb_ge_generic_event_t *)ev);
				break;
				
			case XCB_EXPOSE:
				ev_expose(x11, (xcb_expose_event_t *)ev);
				break;

			case 0:
				shell_send_event(event_error, SHELL_MAIN);
				break;

			default:
				break;
		}
		free(ev);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_rename(struct x11 *x11, enum shell_target target, const char *name)
{
	struct x11_window *win = target == SHELL_MAIN ? &x11->main : &x11->menu;
	size_t name_n = strlen(name);

	prop_set(x11, win, x11->atom_name2, x11->atom_utf8,  name_n, name, true);
	prop_set(x11, win, x11->atom_icon2, x11->atom_utf8,  name_n, name, true);
	prop_set(x11, win, x11->atom_name,  XCB_ATOM_STRING, name_n, name, true);
	prop_set(x11, win, x11->atom_icon,  XCB_ATOM_STRING, name_n, name, true);

	xcb_flush(x11->connection);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
x11_show(struct x11 *x11, enum shell_target target, const char *tag, uint32_t w, uint32_t h)
{
	struct x11_window *win = target == SHELL_MAIN ? &x11->main : &x11->menu;
	int32_t x = 0;
	int32_t y = 0;

	if (win->active)
	{
		return;
	}

	/* popup specifics */

	if (target == SHELL_MENU)
	{
		if (!inputs_grab(x11))
		{
			goto fail_grab;
		}
		move_popup(x11, w, h, &x, &y);
	}

	/* window setup */

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
		  target == SHELL_MENU,
		  XCB_EVENT_MASK_EXPOSURE
		| XCB_EVENT_MASK_STRUCTURE_NOTIFY
		| XCB_EVENT_MASK_BUTTON_PRESS
		| XCB_EVENT_MASK_BUTTON_RELEASE,
		  x11->colormap,
	};

	win->window = xcb_generate_id(x11->connection);
	xcb_create_window(
		x11->connection, 
		x11->depth,
		win->window,
		x11->screen->root,
		x, y, w, h, 0,
		XCB_WINDOW_CLASS_INPUT_OUTPUT,
		x11->visual->visual_id,
		win_opt,
		win_val);

	/* drawing buffer setup */

	win->buffer = xcb_generate_id(x11->connection);
	xcb_create_pixmap(
		x11->connection,
		x11->depth,
		win->buffer,
		win->window,
		w, h);

	/* graphic context setup for present extension fallback */

	const uint32_t gc_opt   = XCB_GC_FOREGROUND;
	const uint32_t gc_val[] = {0x00000000};

	win->gc = xcb_generate_id(x11->connection);
	xcb_create_gc(
		x11->connection,
		win->gc,
		win->buffer,
		gc_opt,
		gc_val);

	/* cairo setup */

	win->surface = cairo_xcb_surface_create(x11->connection, win->buffer, x11->visual, w, h);
	if (cairo_surface_status(win->surface) != CAIRO_STATUS_SUCCESS)
	{
		goto fail_surface;
	}

	win->cairo = cairo_create(win->surface);
	if (cairo_status(win->cairo) != CAIRO_STATUS_SUCCESS)
	{
		goto fail_cairo;
	}

	/* register window to receive present extension events */

	if (x11->opcode_present != 0)
	{
		xcb_present_select_input(
			  x11->connection,
			  xcb_generate_id(x11->connection),
			  win->window,
			  XCB_PRESENT_EVENT_MASK_IDLE_NOTIFY
			| XCB_PRESENT_EVENT_MASK_COMPLETE_NOTIFY);
	}

	/* counter setup for ICCCM sync protocol */

	if (x11->opcode_sync != 0)
	{
		win->sync_count = xcb_generate_id(x11->connection);
		xcb_sync_create_counter(
			x11->connection,
			win->sync_count,
			win->sync_val);
	}

	/* ICCCM and EWMH X properties setup */

	xcb_atom_t atom_type = target == SHELL_MAIN ? x11->atom_shell : x11->atom_menu;
	const char *class = "cgui";
	char host[256] = "";
	size_t class_n;
	size_t host_n;
	size_t tag_n;
	uint32_t pid;

	gethostname(host, 256);

	class_n = strlen(class) + 1;
	tag_n   = strlen(tag) + 1;
	host_n  = strlen(host);
	pid     = getpid();

	prop_set(x11, win, x11->atom_protocol, XCB_ATOM_ATOM,     1,       &x11->atom_close, true);
	prop_set(x11, win, x11->atom_protocol, XCB_ATOM_ATOM,     1,       &x11->atom_focus, false);
	prop_set(x11, win, x11->atom_protocol, XCB_ATOM_ATOM,     1,       &x11->atom_ping,  false);
	prop_set(x11, win, x11->atom_class,    XCB_ATOM_STRING,   class_n, class,            true);
	prop_set(x11, win, x11->atom_class,    XCB_ATOM_STRING,   tag_n,   tag,              false);
	prop_set(x11, win, x11->atom_type,     XCB_ATOM_ATOM,     1,       &atom_type,       true);
	prop_set(x11, win, x11->atom_lead,     XCB_ATOM_WINDOW,   1,       &win->window,     true);
	prop_set(x11, win, x11->atom_pid,      XCB_ATOM_CARDINAL, 1,       &pid,             true);
	prop_set(x11, win, x11->atom_host,     XCB_ATOM_STRING,   host_n,  host,             true);

	if (x11->opcode_sync != 0)
	{
		prop_set(x11, win, x11->atom_protocol, XCB_ATOM_ATOM,     1, &x11->atom_sync,  false);
		prop_set(x11, win, x11->atom_sync2,    XCB_ATOM_CARDINAL, 1, &win->sync_count, true);
	}

	/* finish */

	win->w           = 0;
	win->h           = 0;
	win->sync_val.hi = 0;
	win->sync_val.lo = 0;
	win->serial      = 0;
	win->busy        = false;
	win->wait        = false;
	win->sync        = false;
	win->mapped      = false;
	win->present     = false;
	win->resized     = false;
	win->damaged     = true; 
	win->active      = true;

	xcb_map_window(x11->connection, win->window);
	xcb_flush(x11->connection);

	return;

	/* errors */

fail_cairo:
	cairo_surface_destroy(win->surface);
fail_surface:
	xcb_free_gc(x11->connection, win->gc);
	xcb_free_pixmap(x11->connection, win->buffer);
	xcb_destroy_window(x11->connection, win->window);
fail_grab:
	shell_send_event(event_error, target);
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static xcb_atom_t
atom(struct x11 *x11, const char *name)
{
	xcb_intern_atom_cookie_t ck;
	xcb_intern_atom_reply_t *rp;
	xcb_atom_t at;

	ck = xcb_intern_atom(x11->connection, 0, strlen(name), name);
	rp = xcb_intern_atom_reply(x11->connection, ck, nullptr);
	if (!rp)
	{
		return 0;
	}

	at = rp->atom;
	free(rp);

	return at;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_button(struct x11 *x11, xcb_button_press_event_t *ev, bool press)
{
	struct cevent cev =
	{
		.type   = press ? CEVENT_BUTTON_PRESS : CEVENT_BUTTON_RELEASE,
		.button = ev->detail,
	};

	shell_send_event(cev, x11->menu.active ? SHELL_MENU : SHELL_MAIN);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_conf(struct x11 *x11, xcb_configure_notify_event_t *ev)
{
	enum shell_target type = target(x11, ev->window);
	struct x11_window *win = window(x11, ev->window);
	struct cevent cev =
	{
		.type    = CEVENT_SHAPE,
		.shape_h = ev->height,
		.shape_w = ev->width,
		.shape_x = 0,
		.shape_y = 0,
	};

	if (win->h == ev->height
	 && win->w == ev->width)
	{
		return;
	}

	win->resized  = true;
	win->present |= ev->width  < win->w || ev->height  < win->h;
	win->h        = ev->height;
	win->w        = ev->width;

	shell_send_event(cev, type);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_expose(struct x11 *x11, xcb_expose_event_t *ev)
{
	window(x11, ev->window)->present |= ev->count == 0;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_ext(struct x11 *x11, xcb_ge_generic_event_t *ev)
{
	if (ev->extension == x11->opcode_present)
	{
		ev_present(x11, (xcb_present_generic_event_t *)ev);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_map(struct x11 *x11, xcb_map_notify_event_t *ev)
{	
	enum shell_target type = target(x11, ev->window);
	struct x11_window *win = window(x11, ev->window);

	if (!win->mapped)
	{
		win->mapped = true;
		shell_send_event(event_open, type);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_msg(struct x11 *x11, xcb_client_message_event_t *ev)
{
	enum shell_target type = target(x11, ev->window);
	struct x11_window *win = window(x11, ev->window);
	xcb_atom_t msg = ev->data.data32[0];

	if (ev->format != 32 || ev->type != x11->atom_protocol)
	{
		return;
	}
	else if (msg == x11->atom_close)
	{
		shell_send_event(event_close, type);
	}
	else if (msg == x11->atom_sync)
	{
		win->sync_val.lo = ev->data.data32[2];
		win->sync_val.hi = ev->data.data32[3];
		win->sync        = true;
	}
	else if (msg == x11->atom_focus)
	{
		xcb_set_input_focus_checked(
			x11->connection,
			XCB_INPUT_FOCUS_PARENT,
			ev->window,
			ev->data.data32[1]);
	}
	else if (msg == x11->atom_ping)
	{
		ev->window = x11->screen->root;
		xcb_send_event(
			  x11->connection,
			  0,
			  x11->screen->root,
			  XCB_EVENT_MASK_SUBSTRUCTURE_REDIRECT
			| XCB_EVENT_MASK_SUBSTRUCTURE_NOTIFY,
			  (char*)ev);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_present(struct x11 *x11, xcb_present_generic_event_t *ev)
{
	xcb_present_complete_notify_event_t *cev = (xcb_present_complete_notify_event_t *)ev;
	xcb_present_idle_notify_event_t     *iev = (xcb_present_idle_notify_event_t     *)ev;
	struct x11_window *win;

	switch (ev->evtype)
	{
		case XCB_PRESENT_EVENT_COMPLETE_NOTIFY:
			win = window(x11, cev->window);
			win->wait &= cev->serial != win->serial || cev->kind != XCB_PRESENT_COMPLETE_KIND_PIXMAP;
			break;

		case XCB_PRESENT_EVENT_IDLE_NOTIFY:
			win = window(x11, iev->window);
			win->busy &= iev->pixmap != win->buffer;
			break;

		default:
			break;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
inputs_grab(struct x11 *x11)
{
	xcb_grab_keyboard_reply_t *rp1 = nullptr;
	xcb_grab_pointer_reply_t  *rp2 = nullptr;
	xcb_grab_keyboard_cookie_t ck1;
	xcb_grab_pointer_cookie_t  ck2;
	bool fail;

	ck1 = xcb_grab_keyboard(
		x11->connection,
		0,
		x11->screen->root,
		XCB_CURRENT_TIME,
		XCB_GRAB_MODE_ASYNC,
		XCB_GRAB_MODE_ASYNC);

	ck2 = xcb_grab_pointer(
		x11->connection,
		0,
		x11->screen->root,
		XCB_EVENT_MASK_BUTTON_PRESS | XCB_EVENT_MASK_BUTTON_RELEASE | XCB_EVENT_MASK_POINTER_MOTION,
		XCB_GRAB_MODE_ASYNC,
		XCB_GRAB_MODE_ASYNC,
		XCB_NONE,
		XCB_NONE,
		XCB_CURRENT_TIME);

	rp1 = xcb_grab_keyboard_reply(x11->connection, ck1, nullptr);
	rp2 = xcb_grab_pointer_reply (x11->connection, ck2, nullptr);

	if ((fail = !rp1 || !rp2))
	{
		inputs_ungrab(x11);
	}

	free(rp1);
	free(rp2);

	return !fail;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
inputs_ungrab(struct x11 *x11)
{
	xcb_ungrab_keyboard(x11->connection, XCB_CURRENT_TIME);
	xcb_ungrab_pointer (x11->connection, XCB_CURRENT_TIME);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
move_popup(struct x11 *x11, uint32_t w, uint32_t h, int32_t *x, int32_t *y)
{
	uint32_t dw1 = w;
	uint32_t dw2 = w;
	uint32_t dh1 = h;
	uint32_t dh2 = h;

	/* get pointer position */

	xcb_query_pointer_cookie_t ck1;
	xcb_query_pointer_reply_t *rp1;

	ck1 = xcb_query_pointer(x11->connection, x11->screen->root);
	if (!(rp1 = xcb_query_pointer_reply(x11->connection, ck1, nullptr)))
	{
		return;
	}

	*x = rp1->root_x;
	*y = rp1->root_y;
	free(rp1);

	/* stop here if the randr extension is missing */

	if (x11->opcode_randr == 0)
	{
		return;
	}

	/* locate the monitor the pointer is on */

	xcb_randr_get_monitors_cookie_t ck2;
	xcb_randr_get_monitors_reply_t *rp2;
	xcb_randr_monitor_info_iterator_t it;

	ck2 = xcb_randr_get_monitors(x11->connection, x11->screen->root, 1);
	if (!(rp2 = xcb_randr_get_monitors_reply(x11->connection, ck2, nullptr)))
	{
		return;
	}

	it = xcb_randr_get_monitors_monitors_iterator(rp2);
	for (; it.rem; xcb_randr_monitor_info_next(&it))
	{
		if (*x > it.data->x && *x < it.data->x + it.data->width
		 && *y > it.data->y && *y < it.data->y + it.data->height)
		{
			dw1 = it.data->x + it.data->width  - *x;
			dh1 = it.data->y + it.data->height - *y;
			dw2 = *x - it.data->x;
			dh2 = *y - it.data->y;
			break;
		}
	}

	free(rp2);

	/* adjust popup position to fit inside monitor */

	*x -= dw1 >= w ? 0 : w - (dw2 >= w ? 0 : dw1);
	*y -= dh1 >= h ? 0 : h - (dh2 >= h ? 0 : dh1);

	/* send a shape event manually                           */
	/* because override redirect windows get no configure events */

	struct cevent ev = 
	{
		.type    = CEVENT_SHAPE,
		.shape_x = *x,
		.shape_y = *y,
		.shape_w =  w,
		.shape_h =  h,
	};

	shell_send_event(ev, SHELL_MENU);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static uint8_t
opcode(struct x11 *x11, const char *name)
{
	xcb_query_extension_cookie_t ck;
	xcb_query_extension_reply_t *rp;
	uint8_t opcode = 0;

	ck = xcb_query_extension(x11->connection, strlen(name), name);
	if ((rp = xcb_query_extension_reply(x11->connection, ck, nullptr)))
	{
		opcode = rp->major_opcode;
		free(rp);
	}

	return opcode;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
prop_set(struct x11 *x11, struct x11_window *win, xcb_atom_t prop, xcb_atom_t type, uint32_t n, const void *data, bool head)
{
	xcb_change_property(
		x11->connection,
		head ? XCB_PROP_MODE_REPLACE : XCB_PROP_MODE_APPEND,
		win->window,
		prop,
		type,
		type == x11->atom_utf8 || type == x11->atom_time || type == XCB_ATOM_STRING ? 8 : 32,
		n,
		data);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wanalyzer-double-free"

static void
setup_render(struct x11 *x11)
{
	xcb_render_query_pict_formats_reply_t *rp = nullptr;
	xcb_visualid_t id;

	/* fallback visual */

	id = x11->screen->root_visual;
	if (x11->opcode_render == 0)
	{
		goto done;
	}

	/* find xrender visual with alpha */

	xcb_render_query_pict_formats_cookie_t ck;
	xcb_render_pictscreen_iterator_t it_screen;
	xcb_render_pictdepth_iterator_t it_depth;
	xcb_render_pictvisual_iterator_t it_visual;
	xcb_render_pictforminfo_t *format;

	ck = xcb_render_query_pict_formats(x11->connection);
	if (!(rp = xcb_render_query_pict_formats_reply(x11->connection, ck, nullptr))
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
	x11->depth  = xcb_aux_get_depth_of_visual(x11->screen, id);
	x11->visual = xcb_aux_find_visual_by_id(x11->screen, id);
}

#pragma GCC diagnostic pop

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
setup_sync(struct x11 *x11)
{
	xcb_sync_initialize_cookie_t ck;
	xcb_sync_initialize_reply_t *rp;

	if (x11->opcode_sync == 0)
	{
		return;
	}

	ck = xcb_sync_initialize(x11->connection, 3, 1);
	if (!(rp = xcb_sync_initialize_reply(x11->connection, ck, nullptr)))
	{
		x11->opcode_sync = 0;
	}

	free(rp);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static enum shell_target
target(struct x11 *x11, xcb_window_t id)
{
	return id == x11->menu.window ? SHELL_MENU : SHELL_MAIN;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static struct x11_window *
window(struct x11 *x11, xcb_window_t id)
{
	return id == x11->menu.window ? &x11->menu : &x11->main;
}

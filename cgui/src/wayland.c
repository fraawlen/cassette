/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#include <cairo/cairo.h>
#include <cassette/cgui.h>
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdckdint.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>
#include <wayland-client.h>

#include "event.h"
#include "shell.h"
#include "wayland.h"
#include "xdg-shell.h"
#include "xdg-decoration-unstable-v1.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define FREE(INT, FN) if (INT) { FN(INT); }
#define BIND(REG, ID, NAME, VER, TARGET, FACE) \
	if (!strcmp(NAME, FACE.name)) { TARGET = wl_registry_bind(REG, ID, &FACE, VER); return; }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void ev_axis      (void *, struct wl_pointer   *, uint32_t, uint32_t, wl_fixed_t);
static void ev_bind      (void *, struct wl_registry  *, uint32_t, const char *, uint32_t);
static void ev_button    (void *, struct wl_pointer   *, uint32_t, uint32_t, uint32_t, uint32_t);
static void ev_close_pop (void *, struct xdg_popup    *);
static void ev_close_top (void *, struct xdg_toplevel *);
static void ev_conf_base (void *, struct xdg_surface  *, uint32_t);
static void ev_conf_pop  (void *, struct xdg_popup    *, int, int, int, int);
static void ev_conf_top  (void *, struct xdg_toplevel *, int, int, struct wl_array *);
static void ev_enter     (void *, struct wl_pointer   *, uint32_t, struct wl_surface *, wl_fixed_t, wl_fixed_t);
static void ev_frame     (void *, struct wl_callback  *, uint32_t);
static void ev_leave     (void *, struct wl_pointer   *, uint32_t, struct wl_surface *);
static void ev_motion    (void *, struct wl_pointer   *, uint32_t, wl_fixed_t, wl_fixed_t);
static void ev_ping      (void *, struct xdg_wm_base  *, uint32_t);
static void ev_release   (void *, struct wl_buffer    *);
static void ev_seat      (void *, struct wl_seat      *, uint32_t);
static void ev_unbind    (void *, struct wl_registry  *, uint32_t);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void buffer_free   (struct wayland_buffer *);
static bool buffer_resize (struct wayland_buffer *, struct wayland *, uint32_t, uint32_t);
static void flush         (struct wayland *);
static void init_menu     (struct wayland *, struct wayland_window *);
static void init_shell    (struct wayland *, struct wayland_window *, const char *);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static const struct xdg_surface_listener ear_base =
{
	.configure = ev_conf_base,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const struct wl_buffer_listener ear_buffer =
{
	.release = ev_release,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const struct wl_callback_listener ear_frame =
{
	.done = ev_frame,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const struct wl_pointer_listener ear_pointer =
{
	.enter  = ev_enter,
	.leave  = ev_leave,
	.motion = ev_motion,
	.button = ev_button,
	.axis   = ev_axis,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const struct xdg_popup_listener ear_pop =
{
	.configure  = ev_conf_pop,
	.popup_done = ev_close_pop,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const struct wl_registry_listener ear_reg =
{
	.global        = ev_bind,
	.global_remove = ev_unbind,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const struct wl_seat_listener ear_seat =
{
	.capabilities = ev_seat,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const struct xdg_toplevel_listener ear_top =
{
	.configure = ev_conf_top,
	.close     = ev_close_top,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const struct xdg_wm_base_listener ear_xdg =
{
	.ping = ev_ping,
};

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

void
wayland_damage(struct wayland *wl, enum shell_target target)
{
	struct wayland_window *win = target == SHELL_MAIN ? &wl->main : &wl->menu;

	win->redraw = true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
wayland_commit(struct wayland *wl, enum shell_target target)
{
	struct cevent ev = { .type = CEVENT_REDRAW, };
	struct wayland_window *win = target == SHELL_MAIN ? &wl->main : &wl->menu;
	struct wayland_buffer *buf = nullptr;
	struct wl_callback *cl;

	if (!win->active)
	{
		return;
	}

	/* grab free buffer to update */

	if (win->redraw && !win->wait)
	{
		for (size_t i = 0; i < WAYLAND_BUFFER_N; i++)
		{
			if (!(buf = win->buffers + i)->busy)
			{
				break;
			}
		}
	}

	/* update selected buffer */

	if (buf && !buf->busy)
	{
		if (!buffer_resize(buf, wl, win->w, win->h)
		 || !(cl = wl_surface_frame(win->surface)))
		{
			shell_send_event(event_error, SHELL_MAIN);
			return;
		}

		win->redraw = false;
		win->commit = true;
		win->wait   = true;
		buf->busy   = true;

		ev.redraw_ctx = buf->cairo;
		shell_send_event(ev, target);
		cairo_surface_flush(buf->surface);

		wl_callback_add_listener(cl, &ear_frame, win);
		wl_surface_attach(win->surface, buf->handle, 0, 0);
		wl_surface_damage_buffer(win->surface, 0, 0, buf->w, buf->h);
		xdg_surface_set_window_geometry(win->base, 0, 0, win->w, win->h);
	}

	/* done */

	if (win->commit)
	{
		wl_surface_commit(win->surface);
		win->commit = false;
	}

	flush(wl);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
wayland_hide(struct wayland *wl, enum shell_target target)
{
	struct wayland_window *win = target == SHELL_MAIN ? &wl->main : &wl->menu;

	if (!win->active)
	{
		return;
	}

	for (int i = 0; i < WAYLAND_BUFFER_N; i++)
	{
		buffer_free(win->buffers + i);
	}

	FREE(win->ssd, zxdg_toplevel_decoration_v1_destroy);
	FREE(win->top, xdg_toplevel_destroy);
	FREE(win->pop, xdg_popup_destroy);
	FREE(win->base, xdg_surface_destroy);
	FREE(win->surface, wl_surface_destroy);
	
	win->active = false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

int
wayland_init(struct wayland *wl)
{
	*wl = (struct wayland){0};

	/* base setup */

	if (!(wl->display = wl_display_connect(nullptr)))
	{
		goto fail_con;
	}

	if (!(wl->registry = wl_display_get_registry(wl->display)))
	{
		goto fail_reg;
	}

	wl_registry_add_listener(wl->registry, &ear_reg, wl);
	if (wl_display_roundtrip(wl->display) == -1)
	{
		goto fail_trip;
	}

	/* mandatory interfaces check */

	if (!wl->compositor || !wl->seat || !wl->shm || !wl->xdg)
	{
		goto fail_check;
	}

	/* end */

	xdg_wm_base_add_listener(wl->xdg,  &ear_xdg,  wl);
	wl_seat_add_listener(wl->seat, &ear_seat, wl);
	flush(wl);

	return wl_display_get_fd(wl->display);

	/* errors */

fail_check:
fail_trip:
fail_reg:
	wayland_kill(wl);
fail_con:
	return -1;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
wayland_kill(struct wayland *wl)
{
	FREE(wl->decor, zxdg_decoration_manager_v1_destroy);
	FREE(wl->xdg, xdg_wm_base_destroy);
	FREE(wl->shm, wl_shm_destroy);
	FREE(wl->seat, wl_seat_destroy);
	FREE(wl->compositor, wl_compositor_destroy);
	FREE(wl->registry, wl_registry_destroy);

	wl_display_disconnect(wl->display);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
wayland_read(struct wayland *wl)
{
	/* non-blocking wl_display_dispatch */

	struct pollfd pfd = { wl_display_get_fd(wl->display), POLLIN, 0 };
	int r;

	/* flush old events */

	while (wl_display_prepare_read(wl->display))
	{
		if (wl_display_dispatch_pending(wl->display) == -1)
		{
			shell_send_event(event_error, SHELL_MAIN);
			return;
		}
	}

	/* get and process new events */

	for (;;)
	{
		if ((r = poll(&pfd, 1, 0)) > 0 && pfd.revents & POLLIN)
		{
			if (wl_display_read_events(wl->display)      == -1
			 || wl_display_dispatch_pending(wl->display) == -1)
			{
				shell_send_event(event_error, SHELL_MAIN);
			}
			return;
		}
		else if (r >= 0 || errno != EINTR)
		{
			break;
		}
	}

	/* cancel path & errors */

	wl_display_cancel_read(wl->display);
	if (r != 0)
	{
		shell_send_event(event_error, SHELL_MAIN);
	}
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
wayland_rename(struct wayland *wl, enum shell_target target, const char *name)
{
	if (target == SHELL_MAIN)
	{
		xdg_toplevel_set_title(wl->main.top, name);
		flush(wl);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
wayland_show(struct wayland *wl, enum shell_target target, const char *tag, uint32_t w, uint32_t h)
{
	struct wayland_window *win = target == SHELL_MAIN ? &wl->main : &wl->menu;

	if (win->active)
	{
		return;
	}

	/* common components */

	if (!(win->surface = wl_compositor_create_surface(wl->compositor)))
	{
		goto fail_surface;
	}

	if (!(win->base = xdg_wm_base_get_xdg_surface(wl->xdg, win->surface)))
	{
		goto fail_xdg;
	}

	for (int i = 0; i < WAYLAND_BUFFER_N; i++)
	{
		win->buffers[i] = (struct wayland_buffer){0};
	}

	/* set common properties */

	win->redraw = false;
	win->commit = false;
	win->wait   = false;
	win->init   = false;
	win->w      = w;
	win->h      = h;

	xdg_surface_add_listener(win->base, &ear_base, win);

	/* finish with type specific inits */

	if (target == SHELL_MAIN)
	{
		init_shell(wl, win, tag);
	}
	else
	{
		init_menu(wl, win);
	}

	return;

	/* errors */

fail_xdg:
	wl_surface_destroy(win->surface);
fail_surface:
	shell_send_event(event_error, target);
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
buffer_free(struct wayland_buffer *buf)
{
	FREE(buf->handle, wl_buffer_destroy);
	if (buf->pixels)
	{
		munmap(buf->pixels, buf->w * buf->h * 4);
	}

	cairo_destroy(buf->cairo);
	cairo_surface_finish(buf->surface);
	cairo_surface_destroy(buf->surface);

	buf->handle = nullptr;
	buf->pixels = nullptr;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
buffer_resize(struct wayland_buffer *buf, struct wayland *wl, uint32_t w, uint32_t h)
{
	cairo_t *ctx;
	cairo_surface_t *sfc;
	struct wl_shm_pool *pool;
	struct wl_buffer *buf2;
	struct timespec ts;
	char name[128];
	size_t stride;
	size_t size;
	void *data;
	long v;
	int fd;

	/* checks */

	if (buf->w == w && buf->h == h)
	{
		return true;
	}

	if (ckd_mul(&stride, w, 4) || ckd_mul(&size, h, stride))
	{
		goto fail_math;
	}

	/* open fd with unique filename */

	if (clock_gettime(CLOCK_MONOTONIC, &ts) == -1)
	{
		goto fail_time;
	}

	for (int i = 0; i < 100; i++)
	{
		v = ts.tv_nsec + ts.tv_sec * 1'000'000'000;
		snprintf(name, 128, "/cgui-wl-%li-%li-%i", (long)getpid(), v, i);
		if ((fd = shm_open(name, O_RDWR | O_CREAT | O_EXCL, 0600)) >= 0)
		{
			shm_unlink(name);
			break;
		}
		else if (errno != EEXIST)
		{
			goto fail_open;
		}
	}

	while (ftruncate(fd, size) != 0)
	{
		if (errno != EINTR)
		{
			goto fail_file;
		}
	}

	/* prepare buffer components */

	if (!(pool = wl_shm_create_pool(wl->shm, fd, size)))
	{
		goto fail_pool;
	}

	if (!(buf2 = wl_shm_pool_create_buffer(pool, 0, w, h, stride, WL_SHM_FORMAT_ARGB8888)))
	{
		goto fail_buff;
	}

	if ((data = mmap(nullptr, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0)) == MAP_FAILED)
	{
		goto fail_mmap;
	}

	/* prepare cairo components */

	sfc = cairo_image_surface_create_for_data(data, CAIRO_FORMAT_ARGB32, w, h, stride);
	if (cairo_surface_status(sfc) != CAIRO_STATUS_SUCCESS)
	{
		goto fail_surface;
	}

	ctx = cairo_create(sfc);
	if (cairo_status(ctx) != CAIRO_STATUS_SUCCESS)
	{
		goto fail_cairo;
	}

	/* end & cleanup */

	wl_buffer_add_listener(buf2, &ear_buffer, buf);
	wl_shm_pool_destroy(pool);
	buffer_free(buf);
	close(fd);

	buf->handle  = buf2;
	buf->pixels  = data;
	buf->surface = sfc;
	buf->cairo   = ctx;
	buf->w       = w;
	buf->h       = h;
	buf->busy    = false;

	return true;

	/* errors */

fail_cairo:
	cairo_surface_destroy(sfc);
fail_surface:
	munmap(data, size);
fail_mmap:
	wl_buffer_destroy(buf2);
fail_buff:
	wl_shm_pool_destroy(pool);
fail_pool:
fail_file:
	close(fd);
fail_open:
fail_time:
fail_math:
	return false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_bind(void *data, struct wl_registry *reg, uint32_t id, const char *name, uint32_t ver)
{
	(void)ver;

	struct wayland *wl = data;

	BIND(reg, id, name, 4, wl->compositor, wl_compositor_interface);
	BIND(reg, id, name, 1, wl->seat, wl_seat_interface);
	BIND(reg, id, name, 1, wl->shm, wl_shm_interface);
	BIND(reg, id, name, 1, wl->xdg, xdg_wm_base_interface);
	BIND(reg, id, name, 1, wl->decor, zxdg_decoration_manager_v1_interface);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_button(void *data, struct wl_pointer *pt, uint32_t serial, uint32_t time, uint32_t button, uint32_t state)
{
	(void)pt;
	(void)time;

	struct wayland *wl = data;
	struct cevent ev =
	{
		.type = state == WL_POINTER_BUTTON_STATE_PRESSED ? CEVENT_BUTTON_PRESS : CEVENT_BUTTON_RELEASE,
	};

	switch (button)
	{
		case 0x110:
			ev.button = 1;
			break;

		case 0x112:
			ev.button = 2;
			break;

		case 0x111:
			ev.button = 3;
			break;
	}

	wl->serial = serial;
	shell_send_event(ev, wl->menu.active ? SHELL_MENU : SHELL_MAIN);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_close_pop(void *data, struct xdg_popup *pop)
{
	(void)data;
	(void)pop;

	shell_send_event(event_close, SHELL_MENU);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_close_top(void *data, struct xdg_toplevel *top)
{
	(void)data;
	(void)top;

	shell_send_event(event_close, SHELL_MAIN);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_conf_base(void *data, struct xdg_surface *base, uint32_t serial)
{
	(void)data;

	xdg_surface_ack_configure(base, serial);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_conf_pop(void *data, struct xdg_popup *pop, int x, int y, int w, int h)
{
	(void)pop;

	struct wayland_window *win = data;
	struct cevent ev =
	{
		.type        = CEVENT_TRANSFORM,
		.transform_x = x,
		.transform_y = y,
		.transform_w = w,
		.transform_h = h,
	};

	if (!win->init)
	{
		shell_send_event(event_open, SHELL_MENU);
	}

	win->redraw = true;
	win->init   = true;
	win->w      = w;
	win->h      = h;

	shell_send_event(ev, SHELL_MENU);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_conf_top(void *data, struct xdg_toplevel *top, int w, int h, struct wl_array *states)
{
	(void)top;
	(void)states;

	struct wayland_window *win = data;
	struct cevent ev =
	{
		.type        = CEVENT_TRANSFORM,
		.transform_w = w == 0 ? (int)win->w : w,
		.transform_h = h == 0 ? (int)win->h : h,
		.transform_x = 0,
		.transform_y = 0,
	};

	if (!win->init)
	{
		shell_send_event(event_open, SHELL_MAIN);
	}

	win->redraw = true;
	win->init   = true;
	win->w      = ev.transform_w;
	win->h      = ev.transform_h;

	shell_send_event(ev, SHELL_MAIN);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_frame(void *data, struct wl_callback *cl, uint32_t time)
{
	(void)time;

	wl_callback_destroy(cl);

	((struct wayland_window *)data)->wait = false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_motion(void *data, struct wl_pointer *pt, uint32_t time, wl_fixed_t x, wl_fixed_t y)
{
	(void)time;
	(void)pt;

	struct wayland *wl = data;

	wl->px = wl_fixed_to_int(x);
	wl->py = wl_fixed_to_int(y);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_ping(void *data, struct xdg_wm_base *xdg, uint32_t serial)
{
	(void)data;

	xdg_wm_base_pong(xdg, serial);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_release(void *data, struct wl_buffer *buf)
{
	(void)buf;

	((struct wayland_buffer*)data)->busy = false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_seat(void *data, struct wl_seat *seat, uint32_t can)
{
	struct wayland *wl = data;

	if (can & WL_SEAT_CAPABILITY_POINTER)
	{
		wl->pointer = wl_seat_get_pointer(seat);
		wl_pointer_add_listener(wl->pointer, &ear_pointer, wl);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
flush(struct wayland *wl)
{
	struct pollfd pfd = { wl_display_get_fd(wl->display), POLLOUT, 0 };

	while (wl_display_flush(wl->display) == -1 && errno == EAGAIN)
	{
		pfd.revents = 0;
		if (poll(&pfd, 1, -1) < 0)
		{
			if (errno != EINTR)
			{
				break;
			}
		}
		else if (!(pfd.revents & POLLOUT))
		{
			break;
		}
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
init_menu(struct wayland *wl, struct wayland_window *win)
{
	struct xdg_positioner *pos;

	/* setup popup position */

	if (!(pos = xdg_wm_base_create_positioner(wl->xdg)))
	{
		goto fail_pos;
	}

	xdg_positioner_set_offset(pos, 0, 0);
	xdg_positioner_set_size(pos, win->w, win->h);
	xdg_positioner_set_anchor_rect(pos, wl->px, wl->py, 1, 1);
	xdg_positioner_set_anchor(pos, XDG_POSITIONER_ANCHOR_TOP_LEFT);
	xdg_positioner_set_gravity(pos, XDG_POSITIONER_GRAVITY_BOTTOM_RIGHT);
	xdg_positioner_set_constraint_adjustment(pos,
		  XDG_POSITIONER_CONSTRAINT_ADJUSTMENT_SLIDE_X
		| XDG_POSITIONER_CONSTRAINT_ADJUSTMENT_SLIDE_Y
		| XDG_POSITIONER_CONSTRAINT_ADJUSTMENT_FLIP_X
		| XDG_POSITIONER_CONSTRAINT_ADJUSTMENT_FLIP_Y);

	/* role */

	if (!(win->pop = xdg_surface_get_popup(win->base, wl->main.base, pos)))
	{
		goto fail_role;
	}

	/* finish */

	xdg_positioner_destroy(pos);
	xdg_popup_grab(win->pop, wl->seat, wl->serial);
	xdg_popup_add_listener(win->pop, &ear_pop, win);
	wl_surface_commit(win->surface);
	flush(wl);

	win->active = true;

	return;

	/* errors */

fail_role:
	xdg_positioner_destroy(pos);
fail_pos:
	xdg_surface_destroy(win->base);
	shell_send_event(event_error, SHELL_MENU);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
init_shell(struct wayland *wl, struct wayland_window *win, const char *tag)
{
	/* role */

	if (!(win->top = xdg_surface_get_toplevel(win->base)))
	{
		goto fail_role;
	}

	/* decorations */

	if (!wl->decor)
	{
		goto skip_decor;
	}

	if (!(win->ssd = zxdg_decoration_manager_v1_get_toplevel_decoration(wl->decor, win->top)))
	{
		goto fail_decor;
	}

	zxdg_toplevel_decoration_v1_set_mode(win->ssd, ZXDG_TOPLEVEL_DECORATION_V1_MODE_SERVER_SIDE);

skip_decor:

	/* finish */

	xdg_toplevel_add_listener(win->top, &ear_top, win);
	xdg_toplevel_set_app_id(win->top, tag);
	wl_surface_commit(win->surface);
	flush(wl);
	
	win->active = true;

	return;

	/* errors */

fail_decor:
	xdg_toplevel_destroy(win->top);
fail_role:
	xdg_surface_destroy(win->base);
	shell_send_event(event_error, SHELL_MAIN);
}

/************************************************************************************************************/
/* STATIC - NOOP ********************************************************************************************/
/************************************************************************************************************/

static void
ev_axis(void *data, struct wl_pointer *pt, uint32_t time, uint32_t axis, wl_fixed_t value)
{
	(void)data;
	(void)pt;
	(void)time;
	(void)axis;
	(void)value;

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_enter(void *data, struct wl_pointer *pt, uint32_t serial, struct wl_surface *sfc, wl_fixed_t x, wl_fixed_t y)
{
	(void)data;
	(void)pt;
	(void)serial;
	(void)sfc;
	(void)x;
	(void)y;

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_leave(void *data, struct wl_pointer *pt, uint32_t serial, struct wl_surface *sfc)
{
	(void)data;
	(void)pt;
	(void)serial;
	(void)sfc;

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_unbind(void *data, struct wl_registry *reg, uint32_t id)
{
	(void)data;
	(void)reg;
	(void)id;

	/* nothing */
}

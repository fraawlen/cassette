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
#include <unistd.h>
#include <wayland-client.h>

#include "shell.h"
#include "wayland.h"
#include "xdg-shell.h"
#include "xdg-decoration-unstable-v1.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define FREE(INT, FN) if (INT) {FN(INT);}
#define BIND(REG, ID, NAME, VER, TARGET, FACE) \
	if (!strcmp(NAME, FACE.name)) {TARGET = wl_registry_bind(REG, ID, &FACE, VER); return;}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void cl_bind      (void *, struct wl_registry  *, uint32_t, const char *, uint32_t);
static void cl_button    (void *, struct wl_pointer   *, uint32_t, uint32_t, uint32_t, uint32_t);
static void cl_close     (void *, struct xdg_toplevel *);
static void cl_conf_base (void *, struct xdg_surface  *, uint32_t);
static void cl_conf_pop  (void *, struct xdg_popup    *, int, int, int, int);
static void cl_conf_top  (void *, struct xdg_toplevel *, int, int, struct wl_array *);
static void cl_frame     (void *, struct wl_callback  *, uint32_t);
static void cl_motion    (void *, struct wl_pointer   *, uint32_t, wl_fixed_t, wl_fixed_t);
static void cl_ping      (void *, struct xdg_wm_base  *, uint32_t);
static void cl_popout    (void *, struct xdg_popup    *);
static void cl_release   (void *, struct wl_buffer    *);
static void cl_seat      (void *, struct wl_seat      *, uint32_t);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void cl_axis   (void *, struct wl_pointer   *, uint32_t, uint32_t, wl_fixed_t);
static void cl_enter  (void *, struct wl_pointer   *, uint32_t, struct wl_surface *, wl_fixed_t, wl_fixed_t);
static void cl_leave  (void *, struct wl_pointer   *, uint32_t, struct wl_surface *);
static void cl_unbind (void *, struct wl_registry  *, uint32_t);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void buffer_free        (struct wayland_buffer *);
static bool buffer_resize      (struct wayland_buffer *, struct wayland *, uint32_t, uint32_t);
static void window_commit      (struct wayland_window *, struct wayland *);
static void window_destroy     (struct wayland_window *);
static bool window_init_base   (struct wayland_window *, struct wayland *, uint32_t, uint32_t);
static void destroy_interfaces (struct wayland *);
static bool dispatch_nonblock  (struct wayland *);
static void flush              (struct wayland *);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static const struct wl_registry_listener ear_reg =
{
	.global        = cl_bind,
	.global_remove = cl_unbind,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const struct wl_seat_listener ear_seat =
{
	.capabilities = cl_seat,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const struct wl_pointer_listener ear_pointer =
{
	.enter  = cl_enter,
	.leave  = cl_leave,
	.motion = cl_motion,
	.button = cl_button,
	.axis   = cl_axis,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const struct wl_buffer_listener ear_buffer =
{
	.release = cl_release,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const struct wl_callback_listener ear_frame =
{
	.done = cl_frame,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const struct xdg_surface_listener ear_base =
{
	.configure = cl_conf_base,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const struct xdg_popup_listener ear_pop =
{
	.configure = cl_conf_pop,
	.popup_done = cl_popout,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const struct xdg_toplevel_listener ear_top =
{
	.close     = cl_close,
	.configure = cl_conf_top,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const struct xdg_wm_base_listener ear_xdg =
{
	.ping = cl_ping,
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static atomic_uint file_id = 0;

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

void
wayland_menu_close(struct wayland *wl)
{
	window_destroy(&wl->menu);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
wayland_menu_open(struct wayland *wl, uint32_t w, uint32_t h)
{
	struct wayland_window *win = &wl->menu;
	struct xdg_positioner *pos;

	/* setup popup position */

	if (!(pos = xdg_wm_base_create_positioner(wl->xdg)))
	{
		goto fail_pos;
	}

	xdg_positioner_set_size(pos, w, h);
	xdg_positioner_set_offset(pos, 0, 0);
	xdg_positioner_set_anchor_rect(pos, wl->px, wl->py, 1, 1);
	xdg_positioner_set_anchor(pos, XDG_POSITIONER_ANCHOR_TOP_LEFT);
	xdg_positioner_set_gravity(pos, XDG_POSITIONER_GRAVITY_BOTTOM_RIGHT);
	xdg_positioner_set_constraint_adjustment(pos, 
		  XDG_POSITIONER_CONSTRAINT_ADJUSTMENT_SLIDE_X
		| XDG_POSITIONER_CONSTRAINT_ADJUSTMENT_SLIDE_Y
		| XDG_POSITIONER_CONSTRAINT_ADJUSTMENT_FLIP_X
		| XDG_POSITIONER_CONSTRAINT_ADJUSTMENT_FLIP_Y);

	/* setup popup */

	if (!window_init_base(win, wl, w, h))
	{
		goto fail_base;
	}

	if (!(win->pop = xdg_surface_get_popup(win->base, wl->shell.base, pos)))
	{
		goto fail_role;
	}

	/* end */

	win->active = true;
	xdg_positioner_destroy(pos);
	xdg_popup_grab(win->pop, wl->seat, wl->serial);
	xdg_popup_add_listener(win->pop, &ear_pop, wl);
	wl_surface_commit(win->surface);
	flush(wl);

	return true;

	/* errors */

fail_role:
	xdg_surface_destroy(win->base);
	wl_surface_destroy(win->surface);
fail_base:
	xdg_positioner_destroy(pos);
fail_pos:
	return false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
wayland_menu_redraw(struct wayland *wl)
{
	wl->menu.redraw = true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
wayland_server_commit(struct wayland *wl)
{
	window_commit(&wl->shell, wl);
	window_commit(&wl->menu,  wl);

	flush(wl);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
wayland_server_dispatch(struct wayland *wl)
{
	if (!dispatch_nonblock(wl))
	{
		shell_dispatch_event(cevent_error, false);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
wayland_server_init(struct wayland *wl, int *fd)
{
	*wl = (struct wayland){0};

	/* core components */

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
		goto fail_interfaces;
	}

	/* end */

	xdg_wm_base_add_listener(wl->xdg,  &ear_xdg,  wl);
	wl_seat_add_listener(wl->seat, &ear_seat, wl);
	*fd = wl_display_get_fd(wl->display);
	flush(wl);

	return true;

	/* errors */

fail_interfaces:
fail_trip:
	destroy_interfaces(wl);
fail_reg:
	wl_display_disconnect(wl->display);
fail_con:
	return false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
wayland_server_kill(struct wayland *wl)
{
	destroy_interfaces(wl);
	wl_display_disconnect(wl->display);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
wayland_shell_close(struct wayland *wl)
{
	window_destroy(&wl->shell);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
wayland_shell_open(struct wayland *wl, uint32_t w, uint32_t h)
{
	(void)w;
	(void)h;

	struct wayland_window *win = &wl->shell;

	if (!window_init_base(win, wl, w, h))
	{
		goto fail_base;
	}

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

	/* end */

	win->active = true;
	xdg_toplevel_add_listener(win->top, &ear_top, wl);
	wl_surface_commit(win->surface);
	flush(wl);

	return true;

	/* errors */

fail_decor:
	xdg_toplevel_destroy(win->top);
fail_role:
	xdg_surface_destroy(win->base);
	wl_surface_destroy(win->surface);
fail_base:
	return false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
wayland_shell_redraw(struct wayland *wl)
{
	wl->shell.redraw = true;
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
	cairo_surface_t *sfc;
	cairo_t *ctx;
	char name[128];
	size_t stride;
	size_t size;
	int fd;

	/* checks */

	if (buf->w == w && buf->h == h)
	{
		return true;
	}

	if (ckd_mul(&stride, w, 4) || ckd_mul(&size, h, stride))
	{
		goto fail_open;
	}

	/* open fd with unique filename */

	for (;;)
	{
		snprintf(name, 128, "/cgui-wl-%li-%u", (long)getpid(), atomic_fetch_add(&file_id, 1));
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
	
	struct wl_shm_pool *pool;
	struct wl_buffer *buf2;
	void *data;

	if (!(pool = wl_shm_create_pool(wl->shm, fd, size)))
	{
		goto fail_file;
	}

	if (!(buf2 = wl_shm_pool_create_buffer(pool, 0, w, h, stride, WL_SHM_FORMAT_ARGB8888)))
	{
		goto fail_buff;
	}

	if ((data = mmap(nullptr, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0)) == MAP_FAILED)
	{
		goto fail_mmap;
	}
	
	wl_buffer_add_listener(buf2, &ear_buffer, buf);

	/* prepare cairo components */

	sfc = cairo_image_surface_create_for_data(data, CAIRO_FORMAT_ARGB32, w, h, stride);
	if (cairo_surface_status(sfc) != CAIRO_STATUS_SUCCESS)
	{
		goto fail_sfc;
	}

	ctx = cairo_create(sfc);
	if (cairo_status(ctx) != CAIRO_STATUS_SUCCESS)
	{
		goto fail_ctx;
	}

	/* cleanup */

	buffer_free(buf);
	wl_shm_pool_destroy(pool);
	close(fd);

	/* end */

	buf->handle  = buf2;
	buf->pixels  = data;
	buf->surface = sfc;
	buf->cairo   = ctx;
	buf->w       = w;
	buf->h       = h;
	buf->busy    = false;

	return true;

	/* errors */

fail_ctx:
	cairo_surface_destroy(sfc);
fail_sfc:
	munmap(data, size);
fail_mmap:
	wl_buffer_destroy(buf2);
fail_buff:
	wl_shm_pool_destroy(pool);
fail_file:
	close(fd);
fail_open:
	return false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
cl_bind(void *data, struct wl_registry *reg, uint32_t id, const char *name, uint32_t ver)
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
cl_button(void *data, struct wl_pointer *pt, uint32_t serial, uint32_t time, uint32_t button, uint32_t state)
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
	shell_dispatch_event(ev, wl->menu.active);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
cl_close(void *data, struct xdg_toplevel *top)
{
	(void)data;
	(void)top;
	
	struct cevent ev = {.type = CEVENT_CLOSE};

	shell_dispatch_event(ev, false);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
cl_conf_base(void *data, struct xdg_surface *base, uint32_t serial)
{
	struct wayland_window *win = data;

	xdg_surface_ack_configure(base, serial);
	if (!win->init)
	{
		win->redraw = true;
		win->init   = true;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
cl_conf_pop(void *data, struct xdg_popup *pop, int x, int y, int w, int h)
{
	(void)pop;

	struct wayland *wl = data;
	struct cevent ev =
	{
		.type = CEVENT_TRANSFORM,
		.transform_x = x,
		.transform_y = y,
		.transform_w = w,
		.transform_h = h,
	};

	wl->menu.w = w;
	wl->menu.h = h;
	wl->menu.redraw = true;

	shell_dispatch_event(ev, true);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
cl_conf_top(void *data, struct xdg_toplevel *top, int w, int h, struct wl_array *states)
{
	(void)top;
	(void)states;

	struct cevent ev = {0};
	struct wayland *wl = data;

	w = w == 0 ? (int)wl->shell.w : w;
	h = h == 0 ? (int)wl->shell.h : h;

	ev.type = CEVENT_TRANSFORM;
	ev.transform_w = w;
	ev.transform_h = h;
	ev.transform_x = 0;
	ev.transform_y = 0;
	wl->shell.w = w;
	wl->shell.h = h;
	wl->shell.redraw = true;

	shell_dispatch_event(ev, false);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
cl_frame(void *data, struct wl_callback *cl, uint32_t time)
{
	(void)time;

	wl_callback_destroy(cl);

	((struct wayland_window *)data)->wait = false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
cl_motion(void *data, struct wl_pointer *pt, uint32_t time, wl_fixed_t x, wl_fixed_t y)
{
	(void)time;
	(void)pt;

	struct wayland *wl = data;

	wl->px = wl_fixed_to_int(x);
	wl->py = wl_fixed_to_int(y);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
cl_ping(void *data, struct xdg_wm_base *xdg, uint32_t serial)
{
	(void)data;

	xdg_wm_base_pong(xdg, serial);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
cl_popout(void *data, struct xdg_popup *pop)
{
	(void)data;
	(void)pop;

	struct cevent ev = {.type = CEVENT_CLOSE};

	shell_dispatch_event(ev, true);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
cl_release(void *data, struct wl_buffer *buf)
{
	(void)buf;

	((struct wayland_buffer*)data)->busy = false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
cl_seat(void *data, struct wl_seat *seat, uint32_t capabilities)
{
	struct wayland *wl = data;

	if (capabilities & WL_SEAT_CAPABILITY_POINTER)
	{
		wl->pointer = wl_seat_get_pointer(seat);
		wl_pointer_add_listener(wl->pointer, &ear_pointer, wl);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
destroy_interfaces(struct wayland *wl)
{
	FREE(wl->decor, zxdg_decoration_manager_v1_destroy);
	FREE(wl->xdg, xdg_wm_base_destroy);
	FREE(wl->shm, wl_shm_destroy);
	FREE(wl->seat, wl_seat_destroy);
	FREE(wl->compositor, wl_compositor_destroy);
	FREE(wl->registry, wl_registry_destroy);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
dispatch_nonblock(struct wayland *wl)
{
	struct pollfd pfd = {wl_display_get_fd(wl->display), POLLIN, 0};
	int r;

	while (wl_display_prepare_read(wl->display))
	{
		if (wl_display_dispatch_pending(wl->display) == -1)
		{
			return false;
		}
	}

	for (;;)
	{
		if ((r = poll(&pfd, 1, 0)) > 0 && pfd.revents & POLLIN)
		{
			return wl_display_read_events(wl->display)      != -1
			    && wl_display_dispatch_pending(wl->display) != -1;
		}
		else if (r >= 0 || errno != EINTR)
		{
			break;
		}
	}

	wl_display_cancel_read(wl->display);

	return r == 0;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
flush(struct wayland *wl)
{
	struct pollfd pfd = {wl_display_get_fd(wl->display), POLLOUT, 0};

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
window_commit(struct wayland_window *win, struct wayland *wl)
{
	struct wl_callback *cl;
	struct wayland_buffer *buf = nullptr;
	struct cevent ev =
	{
		.type = CEVENT_REDRAW,
		.redraw_shell = win == &wl->shell,
	};

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
		if (buffer_resize(buf, wl, win->w, win->h)
		&& (cl = wl_surface_frame(win->surface)))
		{
			win->redraw = false;
			win->commit = true;
			win->wait   = true;
			buf->busy   = true;

			ev.redraw_ctx = buf->cairo;
			shell_dispatch_event(ev, win == &wl->menu);
			cairo_surface_flush(buf->surface);

			wl_callback_add_listener(cl, &ear_frame, win);
			wl_surface_attach(win->surface, buf->handle, 0, 0);
			wl_surface_damage_buffer(win->surface, 0, 0, buf->w, buf->h);
			xdg_surface_set_window_geometry(win->base, 0, 0, win->w, win->h);
		}
		else
		{
			shell_dispatch_event(cevent_error, win == &wl->menu);
		}		
	}

	/* commit */

	if (win->commit)
	{
		wl_surface_commit(win->surface);
		win->commit = false;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
window_destroy(struct wayland_window *win)
{
	if (win->active)
	{
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
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
window_init_base(struct wayland_window *win, struct wayland *wl, uint32_t w, uint32_t h)
{
	if (win->active || !(win->surface = wl_compositor_create_surface(wl->compositor)))
	{
		return false;
	}

	if (!(win->base = xdg_wm_base_get_xdg_surface(wl->xdg, win->surface)))
	{
		wl_surface_destroy(win->surface);
		return false;
	}

	for (int i = 0; i < WAYLAND_BUFFER_N; i++)
	{
		win->buffers[i] = (struct wayland_buffer){0};
	}

	win->redraw = false; 
	win->commit = false;
	win->wait   = false;
	win->init   = false;
	win->w      = w;
	win->h      = h;

	xdg_surface_add_listener(win->base, &ear_base, win);

	return true;
}

/************************************************************************************************************/
/* STATIC - NOOP ********************************************************************************************/
/************************************************************************************************************/

static void
cl_axis(void *data, struct wl_pointer *pt, uint32_t time, uint32_t axis, wl_fixed_t value)
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
cl_enter(void *data, struct wl_pointer *pt, uint32_t serial, struct wl_surface *sfc, wl_fixed_t x, wl_fixed_t y)
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
cl_leave(void *data, struct wl_pointer *pt, uint32_t serial, struct wl_surface *sfc)
{
	(void)data;
	(void)pt;
	(void)serial;
	(void)sfc;

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
cl_unbind(void *data, struct wl_registry *reg, uint32_t id)
{
	(void)data;
	(void)reg;
	(void)id;

	/* nothing */
}

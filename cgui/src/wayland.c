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

#include "event.h"
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
static void cl_conf_top  (void *, struct xdg_toplevel *, int, int, struct wl_array *);
static void cl_frame     (void *, struct wl_callback  *, uint32_t);
static void cl_ping      (void *, struct xdg_wm_base  *, uint32_t);
static void cl_release   (void *, struct wl_buffer    *);
static void cl_seat      (void *, struct wl_seat      *, uint32_t);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void cl_axis   (void *, struct wl_pointer   *, uint32_t, uint32_t, wl_fixed_t);
static void cl_enter  (void *, struct wl_pointer   *, uint32_t, struct wl_surface *, wl_fixed_t, wl_fixed_t);
static void cl_leave  (void *, struct wl_pointer   *, uint32_t, struct wl_surface *);
static void cl_motion (void *, struct wl_pointer   *, uint32_t, wl_fixed_t, wl_fixed_t);
static void cl_unbind (void *, struct wl_registry  *, uint32_t);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void buffer_free        (struct wayland_buffer *);
static bool buffer_resize      (struct wayland_buffer *, struct wayland *, uint32_t, uint32_t);
static void window_commit      (struct wayland_window *, struct wayland *, cshell *);
static void window_destroy     (struct wayland_window *);
static bool window_init        (struct wayland_window *, struct wayland *);
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
	(void)wl;
	(void)w;
	(void)h;

	// TODO

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
wayland_menu_redraw(struct wayland *wl)
{
	wl->menu.redraw = true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
wayland_server_commit(struct wayland *wl, cshell *sh)
{
	window_commit(&wl->shell, wl, sh);
	window_commit(&wl->menu,  wl, sh);

	flush(wl);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
wayland_server_dispatch(struct wayland *wl, cshell *sh)
{
	if (dispatch_nonblock(wl) && event_stack_error(wl->queue) == CERR_NONE)
	{
		while (event_stack_length(wl->queue) > 0)
		{
			shell_dispatch_event(sh, event_stack_pop(wl->queue));
		}
	}
	else
	{
		shell_dispatch_event(sh, cevent_error);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
wayland_server_init(struct wayland *wl, int *fd)
{
	*wl = (struct wayland){0};

	/* core components */

	if (!(wl->queue = event_stack_create()))
	{
		goto fail_queue;
	}

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
	event_stack_destroy(wl->queue);
fail_queue:
	return false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
wayland_server_kill(struct wayland *wl)
{
	destroy_interfaces(wl);
	event_stack_destroy(wl->queue);
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

	return window_init(&wl->shell, wl);
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
	(void)serial;
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

	event_stack_push(wl->queue, ev);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
cl_close(void *data, struct xdg_toplevel *top)
{
	(void)top;
	
	struct cevent ev = {.type = CEVENT_CLOSE};
	struct wayland *wl = data;

	event_stack_push(wl->queue, ev);
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
cl_conf_top(void *data, struct xdg_toplevel *top, int w, int h, struct wl_array *arr)
{
	(void)top;
	(void)arr;

	struct cevent ev = {0};
	struct wayland *wl = data;

	if (w > 0 && h > 0)
	{
		ev.type = CEVENT_TRANSFORM;
		ev.transform_w = w;
		ev.transform_h = h;
		ev.transform_x = 0;
		ev.transform_y = 0;
		event_stack_push(wl->queue, ev);
	}
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
cl_ping(void *data, struct xdg_wm_base *xdg, uint32_t serial)
{
	(void)data;

	xdg_wm_base_pong(xdg, serial);
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
window_commit(struct wayland_window *win, struct wayland *wl, cshell *sh)
{
	struct cevent ev = {.type = CEVENT_REDRAW};
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
		if (buffer_resize(buf, wl, shell_w(sh), shell_h(sh))
		&& (cl = wl_surface_frame(win->surface)))
		{
			win->redraw = false;
			win->commit = true;
			win->wait   = true;
			buf->busy   = true;

			ev.redraw_ctx = buf->cairo;
			shell_dispatch_event(sh, ev);
			cairo_surface_flush(buf->surface);

			wl_callback_add_listener(cl, &ear_frame, win);
			wl_surface_attach(win->surface, buf->handle, 0, 0);
			wl_surface_damage_buffer(win->surface, 0, 0, buf->w, buf->h);
		}
		else
		{
			shell_dispatch_event(sh, cevent_error);
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
		FREE(win->base, xdg_surface_destroy);
		FREE(win->surface, wl_surface_destroy);
		win->active = false;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
window_init(struct wayland_window *win, struct wayland *wl)
{
	if (win->active)
	{
		return true;
	}

	/* base components */

	if (!(win->surface = wl_compositor_create_surface(wl->compositor)))
	{
		goto fail_interfaces;
	}

	if (!(win->base = xdg_wm_base_get_xdg_surface(wl->xdg, win->surface)))
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

	for (int i = 0; i < WAYLAND_BUFFER_N; i++)
	{
		win->buffers[i] = (struct wayland_buffer){0};
	}

	win->active = true;
	win->redraw = false; 
	win->commit = false;
	win->wait   = false;
	win->init   = false;

	xdg_surface_add_listener(win->base, &ear_base, win);
	xdg_toplevel_add_listener(win->top, &ear_top,  wl);
	wl_surface_commit(win->surface);
	flush(wl);

	return true;

	/* errors */

fail_decor:
	FREE(win->top, xdg_toplevel_destroy)
	FREE(win->pop, xdg_popup_destroy)
fail_role:
	xdg_surface_destroy(win->base);
fail_base:
	wl_surface_destroy(win->surface);
fail_interfaces:
	return false;
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
cl_motion(void *data, struct wl_pointer *pt, uint32_t time, wl_fixed_t x, wl_fixed_t y)
{
	(void)data;
	(void)pt;
	(void)time;
	(void)x;
	(void)y;

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

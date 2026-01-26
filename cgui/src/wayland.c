/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

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

#define CAST_WL_DATA(WL, DATA) struct wayland *WL = (struct wayland *)DATA;
#define FREE(INT, FN) if (INT) {FN(INT);}
#define BIND(REG, ID, NAME, VER, INT, SRC) \
	if (strcmp(NAME, SRC.name) == 0) \
		{INT = wl_registry_bind(REG, ID, &SRC, (int)VER > SRC.version ? SRC.version : (int)VER); return;}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void cl_capabilities_seat (void *, struct wl_seat      *, uint32_t);
static void cl_close             (void *, struct xdg_toplevel *);
static void cl_conf_shell        (void *, struct xdg_surface  *, uint32_t);
static void cl_conf_top          (void *, struct xdg_toplevel *, int, int, struct wl_array *);
static void cl_frame             (void *, struct wl_callback  *, uint32_t);
static void cl_interface_add     (void *, struct wl_registry  *, uint32_t, const char *, uint32_t);
static void cl_ping              (void *, struct xdg_wm_base  *, uint32_t);
static void cl_release           (void *, struct wl_buffer    *);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void cl_capabilities_top  (void *, struct xdg_toplevel *, struct wl_array *);
static void cl_conf_bounds       (void *, struct xdg_toplevel *, int, int);
static void cl_interface_del     (void *, struct wl_registry  *, uint32_t);
static void cl_seat_name         (void *, struct wl_seat      *, const char *);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/


static void buffer_attach      (struct wayland *, size_t);
static void buffer_free        (struct wayland *, size_t);
static void buffer_paint       (struct wayland *, size_t, uint8_t);
static bool buffer_update      (struct wayland *, size_t);
static void destroy_interfaces (struct wayland *);
static bool dispatch_nonblock  (struct wayland *);
static void flush              (struct wayland *);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static const struct wl_registry_listener ear_reg =
{
	.global        = cl_interface_add,
	.global_remove = cl_interface_del,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const struct wl_seat_listener ear_seat =
{
	.capabilities = cl_capabilities_seat,
	.name         = cl_seat_name,
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

static const struct xdg_surface_listener ear_shell =
{
	.configure = cl_conf_shell,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const struct xdg_toplevel_listener ear_toplevel =
{
	.close            = cl_close,
	.configure        = cl_conf_top,
	.configure_bounds = cl_conf_bounds,
	.wm_capabilities  = cl_capabilities_top,
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
wayland_commit(struct wayland *wl, cshell *sh)
{
	struct cevent ev = {.type = CEVENT_REDRAW};
	struct wl_callback *cl;

	/* redraw */

	if (!wl->redraw || wl->wait)
	{
		goto skip_redraw;
	}

	for (size_t i = 0; i < WAYLAND_BUFFER_N; i++)
	{
		if (!wl->buffers[i].busy)
		{
			if (buffer_update(wl, i))
			{
				buffer_attach(wl, i);
				buffer_paint(wl, i, 0x80);
				event_stack_push(wl->queue, ev);
			}
			break;
		}
	}

	wl->redraw = false;
	wl->commit = true;
	wl->wait   = true;

	/* gate next redraw */

	if ((cl = wl_surface_frame(wl->surface)))
	{
		wl_callback_add_listener(cl, &ear_frame, wl);
	}
	else
	{
		event_stack_push(wl->queue, cevent_error);
	}

skip_redraw:

	/* commit */

	if (wl->commit)
	{
		wl_surface_commit(wl->surface);
		wl->commit = false;
	}

	/* end */

	shell_set_error(sh, event_stack_error(wl->queue));
	flush(wl);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
wayland_dispatch(struct wayland *wl, cshell *sh)
{
	if (dispatch_nonblock(wl))
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
wayland_init(struct wayland *wl, int *fd)
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
		goto fail_interfaces;
	}

	/* mandatory interfaces check */

	if (!wl->compositor
	 || !wl->seat
	 || !wl->shm
	 || !wl->xdg)
	{
		goto fail_interfaces;
	}

	/* create toplevel window */

	if (!(wl->surface = wl_compositor_create_surface(wl->compositor)))
	{
		goto fail_interfaces;
	}

	if (!(wl->shell = xdg_wm_base_get_xdg_surface(wl->xdg, wl->surface)))
	{
		goto fail_shell;
	}

	if (!(wl->toplevel = xdg_surface_get_toplevel(wl->shell)))
	{
		goto fail_top;
	}

	/* decorations */

	if (!wl->decor)
	{
		goto skip_decor;
	}

	if (!(wl->ssd = zxdg_decoration_manager_v1_get_toplevel_decoration(wl->decor, wl->toplevel)))
	{
		goto fail_decor;
	}

	zxdg_toplevel_decoration_v1_set_mode(wl->ssd, ZXDG_TOPLEVEL_DECORATION_V1_MODE_SERVER_SIDE);

skip_decor:

	/* setup listeners */

	wl_seat_add_listener      (wl->seat,     &ear_seat,     wl);
	xdg_wm_base_add_listener  (wl->xdg,      &ear_xdg,      wl);
	xdg_surface_add_listener  (wl->shell,    &ear_shell,    wl);
	xdg_toplevel_add_listener (wl->toplevel, &ear_toplevel, wl);

	/* end */

	for (int i = 0; i < WAYLAND_BUFFER_N; i++)
	{
		wl->buffers[i] = (struct wayland_buffer){0};
	}

	wl->width  = 500;
	wl->height = 300;
	wl->redraw = false; 
	wl->commit = false;
	wl->wait   = false;
	wl->init   = false;

	wl_surface_commit(wl->surface);
	flush(wl);

	*fd = wl_display_get_fd(wl->display);

	return true;

	/* errors */

fail_decor:
	xdg_toplevel_destroy(wl->toplevel);
fail_top:
	xdg_surface_destroy(wl->shell);
fail_shell:
	wl_surface_destroy(wl->surface);
fail_interfaces:
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
wayland_kill(struct wayland *wl)
{
	for (int i = 0; i < WAYLAND_BUFFER_N; i++)
	{
		buffer_free(wl, i);
	}
	
	FREE(wl->ssd,      zxdg_toplevel_decoration_v1_destroy);
	FREE(wl->toplevel, xdg_toplevel_destroy);
	FREE(wl->shell,    xdg_surface_destroy);
	FREE(wl->surface,  wl_surface_destroy);

	destroy_interfaces(wl);

	FREE(wl->display,  wl_display_disconnect);
	FREE(wl->queue,    event_stack_destroy);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
wayland_redraw(struct wayland *wl)
{
	wl->redraw = true;
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
buffer_attach(struct wayland *wl, size_t id)
{
	struct wayland_buffer *buf = wl->buffers + id;

	wl_surface_attach(wl->surface, buf->handle, 0, 0);
	wl_surface_damage_buffer(wl->surface, 0, 0, buf->width, buf->height);

	buf->busy = true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
buffer_free(struct wayland *wl, size_t id)
{
	struct wayland_buffer *buf = wl->buffers + id;
	
	FREE(buf->handle, wl_buffer_destroy);
	if (buf->pixels)
	{
		munmap(buf->pixels, buf->width * buf->height * 4);
	}

	buf->handle = nullptr;
	buf->pixels = nullptr;
}	

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
buffer_paint(struct wayland *wl, size_t id, uint8_t value)
{
	memset(wl->buffers[id].pixels, value, wl->buffers[id].width * wl->buffers[id].height * 4);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
buffer_update(struct wayland *wl, size_t id)
{
	struct wayland_buffer *buf = wl->buffers + id;
	char name[128];
	size_t stride;
	size_t size;
	int fd;

	/* checks */

	if (buf->width == wl->width && buf->height == wl->height)
	{
		return true;
	}

	if (ckd_mul(&stride, wl->width,  4)
	 || ckd_mul(&size,   wl->height, stride))
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

	if (!(buf2 = wl_shm_pool_create_buffer(pool, 0, wl->width, wl->height, stride, WL_SHM_FORMAT_ARGB8888)))
	{
		goto fail_buff;
	}

	if ((data = mmap(nullptr, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0)) == MAP_FAILED)
	{
		goto fail_mmap;
	}

	buffer_free(wl, id);
	wl_buffer_add_listener(buf2, &ear_buffer, buf);
	wl_shm_pool_destroy(pool);
	close(fd);

	/* end */

	buf->handle = buf2;
	buf->pixels = data;
	buf->width  = wl->width;
	buf->height = wl->height;
	buf->busy   = false;

	return true;

	/* errors */

fail_mmap:
	wl_buffer_destroy(buf2);
fail_buff:
	wl_shm_pool_destroy(pool);
fail_file:
	close(fd);
fail_open:
	event_stack_push(wl->queue, cevent_error);
	return false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
cl_capabilities_seat(void *data, struct wl_seat *seat, uint32_t capabilities)
{
	(void)data;
	(void)seat;
	(void)capabilities;

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
cl_close(void *data, struct xdg_toplevel *top)
{
	(void)top;

	struct cevent ev = {.type = CEVENT_CLOSE};
	
	CAST_WL_DATA(wl, data);

	event_stack_push(wl->queue, ev);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
cl_conf_shell(void *data, struct xdg_surface *shell, uint32_t serial)
{
	(void)data;

	CAST_WL_DATA(wl, data);

	xdg_surface_ack_configure(shell, serial);

	if (!wl->init)
	{
		wl->redraw = true;
		wl->init   = true;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
cl_conf_top(void *data, struct xdg_toplevel *top, int w, int h, struct wl_array *arr)
{
	(void)top;
	(void)arr;

	struct cevent ev = {0};

	CAST_WL_DATA(wl, data);

	if (w > 0 && h > 0)
	{
		ev.type = CEVENT_TRANSFORM;
		ev.transform_w = w;
		ev.transform_h = h;
		ev.transform_x = 0;
		ev.transform_y = 0;

		wl->width  = w;
		wl->height = h;

		event_stack_push(wl->queue, ev);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
cl_frame(void *data, struct wl_callback *cl, uint32_t time)
{
	(void)time;

	wl_callback_destroy(cl);

	((struct wayland *)data)->wait = true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
cl_interface_add(void *data, struct wl_registry *reg, uint32_t id, const char *name, uint32_t ver)
{
	CAST_WL_DATA(wl, data);

	BIND( reg, id, name, ver, wl->compositor, wl_compositor_interface              );
	BIND( reg, id, name, ver, wl->seat,       wl_seat_interface                    );
	BIND( reg, id, name, ver, wl->shm,        wl_shm_interface                     );
	BIND( reg, id, name, ver, wl->xdg,        xdg_wm_base_interface                );
	BIND( reg, id, name, ver, wl->decor,      zxdg_decoration_manager_v1_interface );
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
destroy_interfaces(struct wayland *wl)
{
	FREE( wl->decor,      zxdg_decoration_manager_v1_destroy );
	FREE( wl->xdg,        xdg_wm_base_destroy                );
	FREE( wl->shm,        wl_shm_destroy                     );
	FREE( wl->seat,       wl_seat_destroy                    );
	FREE( wl->compositor, wl_compositor_destroy              );
	FREE( wl->registry,   wl_registry_destroy                );
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

/************************************************************************************************************/
/* STATIC - NOOP ********************************************************************************************/
/************************************************************************************************************/

static void
cl_capabilities_top(void *data, struct xdg_toplevel *top, struct wl_array *arr)
{
	(void)data;
	(void)top;
	(void)arr;

	/* nothing */
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
cl_conf_bounds(void *data, struct xdg_toplevel *top, int w, int h)
{
	(void)data;
	(void)top;
	(void)w;
	(void)h;

	/* nothing */
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
cl_interface_del(void *data, struct wl_registry *reg, uint32_t id)
{
	(void)data;
	(void)reg;
	(void)id;

	/* nothing */
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
cl_seat_name(void *data, struct wl_seat *seat, const char *name)
{
	(void)data;
	(void)seat;
	(void)name;

	/* nothing */
}

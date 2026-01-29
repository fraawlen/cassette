/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#pragma once

#include <cairo/cairo.h>
#include <cassette/cgui.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <wayland-client.h>

#include "event.h"
#include "xdg-decoration-unstable-v1.h"
#include "xdg-shell.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define WAYLAND_BUFFER_N 3

struct wayland_buffer
{
	struct wl_buffer *handle;
	cairo_surface_t *surface;
	cairo_t *cairo;
	uint32_t *pixels;
	size_t height;
	size_t width;
	bool busy;
};

struct wayland
{
	event_stack *queue;

	/* core components */

	struct wl_display *display;
	struct wl_registry *registry;

	/* interfaces */

	struct wl_compositor *compositor;
	struct wl_shm *shm;
	struct wl_seat *seat;
	struct xdg_wm_base *xdg;
	struct zxdg_decoration_manager_v1 *decor;

	/* toplevel components */

	struct wl_surface *surface;
	struct xdg_surface *shell;
	struct xdg_toplevel *toplevel;
	struct zxdg_toplevel_decoration_v1 *ssd;
	struct wayland_buffer buffers[WAYLAND_BUFFER_N];

	/* states */

	bool init;
	bool wait;
	bool commit;
	bool redraw;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

[[gnu::visibility("hidden")]] [[gnu::nonnull(1, 2)]] void
wayland_commit(struct wayland *wl, cshell *sh);

[[gnu::visibility("hidden")]] [[gnu::nonnull(1, 2)]] void
wayland_dispatch(struct wayland *wl, cshell *sh);

[[gnu::visibility("hidden")]] [[gnu::nonnull(1, 2)]] bool
wayland_init(struct wayland *wl, int *fd, uint32_t w, uint32_t h);

[[gnu::visibility("hidden")]] [[gnu::nonnull(1)]] void
wayland_kill(struct wayland *wl);

[[gnu::visibility("hidden")]] [[gnu::nonnull(1)]] void
wayland_redraw(struct wayland *wl);

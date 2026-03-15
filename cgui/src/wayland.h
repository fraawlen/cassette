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

#include "xdg-decoration-unstable-v1.h"
#include "xdg-shell.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define WAYLAND_BUFFER_N 3

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct wayland_buffer
{
	/* components */

	struct wl_buffer *handle;
	cairo_surface_t *surface;
	cairo_t *cairo;

	/* states */

	uint32_t *pixels;
	bool busy;
	size_t h;
	size_t w;
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct wayland_window
{	
	/* components */

	struct wl_surface *surface;
	struct xdg_surface *base;
	struct xdg_toplevel *top;
	struct xdg_popup *pop;
	struct zxdg_toplevel_decoration_v1 *ssd;
	struct wayland_buffer buffers[WAYLAND_BUFFER_N];

	/* states */

	uint32_t w;
	uint32_t h;
	bool commit;
	bool redraw;
	bool active;
	bool wait;
	bool init;
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct wayland
{
	/* components */
	
	struct wl_display *display;
	struct wl_registry *registry;
	struct wl_compositor *compositor;
	struct wl_shm *shm;
	struct wl_seat *seat;
	struct xdg_wm_base *xdg;
	struct zxdg_decoration_manager_v1 *decor;
	struct wl_pointer *pointer;

	/* surfaces */

	struct wayland_window main;
	struct wayland_window menu;

	/* pointer tracking */

	uint32_t serial;
	int32_t px;
	int32_t py;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

[[gnu::visibility("hidden")]] [[gnu::nonnull(1)]] int
wayland_init(struct wayland *wl);

[[gnu::visibility("hidden")]] [[gnu::nonnull(1)]] void
wayland_kill(struct wayland *wl);

[[gnu::visibility("hidden")]] [[gnu::nonnull(1)]] void
wayland_read(struct wayland *wl);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

[[gnu::visibility("hidden")]] [[gnu::nonnull(1)]] void
wayland_damage(struct wayland *wl, enum shell_target target);

[[gnu::visibility("hidden")]] [[gnu::nonnull(1)]] void
wayland_commit(struct wayland *wl, enum shell_target target);

[[gnu::visibility("hidden")]] [[gnu::nonnull(1)]] void
wayland_hide(struct wayland *wl, enum shell_target target);

[[gnu::visibility("hidden")]] [[gnu::nonnull(1)]] void
wayland_show(struct wayland *wl, enum shell_target target, uint32_t w, uint32_t h);

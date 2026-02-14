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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct wayland_buffer
{
	struct wl_buffer *handle;
	cairo_surface_t *surface;
	cairo_t *cairo;
	uint32_t *pixels;
	size_t h;
	size_t w;
	bool busy;
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

	bool init;
	bool wait;
	bool commit;
	bool redraw;
	bool active;
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct wayland
{
	/* core components */
	
	event_stack *queue;

	struct wl_display *display;
	struct wl_registry *registry;
	struct wl_compositor *compositor;
	struct wl_shm *shm;
	struct wl_seat *seat;
	struct xdg_wm_base *xdg;
	struct zxdg_decoration_manager_v1 *decor;
	struct wl_pointer *pointer;

	/* surfaces */

	struct wayland_window shell;
	struct wayland_window menu;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

[[gnu::visibility("hidden")]] [[gnu::nonnull(1)]] void
wayland_menu_close(struct wayland *wl);

[[gnu::visibility("hidden")]] [[gnu::nonnull(1)]] bool
wayland_menu_open(struct wayland *wl, uint32_t w, uint32_t h);

[[gnu::visibility("hidden")]] [[gnu::nonnull(1)]] void
wayland_menu_redraw(struct wayland *wl);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

[[gnu::visibility("hidden")]] [[gnu::nonnull(1)]] void
wayland_shell_close(struct wayland *wl);

[[gnu::visibility("hidden")]] [[gnu::nonnull(1)]] bool
wayland_shell_open(struct wayland *wl, uint32_t w, uint32_t h);

[[gnu::visibility("hidden")]] [[gnu::nonnull(1)]] void
wayland_shell_redraw(struct wayland *wl);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

[[gnu::visibility("hidden")]] [[gnu::nonnull(1, 2)]] void
wayland_server_commit(struct wayland *wl, cshell *sh);

[[gnu::visibility("hidden")]] [[gnu::nonnull(1, 2)]] void
wayland_server_dispatch(struct wayland *wl, cshell *sh);

[[gnu::visibility("hidden")]] [[gnu::nonnull(1, 2)]] bool
wayland_server_init(struct wayland *wl, int *fd);

[[gnu::visibility("hidden")]] [[gnu::nonnull(1)]] void
wayland_server_kill(struct wayland *wl);

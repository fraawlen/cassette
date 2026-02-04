/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#pragma once

#include <cairo/cairo.h>
#include <cassette/cgui.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <xcb/xcb.h>
#include <xcb/xcb_renderutil.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

struct x11
{
	/* core components */

	xcb_connection_t *connection;
	xcb_screen_t *screen;
	xcb_visualtype_t *visual;
	xcb_colormap_t colormap;
	uint8_t depth;

	/* toplevel components */

	xcb_window_t window;
	xcb_pixmap_t buffer;
	xcb_gcontext_t gc;
	cairo_surface_t *surface;
	cairo_t *cairo;

	/* atoms */

	xcb_atom_t atom_protocol;
	xcb_atom_t atom_close;
	xcb_atom_t atom_focus;
	xcb_atom_t atom_ping;
	xcb_atom_t atom_utf8;
	xcb_atom_t atom_time;

	/* extensions */

	uint8_t opcode_present;
	uint8_t opcode_render;
	uint8_t opcode_xinput;

	/* states */

	uint32_t buffer_w;
	uint32_t buffer_h;
	uint32_t serial;
	bool present;
	bool redraw;
	bool wait;
	bool busy;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

[[gnu::visibility("hidden")]] [[gnu::nonnull(1, 2)]] void
x11_commit(struct x11 *x, cshell *sh);

[[gnu::visibility("hidden")]] [[gnu::nonnull(1, 2)]] void
x11_dispatch(struct x11 *x, cshell *sh);

[[gnu::visibility("hidden")]] [[gnu::nonnull(1, 2)]] bool
x11_init(struct x11 *x, int *fd, uint32_t w, uint32_t h);

[[gnu::visibility("hidden")]] [[gnu::nonnull(1)]] void
x11_kill(struct x11 *x);

[[gnu::visibility("hidden")]] [[gnu::nonnull(1)]] void
x11_redraw(struct x11 *x);

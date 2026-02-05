/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#pragma once

#include <cairo/cairo.h>
#include <cassette/cgui.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <xcb/sync.h>
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

	xcb_sync_counter_t sync_count;
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
	xcb_atom_t atom_clip;
	xcb_atom_t atom_multiple;
	xcb_atom_t atom_target;
	xcb_atom_t atom_name;
	xcb_atom_t atom_icon;
	xcb_atom_t atom_class;
	xcb_atom_t atom_cmd;
	xcb_atom_t atom_host;
	xcb_atom_t atom_lead;
	xcb_atom_t atom_pid;
	xcb_atom_t atom_name2;
	xcb_atom_t atom_icon2;
	xcb_atom_t atom_type;
	xcb_atom_t atom_shell;
	xcb_atom_t atom_dock;
	xcb_atom_t atom_menu;
	xcb_atom_t atom_sync;
	xcb_atom_t atom_sync2;

	/* extensions */

	xcb_sync_int64_t sync_val;

	uint8_t opcode_present;
	uint8_t opcode_render;
	uint8_t opcode_xinput;
	uint8_t opcode_sync;

	/* states */

	uint32_t buffer_w;
	uint32_t buffer_h;
	uint32_t serial;
	bool present;
	bool redraw;
	bool wait;
	bool busy;
	bool sync;
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

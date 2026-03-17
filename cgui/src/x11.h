/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#pragma once

#include <cairo/cairo.h>
#include <cassette/cgui.h>
#include <cassette/ccfg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <xcb/sync.h>
#include <xcb/xcb.h>
#include <xcb/xcb_renderutil.h>

#include "shell.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

struct x11_window
{
	/* toplevel components */

	xcb_sync_counter_t sync_count;
	xcb_window_t window;
	xcb_pixmap_t buffer;
	xcb_gcontext_t gc;
	cairo_surface_t *surface;
	cairo_t *cairo;

	/* states */

	xcb_sync_int64_t sync_val;
	uint32_t serial;
	uint32_t w;
	uint32_t h;

	bool present;
	bool resized;
	bool damaged;
	bool active;
	bool mapped;
	bool wait;
	bool busy;
	bool sync;
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct x11
{
	/* core components */

	xcb_connection_t *connection;
	xcb_screen_t *screen;
	xcb_visualtype_t *visual;
	xcb_colormap_t colormap;
	uint8_t depth;

	/* extensions */

	uint8_t opcode_present;
	uint8_t opcode_render;
	uint8_t opcode_xinput;
	uint8_t opcode_randr;
	uint8_t opcode_sync;

	/* ICCCM & EWMH atoms */

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

	/* surfaces */

	struct x11_window main;
	struct x11_window menu;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

[[gnu::visibility("hidden")]] [[gnu::nonnull(1)]] void
x11_config(struct x11 *x11, ccfg *cfg);

[[gnu::visibility("hidden")]] [[gnu::nonnull(1)]] int
x11_init(struct x11 *x11);

[[gnu::visibility("hidden")]] [[gnu::nonnull(1)]] void
x11_kill(struct x11 *x11);

[[gnu::visibility("hidden")]] [[gnu::nonnull(1)]] void
x11_read(struct x11 *x11);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

[[gnu::visibility("hidden")]] [[gnu::nonnull(1)]] void
x11_commit(struct x11 *x11, enum shell_target target);

[[gnu::visibility("hidden")]] [[gnu::nonnull(1)]] void
x11_damage(struct x11 *x11, enum shell_target target);

[[gnu::visibility("hidden")]] [[gnu::nonnull(1)]] void
x11_hide(struct x11 *x11, enum shell_target target);

[[gnu::visibility("hidden")]] [[gnu::nonnull(1, 2)]] void
x11_rename(struct x11 *x11, enum shell_target target, const char *name);

[[gnu::visibility("hidden")]] [[gnu::nonnull(1)]] void
x11_show(struct x11 *x11, enum shell_target target, const char *tag, uint32_t w, uint32_t h);


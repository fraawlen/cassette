/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#pragma once

#include <cassette/cgui.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <xcb/xcb.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

struct x11
{
	/* core components */

	xcb_connection_t *connection;
	xcb_screen_t *screen;

	/* toplevel components */

	xcb_window_t window;

	/* atoms */

	xcb_atom_t atom_protocol;
	xcb_atom_t atom_close;
	xcb_atom_t atom_focus;
	xcb_atom_t atom_ping;
	xcb_atom_t atom_utf8;
	xcb_atom_t atom_time;

	/* states */

	bool redraw;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

[[gnu::visibility("hidden")]] [[gnu::nonnull(1, 2)]] void
x11_commit(struct x11 *x, cshell *sh);

[[gnu::visibility("hidden")]] [[gnu::nonnull(1, 2)]] void
x11_dispatch(struct x11 *x, cshell *sh);

[[gnu::visibility("hidden")]] [[gnu::nonnull(1, 2)]] bool
x11_init(struct x11 *x, int *fd);

[[gnu::visibility("hidden")]] [[gnu::nonnull(1)]] void
x11_kill(struct x11 *x);

[[gnu::visibility("hidden")]] [[gnu::nonnull(1)]] void
x11_redraw(struct x11 *x);

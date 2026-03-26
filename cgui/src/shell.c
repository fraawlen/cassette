/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#include <cairo/cairo.h>
#include <cassette/ccfg.h>
#include <cassette/cgui.h>
#include <cassette/cobj.h>
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <pthread.h>
#include <pwd.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "box.h"
#include "event.h"
#include "grid.h"
#include "menu.h"
#include "shell.h"
#include "wayland.h"
#include "x11.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define ENV_NO_CONFIG "CGUI_CONFIG_HARDCODED"
#define ENV_CONFIG    "CGUI_CONFIG_SRC"
#define CONFIG_PARAM  "shell_tag"
#define DEFAULT_NAME  "cgui window"
#define DEFAULT_TAG   "cgui"
#define STR_LEN        256

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

#define GUARD(SH, ...)   if (cerr_critical(cshell_error(SH))) { return __VA_OPT__(__VA_ARGS__); }
#define GUARD_THREAD(SH) if (SH == thread_owner) { set_error(SH, CERR_CALL); return; }
#define FRAME(SH)           (2 * (cbox_border(sh->frame) + cbox_pad(sh->frame)))

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

#define LOCK(SH) \
	for ( \
		int b = 1; \
		b && (pthread_mutex_lock(&SH->mutex) | 1); \
		b = pthread_mutex_unlock(&SH->mutex) & 0)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

#define SERVER(SH, FN, ...) \
	switch (atomic_load(&SH->server)) \
	{ \
		case CSHELL_WAYLAND: \
			wayland_##FN(&SH->wl __VA_OPT__(, __VA_ARGS__)); \
			break; \
		case CSHELL_X11: \
			x11_##FN(&SH->x11 __VA_OPT__(, __VA_ARGS__)); \
			break; \
		default: \
			break; \
	}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct call
{
	void (*fn)(cshell *, void *);
	void *data;
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct cshell
{
	/* multi-threading */

	pthread_mutex_t mutex;
	pthread_cond_t cond;
	pthread_t thread;
	int fd_post[2];
	int fd_wake[2];
	int fd_server;

	/* states */

	_Atomic enum cshell_server server;
	_Atomic enum cshell_state  state;
	_Atomic enum cerr err;

	bool damaged;
	uint32_t w;
	uint32_t h;

	/* contents */

	char name[STR_LEN];
	char tag[STR_LEN];
	struct menu menu;
	cgrid *focus;
	ccfg *config;
	cref *grids;

	/* callbacks */

	struct call cb_open;
	struct call cb_close;
	struct call cb_setup;

	/* config */

	cbox *frame;

	/* backends */

	union
	{
		struct x11 x11;
		struct wayland wl;
	};
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void ev_button    (cshell *, struct cevent);
static void ev_open      (cshell *);
static void ev_redirect  (cshell *, struct cevent);
static void ev_redraw    (cshell *, struct cevent);
static void ev_transform (cshell *, struct cevent);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void apply_name   (cshell *, void *);
static void callback     (cshell *, struct call *);
static void conf_grids   (cshell *);
static void conf_init    (cshell *);
static void conf_menu    (cshell *);
static void conf_shell   (cshell *);
static void destroy      (cshell *);
static void dummy        (cshell *, void *);
static void finish       (cshell *);
static void join         (cshell *);
static void poke         (cshell *);
static void post         (cshell *, void (*)(cshell *, void *), void *, bool);
static void purge_fd     (cshell *, int);
static void read_post    (cshell *);
static bool run          (cshell *);
static bool server_init  (cshell *);
static void set_error    (cshell *, enum cerr);
static void set_min_size (cshell *);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void *ui_thread (void *);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static _Thread_local bool thread_flush    = false;
static _Thread_local bool thread_active   = false;
static _Thread_local bool thread_destroy  = false;
static _Thread_local cshell *thread_owner = nullptr;

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cshell_clear_warnings(cshell *sh)
{
	GUARD(sh);

	enum cerr err;
	enum cerr tmp;

	do
	{
		err = atomic_load(&sh->err);
		tmp = err;
		cerr_clear_warnings(&tmp);
	}
	while (!atomic_compare_exchange_strong(&sh->err, &err, tmp));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cshell_close(cshell *sh)
{
	GUARD(sh);
	LOCK(sh)
	{
		if (atomic_load(&sh->state) == CSHELL_OPEN)
		{
			atomic_store(&sh->state, CSHELL_CLOSING);
			poke(sh);
		}
		else
		{
			set_error(sh, CERR_CALL);
		}
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cshell *
cshell_create(void)
{
	cshell *sh;

	if (!(sh = malloc(sizeof(cshell))))
	{
		goto fail_alloc;
	}

	if (!(sh->frame = cbox_create()))
	{
		goto fail_frame;
	}

	if (!(sh->grids = cref_create()))
	{
		goto fail_grids;
	}

	if (!(sh->config = ccfg_create()))
	{
		goto fail_config;
	}

	if (pthread_mutex_init(&sh->mutex, nullptr) != 0)
	{
		goto fail_mutex;
	}

	if (pthread_cond_init(&sh->cond, nullptr) != 0)
	{
		goto fail_cond;
	}

	if (pipe(sh->fd_post) != 0)
	{
		goto fail_pipe;
	}

	if (pipe(sh->fd_wake) != 0)
	{
		goto fail_pipe2;
	}

	atomic_init(&sh->state,  CSHELL_CLOSED);
	atomic_init(&sh->server, CSHELL_NONE);
	atomic_init(&sh->err,    CERR_NONE);

	fcntl(sh->fd_post[1], F_SETFL, O_NONBLOCK);
	fcntl(sh->fd_wake[1], F_SETFL, O_NONBLOCK);

	snprintf(sh->name, STR_LEN, "%s", DEFAULT_NAME);
	snprintf(sh->tag,  STR_LEN, "%s", DEFAULT_TAG);

	box_strip(sh->frame);
	cbox_default_border(sh->frame, ccolor_black, 10);
	cbox_default_background(sh->frame, ccolor_red);

	sh->cb_close = (struct call){.fn = dummy, .data = nullptr};
	sh->cb_setup = (struct call){.fn = dummy, .data = nullptr};
	sh->cb_open  = (struct call){.fn = dummy, .data = nullptr};
	sh->menu     = (struct menu){0};
	sh->focus    = nullptr;
	sh->damaged  = false;
	sh->w        = 500;
	sh->h        = 300;

	return sh;

	/* errors */

fail_pipe2:
	close(sh->fd_post[0]);
	close(sh->fd_post[1]);
fail_pipe:
	pthread_cond_destroy(&sh->cond);
fail_cond:
	pthread_mutex_destroy(&sh->mutex);
fail_config:
	ccfg_destroy(sh->config);
fail_mutex:
	cref_destroy(sh->grids);
fail_grids:
	cbox_destroy(sh->frame);
fail_frame:
	free(sh);
fail_alloc:
	return nullptr;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

nullptr_t
cshell_destroy(cshell *sh)
{
	if (sh == thread_owner)
	{
		thread_destroy = true;
	}
	else if (sh)
	{
		if (atomic_load(&sh->state) == CSHELL_OPENING
		 || atomic_load(&sh->state) == CSHELL_OPEN)
		{
			poke(sh);
		}

		join(sh);
		destroy(sh);
	}

	return nullptr;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cerr
cshell_error(const cshell *sh)
{
	return sh ? atomic_load(&sh->err) : CERR_INVALID;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cshell_join(cshell *sh)
{
	GUARD(sh);
	GUARD_THREAD(sh);

	join(sh);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cshell_name(cshell *sh, const char *name)
{
	GUARD(sh);
	LOCK(sh)
	{
		snprintf(sh->name, STR_LEN, "%s", name ? name : DEFAULT_NAME);
	}

	post(sh, apply_name, nullptr, false);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cshell_on_close(cshell *sh, void (*fn)(cshell *, void *), void *data)
{
	GUARD(sh);
	LOCK(sh)
	{
		sh->cb_close.fn   = fn ? fn : dummy;
		sh->cb_close.data = data;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cshell_on_open(cshell *sh, void (*fn)(cshell *, void *), void *data)
{
	GUARD(sh);
	LOCK(sh)
	{
		sh->cb_open.fn   = fn ? fn : dummy;
		sh->cb_open.data = data;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cshell_on_setup(cshell *sh, void (*fn)(cshell *, void *), void *data)
{
	GUARD(sh);
	LOCK(sh)
	{
		sh->cb_setup.fn   = fn ? fn : dummy;
		sh->cb_setup.data = data;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cshell_open(cshell *sh, enum cshell_server server, const char *tag)
{
	GUARD(sh);
	GUARD_THREAD(sh);
	LOCK(sh)
	{
		if (atomic_load(&sh->state) == CSHELL_CLOSED)
		{
			snprintf(sh->tag, STR_LEN, "%s", tag ? tag : DEFAULT_TAG);
			atomic_store(&sh->state, CSHELL_OPENING);
			atomic_store(&sh->server, server);	

			if (pthread_create(&sh->thread, nullptr, ui_thread, sh) != 0)
			{
				set_error(sh, CERR_THREAD);
				atomic_store(&sh->state, CSHELL_CLOSED);
				pthread_cond_broadcast(&sh->cond);
			}
			else
			{
				pthread_detach(sh->thread);
			}
		}
		else
		{
			set_error(sh, CERR_CALL);
		}
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cshell_post(cshell *sh, void (*fn)(cshell *, void *), void *data)
{
	GUARD(sh);
	GUARD_THREAD(sh);
	
	post(sh, fn ? fn : dummy, data, true);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cshell_self(const cshell *sh)
{
	GUARD(sh, false);

	return sh == thread_owner;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cshell_server
cshell_server(const cshell *sh)
{
	GUARD(sh, CSHELL_NONE);

	return atomic_load(&sh->state) == CSHELL_OPEN ? atomic_load(&sh->server) : CSHELL_NONE;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cshell_state
cshell_state(const cshell *sh)
{
	GUARD(sh, false);

	return atomic_load(&sh->state);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cshell_use_grid(cshell *sh, cgrid *gr)
{
	GUARD(sh);
	LOCK(sh)
	{
		if (atomic_load(&sh->state) == CSHELL_CLOSING
		|| (atomic_load(&sh->state) == CSHELL_OPEN)
		|| (atomic_load(&sh->state) == CSHELL_OPENING && sh != thread_owner)
		|| (cgrid_locked(gr)))
		{
			set_error(sh, CERR_CALL);
		}
		else
		{
			cref_push(sh->grids, gr);
			set_error(sh, cref_error(sh->grids));
		}
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cshell_wait(cshell *sh)
{
	GUARD(sh);
	GUARD_THREAD(sh);
	LOCK(sh)
	{
		while (atomic_load(&sh->state) == CSHELL_OPENING
		   || (atomic_load(&sh->state) == CSHELL_CLOSING)
		   || (atomic_load(&sh->state) == CSHELL_CLOSED && !cerr_critical(atomic_load(&sh->err))))
		{
			pthread_cond_wait(&sh->cond, &sh->mutex);
		}
	}
}

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

void
shell_damage(void)
{
	if (!thread_flush
	  && thread_active
	  && thread_owner)
	{
		SERVER(thread_owner, damage, SHELL_MAIN);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
shell_send_event(struct cevent ev, enum shell_target target)
{
	/* Always called by server backends. */
	/* Always called from the UI thread. */
	/* Never called with a locked mutex. */

	cshell *sh = thread_owner;

	if (target == SHELL_MENU)
	{
		ev_redirect(sh, ev);
		return;
	}

	event_print(ev, "shell");
	switch (ev.type)
	{
		case CEVENT_BUTTON_PRESS:
			ev_button(sh, ev);
			break;

		case CEVENT_BUTTON_RELEASE:
			ev_button(sh, ev);
			break;

		case CEVENT_REDRAW:
			ev_redraw(sh, ev);
			break;

		case CEVENT_TRANSFORM:
			ev_transform(sh, ev);
			break;

		case CEVENT_OPEN:
			ev_open(sh);
			break;

		case CEVENT_CLOSE:
			cshell_close(sh);
			break;

		case CEVENT_FAIL:
			set_error(sh, CERR_DISPLAY);
			break;

		default:
			break;
	}	
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
apply_name(cshell *sh, void *data)
{
	char tmp[STR_LEN];
	
	(void)data;

	LOCK(sh)
	{
		snprintf(tmp, STR_LEN, "%s", sh->name);
	}

	SERVER(sh, rename, SHELL_MAIN, tmp);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
callback(cshell *sh, struct call *cb)
{
	struct call tmp;

	LOCK(sh)
	{
		tmp = *cb;
	}

	tmp.fn(sh, tmp.data);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
conf_grids(cshell *sh)
{
	struct cevent ev =
	{
		.type   = CEVENT_CONFIG,
		.config = sh->config,
	};

	CREF_FOR_EACH(sh->grids, cgrid, gr, i)
	{
		grid_send_event(gr, ev);
		if (i == 0)
		{
			sh->w = grid_w(gr) + FRAME(sh);
			sh->h = grid_h(gr) + FRAME(sh);
		}
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
conf_init(cshell *sh)
{
	if (cutil_env_exists(ENV_NO_CONFIG))
	{
		return;
	}

	ccfg_clear_sources(sh->config);
	ccfg_push_source(sh->config, getenv(ENV_CONFIG));
	ccfg_push_std_source(sh->config, "cassette/cgui.ccfg");
	ccfg_push_std_source(sh->config, "cgui.ccfg");

	ccfg_clear_params(sh->config);
	ccfg_push_param(sh->config, CONFIG_PARAM, sh->tag);
	ccfg_load(sh->config);

	set_error(sh, ccfg_error(sh->config));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
conf_menu(cshell *sh)
{
	struct cevent ev =
	{
		.type   = CEVENT_CONFIG,
		.config = sh->config,
	};

	menu_send_event(&sh->menu, ev);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
conf_shell(cshell *sh)
{
	cbox_config(sh->frame, sh->config, "shell");
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
destroy(cshell *sh)
{
	pthread_mutex_destroy(&sh->mutex);
	pthread_cond_destroy(&sh->cond);
	ccfg_destroy(sh->config);
	cref_destroy(sh->grids);
	cbox_destroy(sh->frame);
	close(sh->fd_post[0]);
	close(sh->fd_post[1]);
	close(sh->fd_wake[0]);
	close(sh->fd_wake[1]);
	free(sh);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_button(cshell *sh, struct cevent ev)
{
	if (ev.type == CEVENT_BUTTON_RELEASE)
	{
		return;
	}

	if (ev.button == 3)
	{
		SERVER(sh, show, SHELL_MENU, sh->tag, 200, 500);
	}

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_open(cshell *sh)
{
	LOCK(sh)
	{
		atomic_store(&sh->state, CSHELL_OPEN);
		pthread_cond_broadcast(&sh->cond);
		thread_active = true;
	}

	CREF_FOR_EACH(sh->grids, cgrid, gr, i)
	{
		grid_send_event(gr, event_open);
	}

	callback(sh, &sh->cb_open);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_redirect(cshell *sh, struct cevent ev)
{
	switch (menu_send_event(&sh->menu, ev))
	{
		case MENU_DAMAGE:
			SERVER(sh, damage, SHELL_MENU);
			break;

		case MENU_HIDE:
			SERVER(sh, hide, SHELL_MENU);
			break;

		case MENU_FAIL:
			set_error(sh, CERR_MENU);
			break;

		case MENU_IDLE:
			break;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_redraw(cshell *sh, struct cevent ev)
{
	if (sh->damaged)
	{	
		cairo_set_operator(ev.redraw_ctx, CAIRO_OPERATOR_SOURCE);
		cbox_transform(sh->frame, 0, 0, sh->w, sh->h);
		cbox_draw(sh->frame, ev.redraw_ctx);
		sh->damaged = false;
	}

	if (sh->focus)
	{
		grid_send_event(sh->focus, ev);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_transform(cshell *sh, struct cevent ev)
{
	sh->w       = ev.transform_w;
	sh->h       = ev.transform_h;
	sh->focus   = nullptr;
	sh->damaged = true;

	CREF_FOR_EACH(sh->grids, cgrid, gr, i)
	{
		if (grid_w(gr) + FRAME(sh) <= sh->w
		 && grid_h(gr) + FRAME(sh) <= sh->h)
		{
			if (sh->focus)
			{
				if (grid_w(gr) < grid_w(sh->focus)
				 || grid_h(gr) < grid_h(sh->focus))
				{
					continue;
				}
			}
			sh->focus = gr;
		}
	}

	if (sh->focus)
	{
		ev.transform_x += FRAME(sh) / 2;
		ev.transform_y += FRAME(sh) / 2;
		ev.transform_w -= FRAME(sh);
		ev.transform_h -= FRAME(sh);

		grid_send_event(sh->focus, ev);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
finish(cshell *sh)
{
	CREF_FOR_EACH_REV(sh->grids, cgrid, gr, i)
	{
		grid_send_event(gr, event_close);
		cref_purge(sh->grids, i);
	}

	callback(sh,  &sh->cb_close);
	SERVER(sh, hide, SHELL_MENU);
	SERVER(sh, hide, SHELL_MAIN);
	SERVER(sh, kill);

	LOCK(sh)
	{
		purge_fd(sh, sh->fd_post[0]);
		purge_fd(sh, sh->fd_wake[0]);
		atomic_store(&sh->state, CSHELL_CLOSED);
		pthread_cond_broadcast(&sh->cond);
	}

	if (thread_destroy)
	{
		destroy(sh);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
join(cshell *sh)
{
	LOCK(sh)
	{
		while (atomic_load(&sh->state) != CSHELL_CLOSED)
		{
			pthread_cond_wait(&sh->cond, &sh->mutex);
		}
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
poke(cshell *sh)
{
	while (write(sh->fd_wake[1], "\1", 1) < 0 && errno == EINTR) {}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
post(cshell *sh, void (*fn)(cshell *, void *), void *data, bool warn)
{
	struct call cb = {.fn = fn, .data = data};

	if (sh == thread_owner)
	{
		fn(sh, data);
	}
	else
	{
		LOCK(sh)
		{
			while (atomic_load(&sh->state) == CSHELL_OPENING
			    || atomic_load(&sh->state) == CSHELL_OPEN)
			{
				if (write(sh->fd_post[1], &cb, sizeof(cb)) == sizeof(cb))
				{
					warn = false;
					break;
				}
				else if (errno == EAGAIN)
				{
					pthread_cond_wait(&sh->cond, &sh->mutex);
				}
	
				/* On EINTR loop back again.                   */
				/* Other errors should not be possible at all. */		
			}

			set_error(sh, warn ? CERR_CALL : CERR_NONE);
		}
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
purge_fd(cshell *sh, int fd)
{
	struct pollfd pfd = { fd, POLLIN, 0 };
	uint8_t buf[1024];
	bool err = false;

	while (!err)
	{
		pfd.revents = 0;
		switch (poll(&pfd, 1, 0))
		{
			case 0:
				return;

			case -1:
				err = errno != EINTR;
				break;

			default:
				err = read(fd, buf, sizeof(buf)) < 0 && errno != EINTR;
				break;
		}
	}

	set_error(sh, err ? CERR_THREAD : CERR_NONE);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
read_post(cshell *sh)
{
	struct call cb;
	size_t n = 0;
	ssize_t m;

	while (n < sizeof(cb))
	{
		if ((m = read(sh->fd_post[0], (uint8_t *)&cb + n, sizeof(cb) - n)) > 0)
		{
			n += m;
		}
		else if (m == 0 || errno != EINTR)
		{
			set_error(sh, CERR_THREAD);
			return;
		}
	}

	LOCK(sh)
	{
		pthread_cond_broadcast(&sh->cond);
	}

	cb.fn(sh, cb.data);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
run(cshell *sh)
{
	const int poll_err = POLLERR | POLLHUP | POLLNVAL;
	bool flush = thread_flush | thread_destroy;
	struct pollfd pfd[3] =
	{
		{ sh->fd_post[0], POLLIN, 0 },
		{ sh->fd_wake[0], POLLIN, 0 },
		{ sh->fd_server,  POLLIN, 0 },
	};

	switch (poll(pfd, flush ? 1 : 3, flush ? 0 : -1))
	{
		case 0:
			return false;

		case -1:
			set_error(sh, errno == EINTR ? CERR_NONE : CERR_THREAD);
			return true;

		default:
			break;
	}

	if (pfd[0].revents & POLLIN)
	{
		read_post(sh);
	}

	if (pfd[1].revents & POLLIN)
	{
		thread_flush = true;
	}

	if (pfd[2].revents & POLLIN)
	{
		SERVER(sh, read);
	}

	if (pfd[0].revents & poll_err
	 || pfd[1].revents & poll_err
	 || pfd[2].revents & poll_err)
	{
		set_error(sh, CERR_THREAD);
	}

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
server_init(cshell *sh)
{
	enum cshell_server srv = atomic_load(&sh->server);

	if (srv & CSHELL_WAYLAND && (sh->fd_server = wayland_init(&sh->wl)) != -1)
	{
		atomic_store(&sh->server, CSHELL_WAYLAND);
	}
	else if (srv & CSHELL_X11 && (sh->fd_server = x11_init(&sh->x11)) != -1)
	{
		atomic_store(&sh->server, CSHELL_X11);
	}
	else
	{
		atomic_store(&sh->server, CSHELL_NONE);
		return false;
	}

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
set_error(cshell *sh, enum cerr code)
{
	enum cerr err;
	enum cerr tmp;

	do
	{
		err = atomic_load(&sh->err);
		tmp = err;
		cerr_set(&tmp, code);
	}
	while (!atomic_compare_exchange_strong(&sh->err, &err, tmp));

	if (cerr_critical(code))
	{
		poke(sh);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
set_min_size(cshell *sh)
{
	uint32_t w = UINT32_MAX;
	uint32_t h = UINT32_MAX;

	if (cref_length(sh->grids) > 0)
	{
		CREF_FOR_EACH(sh->grids, cgrid, gr, i)
		{
			w = w > grid_w(gr) ? grid_w(gr) : w;
			h = h > grid_h(gr) ? grid_h(gr) : w;
		}
	
		SERVER(sh, hint, w + FRAME(sh), h + FRAME(sh));
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void *
ui_thread(void *arg)
{
	cshell *sh = arg;
	thread_owner = sh;

	conf_init (sh);
	conf_menu (sh);
	conf_shell(sh);

	if (server_init(sh))
	{
		callback(sh, &sh->cb_setup);
		conf_grids(sh);
		SERVER(sh, config, sh->config);
		SERVER(sh, show, SHELL_MAIN, sh->tag, sh->w, sh->h);
		apply_name(sh, nullptr);
		set_min_size(sh);

		while (run(sh))
		{
			SERVER(sh, commit, SHELL_MAIN);
			SERVER(sh, commit, SHELL_MENU);
		}
	}
	else
	{
		set_error(sh, CERR_DISPLAY);
	}

	finish(sh);
	pthread_exit(nullptr);
}

/************************************************************************************************************/
/* STATIC - NOOP ********************************************************************************************/
/************************************************************************************************************/

static void
dummy(cshell *sh, void *data)
{
	(void)sh;
	(void)data;
}

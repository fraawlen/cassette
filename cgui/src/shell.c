/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#include <cairo/cairo.h>
#include <cassette/cgui.h>
#include <cassette/cobj.h>
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <pthread.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "menu.h"
#include "shell.h"
#include "wayland.h"
#include "x11.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define GUARD(SH, ...)   if (cerr_critical(cshell_error(SH))) { return __VA_OPT__(__VA_ARGS__); }
#define GUARD_THREAD(SH) if (SH == thread_owner) { set_error(SH, CERR_CALL); return; }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

#define LOCK(SH) \
	for ( \
		int b = 1; \
		b && (pthread_mutex_lock(&SH->mutex) | 1); \
		b = pthread_mutex_unlock(&SH->mutex) & 0)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

#define SERVER(SH, FN, ...) \
	switch(atomic_load(&SH->server)) \
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
	int fd_call[2];
	int fd_wake[2];
	int fd_server;

	/* states */

	_Atomic enum cshell_server server;
	_Atomic enum cshell_state  state;
	_Atomic enum cerr err;

	uint32_t w;
	uint32_t h;

	/* callback functions */

	struct call cl_open;
	struct call cl_close;

	/* backends */

	union
	{
		struct x11 x11;
		struct wayland wl;
	};

	/* contents */

	struct menu menu;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void  destroy       (cshell *);
static void  dummy         (cshell *, void *);
static void  finish_close  (cshell *);
static void  finish_open   (cshell *);
static void  join          (cshell *);
static void  menu_redirect (cshell *, struct cevent);
static void  read_invoke   (cshell *);
static bool  run           (cshell *);
static bool  server_init   (cshell *, enum cshell_server);
static void  set_error     (cshell *, enum cerr);
static void *ui_thread     (void   *);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static _Thread_local bool thread_flush    = false;
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
	enum cshell_state opened = CSHELL_OPEN;

	GUARD(sh);
	LOCK(sh)
	{
		if (atomic_compare_exchange_strong(&sh->state, &opened, CSHELL_CLOSING))
		{
			while (write(sh->fd_wake[1], "\1", 1) < 0 && errno == EINTR) {}
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

	if (pthread_mutex_init(&sh->mutex, nullptr) != 0)
	{
		goto fail_mutex;
	}

	if (pthread_cond_init(&sh->cond, nullptr) != 0)
	{
		goto fail_cond;
	}

	atomic_init(&sh->server, CSHELL_NONE);
	atomic_init(&sh->state,  CSHELL_INIT);
	atomic_init(&sh->err,    CERR_NONE);

	sh->cl_close = (struct call){.fn = dummy, .data = nullptr};
	sh->cl_open  = (struct call){.fn = dummy, .data = nullptr};

	sh->w = 0;
	sh->h = 0;

	return sh;

	/* errors */

fail_cond:
	pthread_mutex_destroy(&sh->mutex);
fail_mutex:
	free(sh);
fail_alloc:
	return nullptr;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

nullptr_t
cshell_destroy(cshell *sh)
{
	if (!sh)
	{
		return nullptr;
	}

	if (sh == thread_owner)
	{
		cshell_close(sh);
		thread_destroy = true;
	}
	else
	{
		switch (atomic_load(&sh->state))
		{
			case CSHELL_OPENING:
				cshell_wait(sh);
				/* fallthrough */

			case CSHELL_OPEN:
				cshell_close(sh);
				/* fallthrough */

			case CSHELL_CLOSING:
				join(sh);
				/* fallthrough */

			case CSHELL_CLOSED:
			case CSHELL_INIT:
				destroy(sh);
				break;
		}
	}

	return nullptr;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cerr
cshell_error(cshell *sh)
{
	return sh ? atomic_load(&sh->err) : CERR_INVALID;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cshell_invoke(cshell *sh, void (*fn)(cshell *, void *), void *data)
{
	struct call cl = {.fn = fn, .data = data};

	GUARD(sh);
	GUARD_THREAD(sh);
	LOCK(sh)
	{
		while (atomic_load(&sh->state) == CSHELL_OPEN)
		{
			if (write(sh->fd_call[1], &cl, sizeof(cl)) == sizeof(cl))
			{
				goto done;
			}
			else if (errno == EAGAIN)
			{
				pthread_cond_wait(&sh->cond, &sh->mutex);
			}
		}

		set_error(sh, CERR_CALL);
	done:

		/* On EINTR loop back again.                   */
		/* Other errors should not be possible at all. */
	}
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
cshell_on_close(cshell *sh, void (*fn)(cshell *, void *), void *data)
{
	GUARD(sh);
	LOCK(sh)
	{
		sh->cl_close.fn   = fn ? fn : dummy;
		sh->cl_close.data = data;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cshell_on_open(cshell *sh, void (*fn)(cshell *, void *), void *data)
{
	GUARD(sh);
	LOCK(sh)
	{
		sh->cl_open.fn   = fn ? fn : dummy;
		sh->cl_open.data = data;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cshell_open(cshell *sh, enum cshell_server server)
{
	enum cshell_state init   = CSHELL_INIT;
	enum cshell_state closed = CSHELL_CLOSED;

	GUARD(sh);
	GUARD_THREAD(sh);
	LOCK(sh)
	{
		if (!atomic_compare_exchange_strong(&sh->state, &init,   CSHELL_OPENING)
		 && !atomic_compare_exchange_strong(&sh->state, &closed, CSHELL_OPENING))
		{
			goto fail_open;
		}

		if (!server_init(sh, server))
		{
			goto fail_server;
		}

		if (pipe(sh->fd_call) != 0)
		{
			goto fail_pipe;
		}

		if (pipe(sh->fd_wake) != 0)
		{
			goto fail_pipe2;
		}

		if (fcntl(sh->fd_call[1], F_SETFL, O_NONBLOCK) == -1
		 || fcntl(sh->fd_wake[1], F_SETFL, O_NONBLOCK) == -1)
		{
			goto fail_flag;
		}

		if (pthread_create(&sh->thread, nullptr, ui_thread, sh) == 0)
		{
			pthread_detach(sh->thread);
			goto done;
		}

		/* errors */

	fail_flag:
		close(sh->fd_wake[0]);
		close(sh->fd_wake[1]);
	fail_pipe2:
		close(sh->fd_call[0]);
		close(sh->fd_call[1]);
	fail_pipe:
		set_error(sh, CERR_THREAD);
	fail_server:
		set_error(sh, CERR_DISPLAY);
	fail_open:
		set_error(sh, CERR_CALL);
		atomic_store(&sh->state, CSHELL_CLOSED);

		/* end */

	done:
		pthread_cond_broadcast(&sh->cond);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cshell_self(cshell *sh)
{
	return sh == thread_owner;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cshell_server
cshell_server(cshell *sh)
{
	GUARD(sh, CSHELL_NONE);

	return atomic_load(&sh->server);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cshell_state
cshell_state(cshell *sh)
{
	GUARD(sh, false);

	return atomic_load(&sh->state);
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
		    || atomic_load(&sh->state) == CSHELL_INIT)
		{
			pthread_cond_wait(&sh->cond, &sh->mutex);
		}
	}
}

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

void
shell_send_event(struct cevent ev, enum shell_target target)
{
	/* Always called from the UI thread */
	/* Never called with a locked mutex */

	cshell *sh = thread_owner;

	/* menu event redirection */

	if (target == SHELL_MENU)
	{
		menu_redirect(sh, ev);
		return;
	}

	/* main shell event handling */

	switch(ev.type)
	{
		case CEVENT_BUTTON_PRESS:
			if (ev.button == 3)
			{
				SERVER(sh, show, SHELL_MENU, 200, 500);
			}
			break;

		case CEVENT_BUTTON_RELEASE:
			// TODO
			break;

		case CEVENT_REDRAW:
			cairo_set_operator(ev.redraw_ctx, CAIRO_OPERATOR_SOURCE);
			cairo_set_source_rgba(ev.redraw_ctx, 0.0, 0.0, 0.0, 1.0);
			cairo_paint(ev.redraw_ctx);
			cairo_set_source_rgba(ev.redraw_ctx, 1.0, 0.0, 0.0, 0.5);
			cairo_rectangle(ev.redraw_ctx, 20, 20, sh->w - 40, sh->h - 40);
			cairo_fill(ev.redraw_ctx);
			break;

		case CEVENT_TRANSFORM:
			sh->w = ev.transform_w;
			sh->h = ev.transform_h;
			break;

		case CEVENT_CLOSE:
			cshell_close(sh);
			break;

		case CEVENT_OPEN:
			finish_open(sh);
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
destroy(cshell *sh)
{
	pthread_mutex_destroy(&sh->mutex);
	pthread_cond_destroy(&sh->cond);
	free(sh);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
finish_close(cshell *sh)
{
	struct call cl;

	LOCK(sh)
	{
		cl = sh->cl_close;
	}

	cl.fn(sh, cl.data);

	LOCK(sh)
	{
		SERVER(sh, hide, SHELL_MENU);
		SERVER(sh, hide, SHELL_MAIN);
		SERVER(sh, kill);
		close(sh->fd_call[1]);
		close(sh->fd_call[0]);
		close(sh->fd_wake[1]);
		close(sh->fd_wake[0]);
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
finish_open(cshell *sh)
{
	struct call cl;

	LOCK(sh)
	{
		cl = sh->cl_open;
	}
	
	cl.fn(sh, cl.data);

	LOCK(sh)
	{
		atomic_store(&sh->state, CSHELL_OPEN);
		pthread_cond_broadcast(&sh->cond);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
join(cshell *sh)
{
	LOCK(sh)
	{
		while (atomic_load(&sh->state) != CSHELL_CLOSED
		    && atomic_load(&sh->state) != CSHELL_INIT)
		{
			pthread_cond_wait(&sh->cond, &sh->mutex);
		}
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
menu_redirect(cshell *sh, struct cevent ev)
{
	switch (menu_send_event(&sh->menu, ev))
	{
		case MENU_DAMAGE:
			SERVER(sh, damage, SHELL_MENU);
			break;

		case MENU_HIDE:
			SERVER(sh, hide, SHELL_MENU);
			break;

		case MENU_IDLE:
			break;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
read_invoke(cshell *sh)
{
	struct call cl;
	size_t n = 0;
	ssize_t m;

	while (n < sizeof(cl))
	{
		if ((m = read(sh->fd_call[0], (uint8_t *)&cl + n, sizeof(cl) - n)) > 0)
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

	cl.fn(sh, cl.data);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
run(cshell *sh)
{
	const int poll_err = POLLERR | POLLHUP | POLLNVAL;
	bool flush = thread_flush;
	struct pollfd pfd[3] =
	{
		{ sh->fd_call[0], POLLIN, 0 },
		{ sh->fd_wake[0], POLLIN, 0 },
		{ sh->fd_server,  POLLIN, 0 },
	};

	/* detect activity */

	switch (poll(pfd, flush ? 1 : 3, flush ? 0 : -1))
	{
		case 0:
			return false;

		case -1:
			set_error(sh, errno == EINTR ? CERR_NONE : CERR_THREAD);
			break;

		default:
			break;
	}

	/* process input */

	if (pfd[0].revents & POLLIN)
	{
		read_invoke(sh);
	}

	if (pfd[1].revents & POLLIN)
	{
		thread_flush = true;
	}

	if (pfd[2].revents & POLLIN)
	{
		SERVER(sh, read);
	}

	/* end */

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
server_init(cshell *sh, enum cshell_server server)
{
	if (server == CSHELL_NONE)
	{
		sh->fd_server = -1;
	}
	else if (server & CSHELL_WAYLAND && (sh->fd_server = wayland_init(&sh->wl)) != -1)
	{
		atomic_store(&sh->server, CSHELL_WAYLAND);
	}
	else if (server & CSHELL_X11 && (sh->fd_server = x11_init(&sh->x11)) != -1)
	{
		atomic_store(&sh->server, CSHELL_X11);
	}
	else
	{
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
		thread_flush = true;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void *
ui_thread(void *arg)
{
	cshell *sh = arg;
	thread_owner = sh;

	SERVER(sh, show, SHELL_MAIN, 500, 300);
	while (run(sh))
	{
		SERVER(sh, commit, SHELL_MAIN);
		SERVER(sh, commit, SHELL_MENU);
	}

	finish_close(sh);
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

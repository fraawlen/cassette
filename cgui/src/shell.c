/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#include <cairo/cairo.h>
#include <cassette/cgui.h>
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

#include "shell.h"
#include "wayland.h"
#include "x11.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define GUARD(OBJ, ...)   if (!OBJ || cerr_critical(shell_error(OBJ))) { return __VA_OPT__(__VA_ARGS__); }
#define GUARD_THREAD(OBJ) if (OBJ == thread_owner) { shell_set_error(OBJ, CERR_CALL); return; }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

#define ROUTE(SH, FN, ...) \
	switch(atomic_load(&SH->backend)) \
	{ \
		case CSHELL_WAYLAND: \
			wayland_##FN(&SH->wl __VA_OPT__(, __VA_ARGS__)); \
			break; \
		case CSHELL_X11: \
			x11_##FN(&SH->x __VA_OPT__(, __VA_ARGS__)); \
			break; \
		default: \
			break; \
	}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum state
{
	INIT,
	OPENING,
	OPEN,
	CLOSING,
	CLOSED,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct cshell
{
	/* multi-threading */

	pthread_mutex_t mutex;
	pthread_cond_t cond;
	pthread_t thread;
	int fd_call[2];
	int fd_poke[2];
	int fd_backend;

	/* callbacks functions */

	void (*fn_close)(cshell *, void *);
	void (*fn_open)(cshell *, void *);

	/* callbacks data */

	void *data_close;
	void *data_open;

	/* states */

	_Atomic enum cshell_backend backend;
	_Atomic enum state state;
	_Atomic enum cerr err;
	uint32_t w;
	uint32_t h;

	/* backends */

	union
	{
		struct wayland wl;
		struct x11 x;
	};
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct call
{
	void (*fn)(cshell *, void *);
	void *data;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void  backend_menu_close      (cshell *);
static bool  backend_menu_open       (cshell *, uint32_t, uint32_t);
//static void  backend_menu_redraw     (cshell *);
static void  backend_server_commit   (cshell *);
static void  backend_server_dispatch (cshell *);
static bool  backend_server_init     (cshell *, enum cshell_backend);
static void  backend_server_kill     (cshell *);
static void  backend_shell_close     (cshell *);
static bool  backend_shell_open      (cshell *, uint32_t, uint32_t);
static void  backend_shell_redraw    (cshell *);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void  dispatch_invoke (cshell *);
static void  dummy           (cshell *, void *);
static bool  run             (cshell *);
static void *thread          (void   *);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static _Thread_local cshell *thread_owner = nullptr;

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

enum cshell_backend
cshell_backend(const cshell *sh)
{
	GUARD(sh, CSHELL_NONE);

	return atomic_load(&sh->state) == OPEN ? atomic_load(&sh->backend) : CSHELL_NONE;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cshell_clear_warnings(cshell *sh)
{
	GUARD(sh);

	enum cerr err;
	enum cerr tmp;

	do
	{
		err = shell_error(sh);
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

	pthread_mutex_lock(&sh->mutex);

	if (atomic_load(&sh->state) != OPEN)
	{
		shell_set_error(sh, CERR_CALL);
	}
	else
	{
		while (write(sh->fd_poke[1], "\1", 1) < 0 && errno == EINTR) {}
	}
	
	pthread_mutex_unlock(&sh->mutex);
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

	atomic_init(&sh->backend, CSHELL_NONE);
	atomic_init(&sh->err, CERR_NONE);
	atomic_init(&sh->state, INIT);

	sh->data_close = nullptr;
	sh->data_open  = nullptr;
	sh->fn_close   = dummy;
	sh->fn_open    = dummy;

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
	if (sh)
	{
		if (sh == thread_owner)
		{
			shell_set_error(sh, CERR_CALL);
		}
		else
		{
			switch (atomic_load(&sh->state))
			{
				case OPEN:
				case OPENING:
					cshell_close(sh);
					/* fallthrough */

				case CLOSING:
					pthread_join(sh->thread, nullptr);
					/* fallthrough */
			
				case INIT:
				case CLOSED:
					pthread_mutex_destroy(&sh->mutex);
					pthread_cond_destroy(&sh->cond);
					free(sh);
					break;
			}
		}
	}

	return nullptr;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cerr
cshell_error(const cshell *sh)
{
	return sh ? shell_error(sh) : CERR_INVALID;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cshell_invoke(cshell *sh, void (*fn)(cshell *, void *), void *data)
{
	GUARD(sh);
	GUARD_THREAD(sh);

	struct call cl = {.fn = fn ? fn : dummy, .data = data};

	pthread_mutex_lock(&sh->mutex);

	for (;;)
	{
		if (atomic_load(&sh->state) != OPEN)
		{
			shell_set_error(sh, CERR_CALL);
			break;
		}
		else if (write(sh->fd_call[1], &cl, sizeof(cl)) == (int)sizeof(cl))
		{
			break;
		}
		else if (errno == EAGAIN)
		{
			pthread_cond_wait(&sh->cond, &sh->mutex);
		}
		else if (errno != EINTR)
		{
			shell_set_error(sh, CERR_THREAD);
			break;
		}	
	}

	pthread_mutex_unlock(&sh->mutex);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cshell_join(cshell *sh)
{
	GUARD(sh);
	GUARD_THREAD(sh);

	pthread_mutex_lock(&sh->mutex);

	while (atomic_load(&sh->state) != CLOSED)
	{
		pthread_cond_wait(&sh->cond, &sh->mutex);
	}

	pthread_mutex_unlock(&sh->mutex);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cshell_on_close(cshell *sh, void (*fn)(cshell *, void *), void *data)
{
	GUARD(sh);

	sh->fn_close   = fn ? fn   : dummy;
	sh->data_close = fn ? data : nullptr;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cshell_on_open(cshell *sh, void (*fn)(cshell *, void *), void *data)
{
	GUARD(sh);

	sh->fn_open   = fn ? fn   : dummy;
	sh->data_open = fn ? data : nullptr;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cshell_open(cshell *sh)
{
	GUARD(sh);
	GUARD_THREAD(sh);

	pthread_mutex_lock(&sh->mutex);

	/* setups */

	if (!atomic_compare_exchange_strong(&sh->state, &(enum state){INIT},   OPENING)
	 && !atomic_compare_exchange_strong(&sh->state, &(enum state){CLOSED}, OPENING))
	{
		goto fail_open;
	}

	if (!backend_server_init(sh, CSHELL_ANY))
	{
		goto fail_back;
	}

	if (!backend_shell_open(sh, 500, 300))
	{
		goto fail_win;
	}

	if (pipe(sh->fd_call) != 0)
	{
		goto fail_pipe;
	}

	if (pipe(sh->fd_poke) != 0)
	{
		goto fail_pipe2;
	}

	if (fcntl(sh->fd_call[1], F_SETFL, fcntl(sh->fd_call[1], F_GETFL, 0) | O_NONBLOCK) == -1
	 || fcntl(sh->fd_poke[1], F_SETFL, fcntl(sh->fd_poke[1], F_GETFL, 0) | O_NONBLOCK) == -1)
	{
		goto fail_flags;
	}
	
	if (pthread_create(&sh->thread, nullptr, thread, sh) != 0)
	{
		goto fail_thread;
	}

	/* end */

	sh->w = 500;
	sh->h = 300;

	atomic_store(&sh->state, OPEN);
	pthread_cond_broadcast(&sh->cond);
	pthread_mutex_unlock(&sh->mutex);

	return;

	/* error cleanup */

fail_thread:
fail_flags:
	close(sh->fd_poke[0]);
	close(sh->fd_poke[1]);
fail_pipe2:
	close(sh->fd_call[0]);
	close(sh->fd_call[1]);
fail_pipe:
	shell_set_error(sh, CERR_THREAD);
fail_win:
	backend_server_kill(sh);
fail_back:
	shell_set_error(sh, CERR_DISPLAY);
	atomic_store(&sh->state, CLOSED);
fail_open:
	shell_set_error(sh, CERR_CALL);
	pthread_cond_broadcast(&sh->cond);
	pthread_mutex_unlock(&sh->mutex);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cshell_opened(const cshell *sh)
{
	GUARD(sh, false);

	return atomic_load(&sh->state) == OPEN;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cshell_self(const cshell *sh)
{
	GUARD(sh, false);

	return sh == thread_owner;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cshell_wait(cshell *sh)
{
	GUARD(sh);
	GUARD_THREAD(sh);

	pthread_mutex_lock(&sh->mutex);

	while (atomic_load(&sh->state) == INIT)
	{
		pthread_cond_wait(&sh->cond, &sh->mutex);
	}

	pthread_mutex_unlock(&sh->mutex);
}

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

void
shell_dispatch_event(cshell *sh, struct cevent ev)
{
	switch (ev.type)
	{
		case CEVENT_BUTTON_PRESS:
			backend_menu_close(sh);
			break;

		case CEVENT_BUTTON_RELEASE:
			if (ev.button == 3)
			{
				backend_menu_open(sh, 200, 500);
			}
			break;

		case CEVENT_REDRAW:
			//printf("shell redrawn\n");
			cairo_set_operator(ev.redraw_ctx, CAIRO_OPERATOR_SOURCE);
			if (ev.redraw_shell)
			{
				cairo_set_source_rgba(ev.redraw_ctx, 0.0, 0.0, 0.0, 1.0);
				cairo_paint(ev.redraw_ctx);
				cairo_set_source_rgba(ev.redraw_ctx, 1.0, 0.0, 0.0, 0.5);
				cairo_rectangle(ev.redraw_ctx, 20, 20, sh->w - 40, sh->h - 40);
				cairo_fill(ev.redraw_ctx);
			}
			else
			{
				cairo_set_source_rgba(ev.redraw_ctx, 0.2, 0.2, 0.2, 1.0);
				cairo_paint(ev.redraw_ctx);
			}
			break;

		case CEVENT_TRANSFORM:
			if (sh->w != ev.transform_w || sh->h != ev.transform_h)
			{
				sh->w = ev.transform_w;
				sh->h = ev.transform_h;
				backend_shell_redraw(sh);
				//printf("shell resized\n");
			}
			break;

		case CEVENT_UNKNOWN:
			//printf("unhandled display event\n");
			break;

		case CEVENT_CLOSE:
			cshell_close(sh);
			break;

		case CEVENT_FAIL:
			printf("display connection lost\n");
			shell_set_error(sh, CERR_DISPLAY);
			break;

		case CEVENT_NONE:
			break;

		default:
			break;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cerr
shell_error(const cshell *sh)
{
	return atomic_load(&sh->err);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

uint32_t
shell_h(const cshell *sh)
{
	return sh->h;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
shell_set_error(cshell *sh, enum cerr code)
{
	enum cerr err;
	enum cerr tmp;

	do
	{
		err = shell_error(sh);
		tmp = err;
		cerr_set(&tmp, code);
	}
	while (!atomic_compare_exchange_strong(&sh->err, &err, tmp));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

uint32_t
shell_w(const cshell *sh)
{
	return sh->w;
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
backend_menu_close(cshell *sh)
{
	ROUTE(sh, menu_close);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
backend_menu_open(cshell *sh, uint32_t w, uint32_t h)
{
	switch(atomic_load(&sh->backend))
	{
		case CSHELL_X11:
			return x11_menu_open(&sh->x, w, h);

		case CSHELL_WAYLAND:
			return wayland_menu_open(&sh->wl, w, h);

		default:
			return true;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/*
static void
backend_menu_redraw(cshell *sh)
{
	ROUTE(sh, shell_redraw);
}
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
backend_server_commit(cshell *sh)
{
	ROUTE(sh, server_commit, sh);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
backend_server_dispatch(cshell *sh)
{
	ROUTE(sh, server_dispatch, sh);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
backend_server_init(cshell *sh, enum cshell_backend backend)
{
	if (backend & CSHELL_WAYLAND && wayland_server_init(&sh->wl, &sh->fd_backend))
	{
		atomic_store(&sh->backend, CSHELL_WAYLAND);
	}
	else if (backend & CSHELL_X11 && x11_server_init(&sh->x, &sh->fd_backend))
	{
		atomic_store(&sh->backend, CSHELL_X11);
	}
	else if (backend == CSHELL_NONE)
	{
		sh->fd_backend = -1;
	}
	else
	{
		return false;
	}
	
	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
backend_server_kill(cshell *sh)
{
	ROUTE(sh, server_kill);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
backend_shell_close(cshell *sh)
{
	ROUTE(sh, shell_close);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
backend_shell_open(cshell *sh, uint32_t w, uint32_t h)
{
	switch(atomic_load(&sh->backend))
	{
		case CSHELL_X11:
			return x11_shell_open(&sh->x, w, h);

		case CSHELL_WAYLAND:
			return wayland_shell_open(&sh->wl, w, h);

		default:
			return true;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
backend_shell_redraw(cshell *sh)
{
	ROUTE(sh, shell_redraw);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
dispatch_invoke(cshell *sh)
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
			shell_set_error(sh, CERR_THREAD);
			return;
		}
	}

	pthread_mutex_lock(&sh->mutex);
	pthread_cond_broadcast(&sh->cond);
	pthread_mutex_unlock(&sh->mutex);
	cl.fn(sh, cl.data);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
run(cshell *sh)
{
	const int poll_err = POLLERR | POLLHUP | POLLNVAL;
	bool flush = atomic_load(&sh->state) != OPEN;
	struct pollfd pfd[3] = 
	{
		{ sh->fd_call[0], POLLIN, 0 },
		{ sh->fd_backend, POLLIN, 0 },
		{ sh->fd_poke[0], POLLIN, 0 },
	};

	/* poll events */

	switch (poll(pfd, flush ? 1 : 3, flush ? 0 : -1))
	{
		case -1:
			shell_set_error(sh, errno == EINTR ? CERR_NONE : CERR_THREAD);
			break;

		case 0:
			return false;

		default:
			break;
	}

	/* invocations */

	if (pfd[0].revents & POLLIN)
	{
		dispatch_invoke(sh);
	}

	/* display events */

	if (pfd[1].revents & POLLIN)
	{
		backend_server_dispatch(sh);
	}

	/* shutdown signals */

	if (pfd[2].revents & POLLIN)
	{
		pthread_mutex_lock(&sh->mutex);
		atomic_store(&sh->state, CLOSING);
		pthread_mutex_unlock(&sh->mutex);
	}

	/* end */

	if (pfd[0].revents & poll_err
	 || pfd[1].revents & poll_err
	 || pfd[2].revents & poll_err)
	{
		shell_set_error(sh, CERR_THREAD);
	}

	return !cerr_critical(shell_error(sh));	
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void *
thread(void *arg)
{
	cshell *sh = (cshell*)arg;
	thread_owner = sh;

	/* wait cshell_open() to complete */

	pthread_mutex_lock(&sh->mutex);
	pthread_mutex_unlock(&sh->mutex);

	/* operation                                   */
	/* after a close request poke is received      */
	/* run() will operate in flush mode to process */
	/* the remaining invokes, but stops processing */
	/* backend events.                             */

	sh->fn_open(sh, sh->data_open);
	while (run(sh))
	{
		backend_server_commit(sh);
	}
	sh->fn_close(sh, sh->data_close);

	/* teardown */

	pthread_mutex_lock(&sh->mutex);
	close(sh->fd_call[0]);
	close(sh->fd_call[1]);
	close(sh->fd_poke[0]);
	close(sh->fd_poke[1]);
	backend_menu_close(sh);
	backend_shell_close(sh);
	backend_server_kill(sh);
	atomic_store(&sh->state, CLOSED);
	pthread_cond_broadcast(&sh->cond);
	pthread_mutex_unlock(&sh->mutex);

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

	/* nothing */
}

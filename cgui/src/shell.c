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

#include "display.h"
#include "shell.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define GUARD(OBJ, ...)   if (!OBJ || cerr_critical(shell_error(OBJ))) { return __VA_OPT__(__VA_ARGS__); }
#define GUARD_THREAD(OBJ) if (OBJ == thread_owner) { shell_set_error(OBJ, CERR_CALL); return; }

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
	/* kernel */

	_Atomic enum state state;
	_Atomic enum cerr err;
	pthread_mutex_t mutex;
	pthread_cond_t cond;
	pthread_t thread;
	int fd_call[2];
	int fd_poke[2];

	/* callbacks functions */

	void (*fn_close)(cshell *, void *);
	void (*fn_open)(cshell *, void *);

	/* callbacks data */

	void *data_close;
	void *data_open;

	/* contents */

	cdisplay dp;

	// TODO config
	// TODO layouts

	/* states */

	uint32_t w;
	uint32_t h;
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

static void  dispatch_invoke (cshell *);
static void  dummy           (cshell *, void *);
static bool  flush           (cshell *);
static bool  run             (cshell *);
static void *thread          (void   *);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

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

	atomic_init(&sh->err, CERR_NONE);
	atomic_init(&sh->state, INIT);

	sh->data_close = nullptr;
	sh->data_open  = nullptr;
	sh->fn_close   = dummy;
	sh->fn_open    = dummy;
	sh->dp         = display_none;
	sh->w          = 500;
	sh->h          = 300;

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
		if (atomic_load(&sh->state) == CLOSED 
		 || atomic_load(&sh->state) == INIT)
		{
			pthread_mutex_destroy(&sh->mutex);
			pthread_cond_destroy(&sh->cond);
			free(sh);
		}
		else
		{
			shell_set_error(sh, CERR_CALL);
		}
	}

	return nullptr;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const cdisplay *
cshell_display(const cshell *sh)
{
	GUARD(sh, &display_none);

	return &sh->dp;
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

	if (!display_init(&sh->dp, CDISPLAY_ANY, sh->w, sh->h))
	{
		goto fail_disp;
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
	
	if (pthread_create(&sh->thread, nullptr, thread, sh) == 0)
	{
		pthread_detach(sh->thread);
		atomic_store(&sh->state, OPEN);
		goto done;
	}

	/* error cleanup */
	
fail_flags:
	close(sh->fd_poke[0]);
	close(sh->fd_poke[1]);
fail_pipe2:
	close(sh->fd_call[0]);
	close(sh->fd_call[1]);
fail_pipe:
	shell_set_error(sh, CERR_THREAD);
	display_kill(&sh->dp);
fail_disp:
	shell_set_error(sh, CERR_DISPLAY);
	atomic_store(&sh->state, CLOSED);
fail_open:
	shell_set_error(sh, CERR_CALL);

	/* end */

done:
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
			printf("shell clicked (id = %i)\n", ev.button);
			break;

		case CEVENT_BUTTON_RELEASE:
			printf("shell release (id = %i)\n", ev.button);
			break;

		case CEVENT_REDRAW:
			printf("shell redrawn\n");
			cairo_set_operator(ev.redraw_ctx, CAIRO_OPERATOR_SOURCE);
			cairo_set_source_rgba(ev.redraw_ctx, 0.0, 0.0, 0.0, 1.0);
			cairo_paint(ev.redraw_ctx);
			cairo_set_source_rgba(ev.redraw_ctx, 1.0, 0.0, 0.0, 0.5);
			cairo_rectangle(ev.redraw_ctx, 20, 20, sh->w - 40, sh->h - 40);
			cairo_fill(ev.redraw_ctx);
			break;

		case CEVENT_TRANSFORM:
			if (sh->w != ev.transform_w || sh->h != ev.transform_h)
			{
				sh->w = ev.transform_w;
				sh->h = ev.transform_h;
				display_redraw(&sh->dp);
				printf("shell resized\n");
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
dispatch_invoke(cshell *sh)
{
	struct call cl;

	switch (read(sh->fd_call[0], &cl, sizeof(cl)))
	{
		case sizeof(cl):
			pthread_mutex_lock(&sh->mutex);
			pthread_cond_broadcast(&sh->cond);
			pthread_mutex_unlock(&sh->mutex);
			cl.fn(sh, cl.data);
			break;

		case -1:
			if (errno == EINTR)
			{
				break;
			}
			/* fallthrough */

		default:
			shell_set_error(sh, CERR_THREAD);
			break;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
dummy(cshell *sh, void *data)
{
	(void)sh;
	(void)data;

	/* nothing */
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
flush(cshell *sh)
{
	struct pollfd pfd = {sh->fd_call[0], POLLIN, 0};

	switch (poll(&pfd, 1, 0))
	{
		case -1:
			return errno == EINTR;

		case 0:
			return false;

		default:
			if (pfd.revents & POLLIN)
			{
				dispatch_invoke(sh);
				return true;
			}	
			else
			{
				shell_set_error(sh, CERR_THREAD);
				return false;
			}
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
run(cshell *sh)
{
	const int poll_err = POLLERR | POLLHUP | POLLNVAL;
	struct pollfd pfd[3] = 
	{
		{ sh->fd_poke[0], POLLIN, 0 },
		{ sh->fd_call[0], POLLIN, 0 },
		{ sh->dp.fd,      POLLIN, 0 },
	};

	/* poll events */

	if (poll(pfd, 3, -1) < 0)
	{
		if (errno != EINTR)
		{
			shell_set_error(sh, CERR_THREAD);
		}
		else
		{
			return true;
		}
	}

	/* shutdown signals */

	if (pfd[0].revents & POLLIN)
	{
		return false;
	}

	/* invocations */

	if (pfd[1].revents & POLLIN)
	{
		dispatch_invoke(sh);
	}

	/* display events */

	if (pfd[2].revents & POLLIN)
	{
		display_dispatch(&sh->dp, sh);
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

	/* operation */

	sh->fn_open(sh, sh->data_open);
	display_dispatch(&sh->dp, sh);
	while (run(sh))
	{
		display_commit(&sh->dp, sh);
	}

	/* exiting */

	pthread_mutex_lock(&sh->mutex);
	atomic_store(&sh->state, CLOSING);
	pthread_mutex_unlock(&sh->mutex);

	while (flush(sh)) {}
	sh->fn_close(sh, sh->data_close);

	/* teardown */

	pthread_mutex_lock(&sh->mutex);
	close(sh->fd_call[0]);
	close(sh->fd_call[1]);
	close(sh->fd_poke[0]);
	close(sh->fd_poke[1]);
	display_kill(&sh->dp);
	atomic_store(&sh->state, CLOSED);
	pthread_cond_broadcast(&sh->cond);
	pthread_mutex_unlock(&sh->mutex);

	pthread_exit(nullptr);
}

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#include <cassette/ccfg.h>
#include <cassette/cgui.h>
#include <cassette/cobj.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "cell.h"
#include "event.h"
#include "shell.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define GUARD(CL, ...) if (!CL || cerr_critical(CL->err)) { return __VA_OPT__(__VA_ARGS__); }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct call
{
	void (*fn)(ccell *, void *, struct cevent);
	void *data;
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct ccell
{
	/* state */

	enum cerr err;
	bool damaged;

	/* geometry */

	uint32_t x;
	uint32_t y;
	uint32_t w;
	uint32_t h;

	/* propagates */

	struct call cb_event;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void ev_redraw    (ccell *, struct cevent);
static void ev_transform (ccell *, struct cevent);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void propagate (ccell *, struct cevent);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
ccell_clear_warnings(ccell *cl)
{
	GUARD(cl);

	cerr_clear_warnings(&cl->err);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

ccell *
ccell_create(void (*fn)(ccell *, void *, struct cevent), void *data)
{
	ccell *cl;

	if (!fn || !(cl = malloc(sizeof(ccell))))
	{
		return nullptr;
	}

	cl->cb_event = (struct call){.fn = fn, .data = data};
	cl->err      = CERR_NONE;
	cl->damaged  = false;
	cl->x        = 0;
	cl->y        = 0;
	cl->w        = 0;
	cl->h        = 0;

	return cl;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ccell_damage(ccell *cl)
{
	GUARD(cl);

	cl->damaged = true;

	shell_damage();
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void *
ccell_data(const ccell *cl)
{
	GUARD(cl, nullptr);

	return cl->cb_event.data;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

nullptr_t
ccell_destroy(ccell *cl)
{
	free(cl);

	return nullptr;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cerr
ccell_error(const ccell *cl)
{
	return cl ? cl->err : CERR_INVALID;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

uint32_t
ccell_h(const ccell *cl)
{
	GUARD(cl, 0);

	return cl->h;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

uint32_t
ccell_w(const ccell *cl)
{
	GUARD(cl, 0);

	return cl->w;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

uint32_t
ccell_x(const ccell *cl)
{
	GUARD(cl, 0);

	return cl->x;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

uint32_t
ccell_y(const ccell *cl)
{
	GUARD(cl, 0);

	return cl->y;
}

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

void
cell_send_event(ccell *cl, struct cevent ev)
{
	event_print(ev, "cell");
	switch (ev.type)
	{
		case CEVENT_TRANSFORM:
			ev_transform(cl, ev);
			break;

		case CEVENT_REDRAW:
			ev_redraw(cl, ev);
			break;

		default:
			propagate(cl, ev);
			break;
	}
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
propagate(ccell *cl, struct cevent ev)
{
	cl->cb_event.fn(cl, cl->cb_event.data, ev);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_redraw(ccell *cl, struct cevent ev)
{
	if (cl->damaged)
	{
		cl->damaged = false;
		propagate(cl, ev);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_transform(ccell *cl, struct cevent ev)
{
	cl->x = ev.transform_x;
	cl->y = ev.transform_y;
	cl->w = ev.transform_w;
	cl->h = ev.transform_h;

	cl->damaged = true;
	propagate(cl, ev);
}

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#include <cassette/ccfg.h>
#include <cassette/cgui.h>
#include <cassette/cobj.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "cell.h"
#include "event.h"

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

	/* callbacks */

	struct call cl_destroy;
	struct call cl_event;

	/* geometry */

	uint32_t x;
	uint32_t y;
	uint32_t w;
	uint32_t h;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void dummy (ccell *, void *, struct cevent);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
ccell_assign(ccell *cl, cgrid *gr, int layer, size_t x, size_t y, size_t w, size_t h)
{
	GUARD(cl);

	(void)layer;
	(void)gr;
	(void)x;
	(void)y;
	(void)w;
	(void)h;

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ccell_clear_warnings(ccell *cl)
{
	GUARD(cl);

	cerr_clear_warnings(&cl->err);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

ccell *
ccell_create(void)
{
	ccell *cl;

	if (!(cl = malloc(sizeof(ccell))))
	{
		return nullptr;
	}

	cl->cl_destroy = (struct call){.fn = dummy, .data = nullptr};
	cl->cl_event   = (struct call){.fn = dummy, .data = nullptr};
	cl->err        = CERR_NONE;
	cl->damaged    = false;
	cl->x          = 0;
	cl->y          = 0;
	cl->w          = 0;
	cl->h          = 0;

	return cl;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ccell_damage(ccell *cl)
{
	GUARD(cl);

	cl->damaged = true;

	// TODO signal shell
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

nullptr_t
ccell_destroy(ccell *cl)
{
	if (cl)
	{
		free(cl);
	}

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

void
ccell_on_event(ccell *cl, void (*fn)(ccell *, void *, struct cevent), void *data)
{
	GUARD(cl);

	cl->cl_event.fn   = fn ? fn : dummy;
	cl->cl_event.data = data;
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
	(void)cl;

	event_print(ev, "cell");
	switch (ev.type)
	{
		case CEVENT_TRANSFORM:
			cl->x = ev.transform_x;
			cl->y = ev.transform_y;
			cl->w = ev.transform_w;
			cl->h = ev.transform_h;
			cl->damaged = true;
			break;

		case CEVENT_REDRAW:
			cl->damaged = false; // TODO
			break;

		default:
			break;
	}

	cl->cl_event.fn(cl, cl->cl_event.data, ev);
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

/************************************************************************************************************/
/* STATIC - NOOP ********************************************************************************************/
/************************************************************************************************************/

static void
dummy(ccell *cl, void *data, struct cevent ev)
{
	(void)data;
	(void)cl;
	(void)ev;
}

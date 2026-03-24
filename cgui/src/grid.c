/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#include <cassette/ccfg.h>
#include <cassette/cgui.h>
#include <cassette/cobj.h>
#include <float.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "cell.h"
#include "event.h"
#include "grid.h"
#include "shell.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define GUARD(GR, ...)     if (!GR || cerr_critical(GR->err)) { return __VA_OPT__(__VA_ARGS__); }
#define GUARD_COL(GR, COL) if (COL >= gr->cols_n) { cerr_set(&gr->err, CERR_PARAM); return; }
#define GUARD_ROW(GR, ROW) if (ROW >= gr->rows_n) { cerr_set(&gr->err, CERR_PARAM); return; }
#define GUARD_LOCK(GR)     if (GR->locked)        { cerr_set(&gr->err, CERR_CALL ); return; }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct line
{
	int32_t size;
	double factor;
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct zone
{
	ccell *cell;
	uint32_t x;
	uint32_t y;
	uint32_t w;
	uint32_t h;
	int layer;
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct cgrid
{
	/* state */

	enum cerr err;
	bool damaged;
	bool locked;
	int  layer;

	/* contents */

	cref *zones;
	struct line *cols;
	struct line *rows;
	size_t rows_n;
	size_t cols_n;

	/* config */

	uint32_t gutter;
	uint32_t gap;
	uint32_t pad;
	uint32_t font_w; // TODO actual sampling
	uint32_t font_h; // TODO actual sampling
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void ev_close     (cgrid *, struct cevent);
static void ev_conf      (cgrid *, struct cevent);
static void ev_redraw    (cgrid *, struct cevent);
static void ev_transform (cgrid *, struct cevent);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static uint32_t fetch     (ccfg  *, uint32_t, const char *);
static void     propagate (cgrid *, struct cevent);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cgrid_assign_cell(cgrid *gr, ccell *cl, int layer, uint32_t x, uint32_t y, uint32_t w, uint32_t h)
{
	struct zone *zn;

	GUARD(gr);
	GUARD_LOCK(gr);
	GUARD_COL(gr, x);
	GUARD_ROW(gr, y);

	CREF_FOR_EACH(gr->zones, struct zone, tmp, i)
	{
		if (tmp->cell == cl && tmp->layer == layer)
		{
			goto fail_check;
		}
	}

	if (w == 0 || w > gr->rows_n - x
	 || h == 0 || h > gr->rows_n - y)
	{
		goto fail_param;
	}

	if (!(zn = malloc(sizeof(struct zone))))
	{
		goto fail_alloc;
	}

	cref_push(gr->zones, zn);
	if (cref_error(gr->zones))
	{
		goto fail_push;
	}

	zn->layer = layer;
	zn->cell  = cl;
	zn->x     = x;
	zn->y     = y;
	zn->w     = w;
	zn->h     = h;

	return;

	/* errors */

fail_push:
	cerr_set(&gr->err, cref_error(gr->zones));
	free(zn);
fail_alloc:
	cerr_set(&gr->err, CERR_MEMORY);
fail_param:
fail_check:
	cerr_set(&gr->err, CERR_PARAM);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgrid_clear_warnings(cgrid *gr)
{
	GUARD(gr);

	cerr_clear_warnings(&gr->err);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgrid *
cgrid_create(size_t rows, size_t cols)
{
	cgrid *gr;

	if (!(gr = malloc(sizeof(cgrid))))
	{
		goto fail_alloc;
	}

	if (!(gr->rows = calloc(rows, sizeof(struct line))))
	{
		goto fail_rows;
	}

	if (!(gr->cols = calloc(cols, sizeof(struct line))))
	{
		goto fail_cols;
	}

	if (!(gr->zones = cref_create()))
	{
		goto fail_zones;
	}

	if (rows == 0 || cols == 0)
	{
		goto fail_param;
	}

	gr->err     = CERR_NONE;
	gr->damaged = false;
	gr->locked  = false;
	gr->rows_n  = rows;
	gr->cols_n  = cols;
	gr->layer   = 0;
	gr->gutter  = 0;
	gr->gap     = 0;
	gr->pad     = 0;
	gr->font_w  = 0;
	gr->font_h  = 0;

	return gr;

	/* errors */

fail_param:
	cref_destroy(gr->zones);
fail_zones:
	free(gr->cols);
fail_cols:
	free(gr->rows);
fail_rows:
	free(gr);
fail_alloc:
	return nullptr;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

nullptr_t
cgrid_destroy(cgrid *gr)
{
	if (gr && !gr->locked)
	{
		cref_destroy(gr->zones);
		free(gr->cols);
		free(gr->rows);
		free(gr);
	}

	return nullptr;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cerr
cgrid_error(const cgrid *gr)
{
	return gr ? gr->err : CERR_INVALID;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgrid_flex_col(cgrid *gr, size_t col, double factor)
{
	GUARD(gr);
	GUARD_LOCK(gr);
	GUARD_COL(gr, col);

	if (factor < DBL_EPSILON)
	{
		cerr_set(&gr->err, CERR_PARAM);
	}
	else
	{
		gr->cols[col].factor = factor;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgrid_flex_row(cgrid *gr, size_t row, double factor)
{
	GUARD(gr);
	GUARD_LOCK(gr);
	GUARD_ROW(gr, row);

	if (factor < DBL_EPSILON)
	{
		cerr_set(&gr->err, CERR_PARAM);
	}
	else
	{
		gr->rows[row].factor = factor;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgrid_locked(const cgrid *gr)
{
	GUARD(gr, true);

	return gr->locked;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgrid_resize_col(cgrid *gr, size_t col, int32_t size)
{
	GUARD(gr);
	GUARD_LOCK(gr);
	GUARD_COL(gr, col);

	gr->cols[col].size = size;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgrid_resize_row(cgrid *gr, size_t row, int32_t size)
{
	GUARD(gr);
	GUARD_LOCK(gr);
	GUARD_ROW(gr, row);

	gr->rows[row].size = size;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgrid_show_layer(cgrid *gr, int layer)
{
	GUARD(gr);

	gr->layer   = layer;
	gr->damaged = true;
}

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

uint32_t
grid_h(cgrid *gr)
{
	uint32_t h = gr->gap * (gr->rows_n - 1);
	int32_t  r;

	for (size_t i = 0; i < gr->rows_n; i++)
	{
		r  = gr->rows[i].size;
		h += r == 0 ? gr->gutter : ((r > 0 ? gr->font_h : -gr->font_w) * r);
	}

	return h;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
grid_send_event(cgrid *gr, struct cevent ev)
{
	event_print(ev, "grid");
	switch (ev.type)
	{
		case CEVENT_TRANSFORM:
			ev_transform(gr, ev);
			break;

		case CEVENT_REDRAW:
			ev_redraw(gr, ev);
			break;

		case CEVENT_CLOSE:
			ev_close(gr, ev);
			break;

		case CEVENT_CONFIG:
			ev_conf(gr, ev);
			break;

		default:
			break;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

uint32_t
grid_w(cgrid *gr)
{
	uint32_t w = gr->gap * (gr->cols_n - 1);
	int32_t  c;

	for (size_t i = 0; i < gr->cols_n; i++)
	{
		c  = gr->cols[i].size;
		w += c == 0 ? gr->gutter : ((c > 0 ? gr->font_w : -gr->font_h) * c);
	}

	return w;
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
ev_close(cgrid *gr, struct cevent ev)
{
	gr->locked = false;

	propagate(gr, ev);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_conf(cgrid *gr, struct cevent ev)
{
	gr->locked = true;

	gr->font_w = fetch(ev.config, 4, "font_w");
	gr->font_h = fetch(ev.config, 8, "font_h");
	gr->gutter = fetch(ev.config, 5, "gutter");
	gr->gap    = fetch(ev.config, 5, "gap");
	gr->pad    = fetch(ev.config, 5, "pad");

	propagate(gr, ev);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_redraw(cgrid *gr, struct cevent ev)
{
	gr->damaged = false;

	propagate(gr, ev);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_transform(cgrid *gr, struct cevent ev)
{
	(void)ev;

	gr->damaged = true;

	// TODO per cell transform
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static uint32_t
fetch(ccfg *cfg, uint32_t base, const char *name)
{
	ccfg_fetch(cfg, "grid", name);

	return ccfg_iterate(cfg) ? cutil_str_to_long(ccfg_resource(cfg), 0, UINT32_MAX) : base;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
propagate(cgrid *gr, struct cevent ev)
{
	CREF_FOR_EACH(gr->zones, struct zone, zn, i)
	{
		if (gr->layer == zn->layer)
		{
			cell_send_event(zn->cell, ev);
		}
	}
}

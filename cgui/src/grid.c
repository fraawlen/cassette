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
#include <stdio.h>
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
	/* spec */

	int32_t size;
	double  flex;

	/* cache */

	uint32_t offset_1;
	uint32_t offset_2;
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
static void ev_transform (cgrid *, struct cevent);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void     cache_axis (cgrid *, uint32_t, uint32_t, struct line *, size_t, int);
static uint32_t fetch      (ccfg  *, uint32_t, const char  *);
static uint32_t line_len   (cgrid *, struct line *, int);
static void     propagate  (cgrid *, struct cevent, bool);

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

	if (cerr_critical(ccell_error(cl))
	 || w == 0 || w > gr->cols_n - x
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
cgrid_create(size_t cols, size_t rows)
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
		gr->cols[col].flex = factor;
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
		gr->rows[row].flex = factor;
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

	gr->layer = layer;
	CREF_FOR_EACH(gr->zones, struct zone, zn, i)
	{
		if (zn->layer == layer)
		{
			ccell_damage(zn->cell);
		}
	}
}

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

uint32_t
grid_h(cgrid *gr)
{
	uint32_t h = 0;

	for (size_t i = 0; i < gr->rows_n; i++)
	{
		h += line_len(gr, gr->rows + i, -1);
		h += gr->gap;
	}

	return h - gr->gap;
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

		case CEVENT_CLOSE:
			ev_close(gr, ev);
			break;

		case CEVENT_CONFIG:
			ev_conf(gr, ev);
			break;

		case CEVENT_REDRAW:
			propagate(gr, ev, false);
			break;

		case CEVENT_OPEN:
			propagate(gr, ev, true);
			break;

		default:
			break;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

uint32_t
grid_w(cgrid *gr)
{
	uint32_t w = 0;

	for (size_t i = 0; i < gr->cols_n; i++)
	{
		w += line_len(gr, gr->cols + i, 1);
		w += gr->gap;
	}

	return w - gr->gap;
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
cache_axis(cgrid *gr, uint32_t o, uint32_t l, struct line *ln, size_t n, int axis)
{
	double   f = 0.0;
	uint32_t a;

	for (size_t i = 0; i < n; i++)
	{
		f += ln[i].flex;
	}

	if (f < DBL_EPSILON)
	{
		o += l / 2;
	}

	for (size_t i = 0; i < n; i++)
	{
		a = f < DBL_EPSILON ? 0 : l * ln[i].flex / f;

		ln[i].offset_1 = o;
		ln[i].offset_2 = o + a + line_len(gr, ln + i, axis);

		o  = ln[i].offset_2 + gr->gap;
		f -= ln[i].flex;
		l -= a;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_close(cgrid *gr, struct cevent ev)
{
	gr->locked = false;

	propagate(gr, ev, true);
	cref_clear(gr->zones);
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

	propagate(gr, ev, true);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_transform(cgrid *gr, struct cevent ev)
{
	cache_axis(gr, ev.transform_x, ev.transform_w - grid_w(gr), gr->cols, gr->cols_n,  1);
	cache_axis(gr, ev.transform_y, ev.transform_h - grid_h(gr), gr->rows, gr->rows_n, -1);

	CREF_FOR_EACH(gr->zones, struct zone, zn, i)
	{
		if (gr->layer == zn->layer)
		{
			ev.transform_x = gr->cols[zn->x].offset_1;
			ev.transform_y = gr->rows[zn->y].offset_1;
			ev.transform_w = gr->cols[zn->x + zn->w - 1].offset_2 - ev.transform_x;
			ev.transform_h = gr->rows[zn->y + zn->h - 1].offset_2 - ev.transform_y;

			cell_send_event(zn->cell, ev);
		}	
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static uint32_t
fetch(ccfg *cfg, uint32_t base, const char *name)
{
	ccfg_fetch(cfg, "grid", name);

	return ccfg_iterate(cfg) ? cutil_str_to_long(ccfg_resource(cfg), 0, UINT32_MAX) : base;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static uint32_t
line_len(cgrid *gr, struct line *ln, int axis)
{
	uint32_t a = axis > 0 ? gr->font_w : gr->font_h;
	uint32_t b = axis > 0 ? gr->font_h : gr->font_w;
	
	return 2 * gr->pad + (ln->size == 0 ? gr->gutter : ((ln->size > 0 ? a : -b) * ln->size));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
propagate(cgrid *gr, struct cevent ev, bool all_layers)
{
	CREF_FOR_EACH(gr->zones, struct zone, zn, i)
	{
		if (all_layers || gr->layer == zn->layer)
		{
			cell_send_event(zn->cell, ev);
		}
	}
}
